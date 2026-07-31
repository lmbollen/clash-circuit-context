{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}

{- | A worked example: instrument a design, then get a VCD out of a test.

Runnable — @./check.sh@ builds and runs it, so nothing here can drift from the
API. Read it top to bottom; each section is one thing you would actually do.

@
cabal run example-waveforms
@
-}
module Main where

import Control.Exception (SomeException, try)
import Data.IORef (IORef, newIORef)
import System.Directory (doesFileExist)

-- Clash's prelude replaces some list functions with Vec ones, so the few list
-- operations here are qualified.
import Clash.Explicit.Prelude
import qualified Prelude as P

import Hedgehog (Property, forAll, property, withTests, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.CircuitContext
import Clash.CircuitContext.Waveform
import Clash.CircuitContext.Waveform.Hedgehog

--------------------------------------------------------------------------------
-- 1. The design. Two annotations instrument it: OPAQUE makes it a hierarchy
--    level, HasCircuitContext opts it in. Nothing else is tracing-specific --
--    no traceSignal, no names, and it still generates the same hardware.
--------------------------------------------------------------------------------

-- | An accumulator whose mealy step is probed, so the VCD shows the state the
-- step function computes as well as the signals around it.
acc ::
  (HasCircuitContext) =>
  Clock System ->
  Reset System ->
  Signal System (Unsigned 8) ->
  Signal System (Unsigned 8)
acc clk rst inp = total
 where
  total = mealyProbed clk rst enableGen step 0 inp
  step :: (HasProbe) => Unsigned 8 -> Unsigned 8 -> (Unsigned 8, Unsigned 8)
  step s i = (next, s)
   where
    next = s + i
{-# OPAQUE acc #-}

{- | Two instances of the same component. They are disambiguated by
instantiation call site, so they appear as @acc_0@ and @acc_1@ -- ordered by
the design, never by evaluation order.
-}
top ::
  (HasCircuitContext) =>
  Clock System ->
  Reset System ->
  Signal System (Unsigned 8) ->
  Signal System (Unsigned 8)
top clk rst inp = out
 where
  out = acc clk rst inp + acc clk rst (inp + 1)
{-# OPAQUE top #-}

-- | The stimulus, shared by every example below.
stimulus :: Signal System (Unsigned 8)
stimulus = fromList (cycle [1, 2, 3])

simulate' :: (HasCircuitContext) => Int -> [Unsigned 8]
simulate' n = sampleN n (top clockGen resetGen stimulus)

--------------------------------------------------------------------------------
-- 2. A plain unit test that always leaves a waveform.
--------------------------------------------------------------------------------

-- | 'withWaveformSlot' brackets the test with its slot and writes whatever was
-- captured -- even if the test throws, so a failing test still leaves the
-- waveform that shows why.
alwaysCapture :: IO ()
alwaysCapture = withWaveformSlot "example_always" $ \wf -> do
  out <- withWaveform wf 32 (simulate' 32)
  check "the design accumulates" (P.length out == 32)

--------------------------------------------------------------------------------
-- 3. Capture only what will be kept. This is what makes a big suite
--    affordable: a run nobody keeps never enters a recording context.
--------------------------------------------------------------------------------

{- | Nothing is recorded, rendered or written while the test passes. If the
consumer throws, the simulation is re-run WITH recording and that run's
waveform is written before the original exception is rethrown.

Because it re-runs, @consume@ must be re-runnable: assertions and forcing, not
one-shot IO. Where a re-run may not reproduce (a CPU model that resolves
undefined inputs randomly), use 'withWaveformOnFailure'' instead, which records
as it goes and still renders only on failure.
-}
onlyOnFailure :: IO ()
onlyOnFailure = do
  passing <- newWaveformSlot "example_passing"
  withWaveformOnFailure passing 32 (simulate' 32) $ \out ->
    check "passing test asserts as usual" (P.length out == 32)
  wrote <- doesFileExist (waveformSlotPath passing)
  check "a passing test writes no waveform at all" (not wrote)

  failing <- newWaveformSlot "example_failing"
  outcome <-
    try @SomeException $
      withWaveformOnFailure failing 32 (simulate' 32) $ \out ->
        check "this one fails on purpose" (P.length out == 99)
  check "the original failure still propagates" (either (const True) (const False) outcome)
  captured <- doesFileExist (waveformSlotPath failing)
  check "a failing test leaves its waveform" captured

--------------------------------------------------------------------------------
-- 4. Hedgehog. A property failure is a value in PropertyT's error layer, not a
--    thrown exception, so the IO combinators above cannot see it.
--------------------------------------------------------------------------------

{- | On failure this captures the SHRUNK counterexample: shrinking re-runs the
property on smaller inputs and each failing case overwrites the slot, so what
survives is the minimal case hedgehog reports, and its path is printed in the
failure report. On success the flag decides which case to keep.

Note both arguments are owned by the CALLER, and that matters:

* the 'IORef' must be one per test, not one per case. Made inside the property
  body it would be fresh every case, and "fire at most once" would mean
  nothing.

* the size to keep must be one this property actually reaches.
  'recordLargestCase' is @'recordCaseOfSize' fired 'maxSize'@ (99), which a
  default 100-test property hits on its last case -- but this one runs
  'withTests' 20, so it only ever reaches size 19 and 'recordLargestCase'
  would never fire at all.
-}
prop_accumulates :: IORef Bool -> WaveformSlot -> Property
prop_accumulates fired wf = withTests 20 $ property $ do
  n <- forAll (Gen.integral (Range.linear 4 32))
  keep <- recordCaseOfSize fired 15
  withWaveformCase keep wf n (simulate' n) $ \out ->
    P.length out === n

--------------------------------------------------------------------------------

check :: String -> Bool -> IO ()
check what ok
  | ok = putStrLn ("  ok: " <> what)
  | otherwise = P.ioError (userError ("FAILED: " <> what))

main :: IO ()
main = do
  putStrLn "1. always capture"
  alwaysCapture
  putStrLn "2. capture only on failure"
  onlyOnFailure
  putStrLn "3. hedgehog property"
  wf <- newWaveformSlot "example_property"
  fired <- newIORef False
  ok <- Hedgehog.check (prop_accumulates fired wf)
  check "the property passes" ok
  writeWaveformSlot wf
  putStrLn ""
  putStrLn "wrote:"
  mapM_
    (\n -> doesFileExist (waveformPath n) >>= \e -> putStrLn ("  " <> waveformPath n <> if e then "" else "  (absent, as intended)"))
    ["example_always", "example_passing", "example_failing", "example_property"]
