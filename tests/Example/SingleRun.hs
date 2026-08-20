{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}

{- | Instrumenting ONE run with waveform capture: the unit-test shapes.

Read it top to bottom; each test is one thing you would actually do. The
design is instrumented by two annotations alone — @OPAQUE@ makes a function a
hierarchy level, 'HasCircuitContext' opts it in — and generates the same
hardware; everything else here is the test-side lifecycle from
"Clash.CircuitContext.Waveform".

"Test.ExampleOutput" later decodes what these examples wrote, so being
examples does not exempt them from being checked.
-}
module Example.SingleRun (
  tests,
  singleRunVcd,
) where

import qualified Prelude as P

import Control.Exception (SomeException, try)
import System.Directory (doesFileExist)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.CircuitContext.Waveform

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
-- 2. The tests
--------------------------------------------------------------------------------

-- | Where the always-capture example writes; "Test.ExampleOutput" reads it.
singleRunVcd :: FilePath
singleRunVcd = waveformPath "example-single-run"

tests :: TestTree
tests =
  testGroup
    "Example.SingleRun"
    [ -- 'withWaveformSlot' brackets the test with its slot and writes
      -- whatever was captured -- even if the test throws, so a failing test
      -- still leaves the waveform that shows why.
      testCase "a bracketed test always leaves its waveform" $ do
        out <- withWaveformSlot "example-single-run" $ \wf ->
          withWaveform wf 32 (simulate' 32)
        P.length out @?= 32
        wrote <- doesFileExist singleRunVcd
        assertBool "the waveform was written when the bracket closed" wrote
    , -- Capture only what will be kept. This is what makes a big suite
      -- affordable: a run nobody keeps never enters a recording context.
      -- Nothing is recorded, rendered or written while the test passes.
      testCase "on-failure capture costs a passing test nothing" $ do
        passing <- newWaveformSlot "example-on-failure-passing"
        withWaveformOnFailure passing 32 (simulate' 32) $ \out ->
          P.length out @?= 32
        wrote <- doesFileExist (waveformSlotPath passing)
        assertBool "a passing test writes no waveform at all" (P.not wrote)
    , -- If the consumer throws, the simulation is re-run WITH recording and
      -- that run's waveform is written before the original exception is
      -- rethrown. Because it re-runs, the consumer must be re-runnable:
      -- assertions and forcing, not one-shot IO. Where a re-run may not
      -- reproduce (a CPU model that resolves undefined inputs randomly), use
      -- withWaveformOnFailure' instead, which records as it goes and still
      -- renders only on failure.
      testCase "a failing test leaves the waveform that shows why" $ do
        failing <- newWaveformSlot "example-on-failure-failing"
        outcome <-
          try @SomeException $
            withWaveformOnFailure failing 32 (simulate' 32) $ \out ->
              P.length out @?= 99 -- fails on purpose
        assertBool
          "the original failure still propagates"
          (P.either (P.const True) (P.const False) outcome)
        wrote <- doesFileExist (waveformSlotPath failing)
        assertBool "the failing run's waveform was written" wrote
    ]
