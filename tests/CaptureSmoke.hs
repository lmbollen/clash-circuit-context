{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{- | The capture contract: a waveform costs nothing unless it is kept.

Under a parallel test runner peak memory is the sum over concurrently running
tests, so recording work that is thrown away is the dominant cost. These tests
pin the behaviour that makes it avoidable — each one asserts on the FILES,
because that is the observable a developer actually depends on.
-}
module Main where

import qualified Prelude as P

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (when)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitFailure)

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.CircuitContext.Waveform
import Clash.CircuitContext.Waveform.Hedgehog (
  withWaveformCase,
  withWaveformOnCounterexample,
 )

import qualified Hedgehog

-- | A trivial traced design; the payload type is irrelevant to what we assert.
counter :: (HasCircuitContext) => Signal System (Unsigned 8)
counter = traceSignalC "count" (fromList [0 ..])
{-# OPAQUE counter #-}

check :: String -> Bool -> IO ()
check what ok
  | ok = putStrLn ("ok: " <> what)
  | otherwise = putStrLn ("FAIL: " <> what) >> exitFailure

-- | Does the slot's file exist?
fileFor :: WaveformSlot -> IO Bool
fileFor = doesFileExist . waveformSlotPath

scrub :: WaveformSlot -> IO ()
scrub slot = do
  let vcd = waveformSlotPath slot
  doesFileExist vcd >>= \e -> when e (removeFile vcd)

main :: IO ()
main = do
  -- A run nobody keeps must not produce a file …
  skipped <- newWaveformSlot "capture-skipped"
  scrub skipped
  _ <- withWaveformWhen False skipped 8 (sampleN 8 counter)
  v0 <- fileFor skipped
  check "withWaveformWhen False writes nothing" (P.not v0)

  -- … while one that is kept produces both halves.
  kept <- newWaveformSlot "capture-kept"
  scrub kept
  _ <- withWaveformWhen True kept 8 (sampleN 8 counter)
  writeWaveformSlot kept
  v1 <- fileFor kept
  check "withWaveformWhen True writes the VCD" v1

  -- A PASSING run under on-failure capture writes nothing at all: the
  -- simulation runs without a recording context, so there is no history to
  -- render and nothing to discard.
  passing <- newWaveformSlot "capture-passing"
  scrub passing
  _ <- withWaveformOnFailure passing 8 (sampleN 8 counter) evaluate
  v2 <- fileFor passing
  check "withWaveformOnFailure writes nothing when the test passes" (P.not v2)

  -- A FAILING run writes both, and the original exception still propagates.
  failing <- newWaveformSlot "capture-failing"
  scrub failing
  outcome <-
    try @SomeException $
      withWaveformOnFailure failing 8 (sampleN 8 counter) $ \xs -> do
        _ <- evaluate (P.length xs)
        errorX "deliberate failure"
  check "the failure propagates" (P.either (P.const True) (P.const False) outcome)
  v3 <- fileFor failing
  check "withWaveformOnFailure writes the VCD when the test fails" v3

  -- The always-recording variant behaves the same way from the outside.
  failing' <- newWaveformSlot "capture-failing-strict"
  scrub failing'
  _ <-
    try @SomeException $
      withWaveformOnFailure' failing' 8 (sampleN 8 counter) $ \xs -> do
        _ <- evaluate (P.length xs)
        errorX "deliberate failure"
  v4 <- fileFor failing'
  check "withWaveformOnFailure' writes the VCD when the test fails" v4

  -- The same contract inside a hedgehog property, where a failure is a VALUE
  -- in the property monad and no amount of `try` in IO can see it.
  hPassing <- newWaveformSlot "capture-prop-passing"
  scrub hPassing
  okP <-
    Hedgehog.check . Hedgehog.property $
      withWaveformOnCounterexample hPassing 8 (sampleN 8 counter) $ \xs ->
        P.length xs Hedgehog.=== 8
  check "the passing property passes" okP
  v5 <- fileFor hPassing
  check "withWaveformOnCounterexample writes nothing when the property passes" (P.not v5)

  -- A failing property still fails, and leaves the counterexample's waveform.
  hFailing <- newWaveformSlot "capture-prop-failing"
  scrub hFailing
  okF <-
    Hedgehog.check . Hedgehog.property $
      withWaveformOnCounterexample hFailing 8 (sampleN 8 counter) $ \xs ->
        P.length xs Hedgehog.=== 7
  check "the failing property fails" (P.not okF)
  v6 <- fileFor hFailing
  check "withWaveformOnCounterexample writes the VCD for a counterexample" v6

  -- The counterexample's LAST cycle must be in the waveform. Two ways to lose
  -- it, both found by reading a real one: the assertion here only looks at the
  -- list's length, so nothing forces the samples unless the capture does; and
  -- the recorder commits cycle i only when cell i+1 is forced, so the final
  -- cycle is still pending in the packed tail when the simulation stops. A
  -- counterexample is usually ABOUT its last cycle, so both matter.
  hLast <- newWaveformSlot "capture-prop-lastcycle"
  scrub hLast
  _ <-
    Hedgehog.check . Hedgehog.withTests 1 . Hedgehog.property $
      withWaveformOnCounterexample hLast 8 (sampleN 8 counter) $ \xs ->
        P.length xs Hedgehog.=== 7
  vcd <- P.readFile (waveformSlotPath hLast)
  let stamps = [l | l <- P.lines vcd, P.take 1 l P.== "#"]
  check
    ("all 8 cycles are captured, not 7 (got " <> show (P.length stamps) <> ")")
    (P.length stamps P.== 8 P.&& P.last stamps P.== "#7")

  -- And a passing case can be kept deliberately (the artifact path).
  hKept <- newWaveformSlot "capture-prop-kept"
  scrub hKept
  okK <-
    Hedgehog.check . Hedgehog.withTests 1 . Hedgehog.property $
      withWaveformCase True hKept 8 (sampleN 8 counter) $ \xs ->
        P.length xs Hedgehog.=== 8
  check "the kept property passes" okK
  v7 <- fileFor hKept
  check "withWaveformCase True writes the VCD for a passing case" v7

  -- A run that forces exactly ONE cycle must still capture it. Recording is
  -- one cell behind the simulation, so after one forced cell nothing is
  -- committed yet — only the tap's touched bit distinguishes this run from a
  -- signal nobody looked at, and without it the capture is refused as empty.
  one <- newWaveformSlot "capture-one-cycle"
  scrub one
  _ <- withWaveformLazy one 8 (sampleN 1 counter) (evaluate . P.length)
  writeWaveformSlot one
  v8 <- fileFor one
  check "a run that forced exactly one cycle still captures it" v8

  P.mapM_ scrub [kept, failing, failing', hFailing, hKept, hLast, one]
  putStrLn "capture-smoke passed"
