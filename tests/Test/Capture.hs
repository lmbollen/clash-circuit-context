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

Every test owns its slot and scrubs its files first, so they are independent
and free to run in parallel.
-}
module Test.Capture (tests) where

import qualified Prelude as P

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (when)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((-<.>))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

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

{- | Two traced signals identical in every COMMITTED cycle and diverging only
at the last forced one — which the recorder has not committed yet (it sits in
the packed tail; see 'teForced') when the run stops. Sampled 4 cycles, both
commit @[0, 0, 0]@ and the divergent cycle 3 is drained at dump time.
-}
twins :: (HasCircuitContext) => ([Unsigned 8], [Unsigned 8])
twins =
  ( sampleN 4 (traceSignalC "same" (fromList [0, 0, 0, 0, 0] :: Signal System (Unsigned 8)))
  , sampleN 4 (traceSignalC "diff" (fromList [0, 0, 0, 1, 0] :: Signal System (Unsigned 8)))
  )

-- | Do the slot's files exist? (VCD and its ADT sidecar.)
filesFor :: WaveformSlot -> IO (Bool, Bool)
filesFor slot = do
  let vcd = waveformSlotPath slot
  (,) <$> doesFileExist vcd <*> doesFileExist (vcd -<.> "json")

scrub :: WaveformSlot -> IO ()
scrub slot = do
  let vcd = waveformSlotPath slot
  P.mapM_ (\p -> doesFileExist p >>= \e -> when e (removeFile p)) [vcd, vcd -<.> "json"]

-- | A fresh, scrubbed slot for one test.
withSlot :: String -> (WaveformSlot -> Assertion) -> Assertion
withSlot name body = do
  slot <- newWaveformSlot name
  scrub slot
  body slot

tests :: TestTree
tests =
  testGroup
    "Test.Capture"
    [ testCase "withWaveformWhen False writes nothing" $
        withSlot "capture-skipped" $ \slot -> do
          _ <- withWaveformWhen False slot 8 (sampleN 8 counter)
          (v, j) <- filesFor slot
          assertBool "no VCD, no sidecar" (P.not v P.&& P.not j)
    , testCase "withWaveformWhen True writes the VCD and its sidecar" $
        withSlot "capture-kept" $ \slot -> do
          _ <- withWaveformWhen True slot 8 (sampleN 8 counter)
          writeWaveformSlot slot
          (v, j) <- filesFor slot
          assertBool "both halves written" (v P.&& j)
    , -- A PASSING run under on-failure capture writes nothing at all: the
      -- simulation runs without a recording context, so there is no history
      -- to render and nothing to discard.
      testCase "withWaveformOnFailure writes nothing when the test passes" $
        withSlot "capture-passing" $ \slot -> do
          _ <- withWaveformOnFailure slot 8 (sampleN 8 counter) evaluate
          (v, j) <- filesFor slot
          assertBool "no VCD, no sidecar" (P.not v P.&& P.not j)
    , -- A FAILING run writes both, and the original exception propagates.
      testCase "withWaveformOnFailure writes both files when the test fails" $
        withSlot "capture-failing" $ \slot -> do
          outcome <-
            try @SomeException $
              withWaveformOnFailure slot 8 (sampleN 8 counter) $ \xs -> do
                _ <- evaluate (P.length xs)
                errorX "deliberate failure"
          assertBool
            "the failure propagates"
            (P.either (P.const True) (P.const False) outcome)
          (v, j) <- filesFor slot
          assertBool "both halves written" (v P.&& j)
    , -- The always-recording variant behaves the same from the outside.
      testCase "withWaveformOnFailure' writes both files when the test fails" $
        withSlot "capture-failing-strict" $ \slot -> do
          _ <-
            try @SomeException $
              withWaveformOnFailure' slot 8 (sampleN 8 counter) $ \xs -> do
                _ <- evaluate (P.length xs)
                errorX "deliberate failure"
          (v, j) <- filesFor slot
          assertBool "both halves written" (v P.&& j)
    , -- The same contract inside a hedgehog property, where a failure is a
      -- VALUE in the property monad and no amount of `try` in IO can see it.
      testCase "withWaveformOnCounterexample writes nothing when the property passes" $
        withSlot "capture-prop-passing" $ \slot -> do
          ok <-
            Hedgehog.check . Hedgehog.property $
              withWaveformOnCounterexample slot 8 (sampleN 8 counter) $ \xs ->
                P.length xs Hedgehog.=== 8
          assertBool "the passing property passes" ok
          (v, j) <- filesFor slot
          assertBool "no VCD, no sidecar" (P.not v P.&& P.not j)
    , testCase "withWaveformOnCounterexample writes both files for a counterexample" $
        withSlot "capture-prop-failing" $ \slot -> do
          ok <-
            Hedgehog.check . Hedgehog.property $
              withWaveformOnCounterexample slot 8 (sampleN 8 counter) $ \xs ->
                P.length xs Hedgehog.=== 7
          assertBool "the failing property fails" (P.not ok)
          (v, j) <- filesFor slot
          assertBool "both halves written" (v P.&& j)
    , -- The counterexample's LAST cycle must be in the waveform. Two ways to
      -- lose it, both found by reading a real one: the assertion here only
      -- looks at the list's length, so nothing forces the samples unless the
      -- capture does; and the recorder commits cycle i only when cell i+1 is
      -- forced, so the final cycle is still pending in the packed tail when
      -- the simulation stops. A counterexample is usually ABOUT its last
      -- cycle, so both matter.
      testCase "a counterexample keeps its last cycle" $
        withSlot "capture-prop-lastcycle" $ \slot -> do
          _ <-
            Hedgehog.check . Hedgehog.withTests 1 . Hedgehog.property $
              withWaveformOnCounterexample slot 8 (sampleN 8 counter) $ \xs ->
                P.length xs Hedgehog.=== 7
          vcd <- P.readFile (waveformSlotPath slot)
          let stamps = [l | l <- P.lines vcd, P.take 1 l P.== "#"]
          P.length stamps @?= 8
          P.last stamps @?= "#7"
    , -- And a passing case can be kept deliberately (the artifact path).
      testCase "withWaveformCase True writes both files for a passing case" $
        withSlot "capture-prop-kept" $ \slot -> do
          ok <-
            Hedgehog.check . Hedgehog.withTests 1 . Hedgehog.property $
              withWaveformCase True slot 8 (sampleN 8 counter) $ \xs ->
                P.length xs Hedgehog.=== 8
          assertBool "the kept property passes" ok
          (v, j) <- filesFor slot
          assertBool "both halves written" (v P.&& j)
    , -- A run that forces exactly ONE cycle must still capture it. Recording
      -- is one cell behind the simulation, so after one forced cell nothing
      -- is committed yet — only the tap's touched bit distinguishes this run
      -- from a signal nobody looked at, and without it the capture is
      -- refused as empty.
      testCase "a run that forced exactly one cycle still captures it" $
        withSlot "capture-one-cycle" $ \slot -> do
          _ <- withWaveformLazy slot 8 (sampleN 1 counter) (evaluate . P.length)
          writeWaveformSlot slot
          (v, j) <- filesFor slot
          assertBool "both halves written" (v P.&& j)
    , -- Signals that agree on every committed cycle but diverge at the
      -- drained last one must NOT share a VCD identifier: an alias would
      -- display the representative's value for exactly the cycle the capture
      -- is about. The divergent value 1 only exists at that last cycle, so
      -- its (full-width) change line appearing at all proves "diff" kept its
      -- own identifier.
      testCase "signals diverging only at the last (uncommitted) cycle are not aliased" $
        withSlot "capture-alias-tail" $ \slot -> do
          _ <-
            withWaveformLazy slot 8 twins $ \(xs, ys) ->
              evaluate (P.length xs + P.length ys)
          writeWaveformSlot slot
          vcd <- P.readFile (waveformSlotPath slot)
          assertBool
            "the divergent change line is present"
            (P.any ("b00000001 " `isPrefixOf`) (P.lines vcd))
    ]
