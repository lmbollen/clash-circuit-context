{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{- | The capture contract: a waveform costs nothing unless it is kept, and
the one that is kept is the run that matters.

Under a parallel test runner peak memory is the sum over concurrently running
tests, so recording work that is thrown away is the dominant cost. These tests
pin the behaviour that makes it avoidable. Each one asserts on the FILES,
because that is the observable a developer actually depends on.

Every test uses its own waveform name and scrubs its files first, so they are
independent and free to run in parallel.
-}
module Test.Capture (tests) where

import qualified Prelude as P

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((-<.>), (<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.CircuitContext.Waveform
import Clash.CircuitContext.Waveform.Hedgehog (Capture (..), simulateWaveform)

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Config (UseColor (DisableColor))
import Hedgehog.Internal.Property (Property (..), footnote)
import qualified Hedgehog.Internal.Report as Report
import Hedgehog.Internal.Runner (checkReport)
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Range as Range

-- | A trivial traced design; the payload type is irrelevant to what we assert.
counter :: (HasCircuitContext) => Signal System (Unsigned 8)
counter = traceSignalC "count" (fromList [0 ..])
{-# OPAQUE counter #-}

{- | Two traced signals identical in every COMMITTED cycle and diverging only
at the last forced one — which the recorder has not committed yet (it sits in
the packed tail; see 'teForced') when the run stops. Sampled 4 cycles, both
commit @[0, 0, 0]@ and the divergent cycle 3 is drained at dump time.
-}
twins :: (HasCircuitContext) => Signal System (Unsigned 8, Unsigned 8)
twins =
  bundle
    ( traceSignalC "same" (fromList [0, 0, 0, 0, 0])
    , traceSignalC "diff" (fromList [0, 0, 0, 1, 0])
    )

vcdOf :: String -> FilePath
vcdOf name = "waveforms" </> name <.> "vcd"

-- | Leave the waveform of a failing run behind, as an earlier run would.
plantFailure :: String -> IO ()
plantFailure name = do
  _ <- try @SomeException $ withWaveform name 8 counter $ \xs -> evaluate (P.length xs) >> errorX "an earlier failure"
  (v, j) <- filesFor name
  assertBool "the earlier failure left its waveform" (v P.&& j)

-- | Do the files of a name exist? (VCD and its ADT sidecar.)
filesFor :: String -> IO (Bool, Bool)
filesFor name = (,) <$> doesFileExist (vcdOf name) <*> doesFileExist (vcdOf name -<.> "json")

-- | A test with its own, scrubbed waveform name.
withName :: String -> (String -> Assertion) -> Assertion
withName name body = do
  let vcd = vcdOf name
  P.mapM_ (\p -> doesFileExist p >>= \e -> when e (removeFile p)) [vcd, vcd -<.> "json"]
  body name

-- | How many cycles a VCD has (one @#@ timestamp per cycle).
cyclesOf :: String -> IO Int
cyclesOf name = do
  vcd <- P.readFile (vcdOf name)
  pure (P.length [l | l <- P.lines vcd, P.take 1 l P.== "#"])

{- | A property that always fails and shrinks through several cases (any
non-empty list fails; Hedgehog shrinks it to one element). Each case leaves
a footnote that counts how often its text is evaluated.
-}
countingFootnotes :: IORef Int -> Property
countingFootnotes evaluated = Hedgehog.property $ do
  xs <- Hedgehog.forAll (Gen.list (Range.linear 1 20) Gen.bool)
  footnote (counted evaluated)
  P.length xs Hedgehog.=== 0

-- | A footnote text that counts its own evaluation.
counted :: IORef Int -> String
counted evaluated = unsafePerformIO (atomicModifyIORef' evaluated (\n -> (n + 1, "counted")))
{-# NOINLINE counted #-}

tests :: TestTree
tests =
  testGroup
    "Test.Capture"
    [ testCase "writeWaveform writes the VCD and its sidecar" $
        withName "capture-written" $ \name -> do
          out <- writeWaveform name 8 counter
          out @?= [0 .. 7]
          (v, j) <- filesFor name
          assertBool "both halves written" (v P.&& j)
    , -- A PASSING run writes nothing at all: the simulation runs without a
      -- recording context, so there is no history to render or discard.
      testCase "withWaveform writes nothing when the test passes" $
        withName "capture-passing" $ \name -> do
          _ <- withWaveform name 8 counter evaluate
          (v, j) <- filesFor name
          assertBool "no VCD, no sidecar" (P.not v P.&& P.not j)
    , -- A file left by an earlier FAILING run is deleted when the test
      -- passes: from withWaveform, a file means "this test failed".
      testCase "withWaveform deletes an earlier failure's files when the test passes" $
        withName "capture-stale" $ \name -> do
          plantFailure name
          _ <- withWaveform name 8 counter evaluate
          (v, j) <- filesFor name
          assertBool "no stale VCD, no stale sidecar" (P.not v P.&& P.not j)
    , -- …but keeps a waveform that was written on purpose, by writeWaveform.
      testCase "withWaveform keeps a waveform written on purpose when the test passes" $
        withName "capture-requested" $ \name -> do
          _ <- writeWaveform name 8 counter
          _ <- withWaveform name 8 counter evaluate
          (v, j) <- filesFor name
          assertBool "the requested waveform is still there" (v P.&& j)
    , -- A FAILING run writes both, and the check's exception propagates.
      testCase "withWaveform writes both files when the test fails" $
        withName "capture-failing" $ \name -> do
          outcome <-
            try @SomeException $
              withWaveform name 8 counter $ \xs -> do
                _ <- evaluate (P.length xs)
                errorX "deliberate failure"
          assertBool
            "the failure propagates"
            (P.either (P.const True) (P.const False) outcome)
          (v, j) <- filesFor name
          assertBool "both halves written" (v P.&& j)
    , -- A run that forces exactly ONE cycle must still capture it. Recording
      -- is one cell behind the simulation, so after one forced cell nothing
      -- is committed yet — only the tap's touched bit distinguishes this run
      -- from a signal nobody looked at, and without it the capture is
      -- refused as empty.
      testCase "a failing check that forced exactly one cycle still leaves it" $
        withName "capture-one-cycle" $ \name -> do
          _ <-
            try @SomeException $
              withWaveform name 8 counter $ \xs -> do
                _ <- evaluate (P.take 1 xs P.== [0])
                errorX "deliberate failure"
          (v, j) <- filesFor name
          assertBool "both halves written" (v P.&& j)
          n <- cyclesOf name
          n @?= 1
    , -- Signals that agree on every committed cycle but diverge at the
      -- drained last one must NOT share a VCD identifier: an alias would
      -- display the representative's value for exactly the cycle the capture
      -- is about. The divergent value 1 only exists at that last cycle, so
      -- its (full-width) change line appearing at all proves "diff" kept its
      -- own identifier.
      testCase "signals diverging only at the last (uncommitted) cycle are not aliased" $
        withName "capture-alias-tail" $ \name -> do
          _ <- writeWaveform name 4 twins
          vcd <- P.readFile (vcdOf name)
          assertBool
            "the divergent change line is present"
            (P.any ("b00000001 " `isPrefixOf`) (P.lines vcd))
    , -- The same contract inside a hedgehog property, where a failure is a
      -- VALUE in the property monad and no amount of `try` in IO can see it.
      testCase "simulateWaveform writes nothing when the property passes" $
        withName "capture-prop-passing" $ \name -> do
          ok <-
            Hedgehog.check . Hedgehog.property $ do
              xs <- simulateWaveform Counterexample name 8 counter
              P.length xs Hedgehog.=== 8
          assertBool "the passing property passes" ok
          (v, j) <- filesFor name
          assertBool "no VCD, no sidecar" (P.not v P.&& P.not j)
    , -- The counterexample's LAST cycle must be in the waveform. Two ways to
      -- lose it, both found by reading a real one: the assertion here only
      -- looks at the list's length, so nothing forces the samples unless the
      -- capture does; and the recorder commits cycle i only when cell i+1 is
      -- forced, so the final cycle is still pending in the packed tail when
      -- the simulation stops. A counterexample is usually ABOUT its last
      -- cycle, so both matter.
      testCase "simulateWaveform writes the counterexample, last cycle included" $
        withName "capture-prop-failing" $ \name -> do
          ok <-
            Hedgehog.check . Hedgehog.property $ do
              xs <- simulateWaveform Counterexample name 8 counter
              P.length xs Hedgehog.=== 7
          assertBool "the failing property fails" (P.not ok)
          (v, j) <- filesFor name
          assertBool "both halves written" (v P.&& j)
          n <- cyclesOf name
          n @?= 8
    , -- The waveform is of the case Hedgehog REPORTS: the smallest
      -- counterexample, not the first failing case. Every list of length
      -- 3 or more fails, so Hedgehog shrinks to exactly 3 elements.
      testCase "the counterexample waveform is the shrunk case" $
        withName "capture-prop-shrunk" $ \name -> do
          ok <-
            Hedgehog.check . Hedgehog.property $ do
              xs <- Hedgehog.forAll (Gen.list (Range.linear 1 30) Gen.bool)
              out <- simulateWaveform Counterexample name (P.length xs) counter
              Hedgehog.assert (P.length out P.< 3)
          assertBool "the failing property fails" (P.not ok)
          n <- cyclesOf name
          n @?= 3
    , -- The mechanism the counterexample capture relies on: Hedgehog
      -- evaluates a footnote's text only when it prints the report, and only
      -- for the case it reports. If a Hedgehog upgrade changes that, every
      -- failing shrink step would record, and this fails.
      testCase "Hedgehog evaluates the footnote of the reported case only, once" $ do
        evaluated <- newIORef 0
        ok <- Hedgehog.check (countingFootnotes evaluated)
        assertBool "the property fails" (P.not ok)
        n <- readIORef evaluated
        n @?= 1
    , -- The same through the runner tasty-hedgehog uses: checkReport, with a
      -- progress display during shrinking, then the rendered result.
      testCase "…also with progress rendered while it shrinks" $ do
        evaluated <- newIORef 0
        seed <- Seed.random
        let Property config test = countingFootnotes evaluated
        report <-
          checkReport config 0 seed test $ \progress ->
            Report.renderProgress DisableColor Nothing progress >>= evaluate . P.length >> pure ()
        before <- readIORef evaluated
        before @?= 0
        _ <- Report.renderResult DisableColor Nothing report >>= evaluate . P.length
        after <- readIORef evaluated
        after @?= 1
    , testCase "a passing property never evaluates a footnote" $ do
        evaluated <- newIORef 0
        ok <-
          Hedgehog.check . Hedgehog.property $ do
            footnote (counted evaluated)
            Hedgehog.success
        assertBool "the property passes" ok
        n <- readIORef evaluated
        n @?= 0
    , -- AtSize keeps one passing case, also for a property of fewer than 100
      -- tests, which never reaches sizes of 20 and more.
      testCase "AtSize keeps a passing case of a size the property reaches" $
        withName "capture-prop-sized" $ \name -> do
          ok <-
            Hedgehog.check . Hedgehog.withTests 20 . Hedgehog.property $ do
              xs <- simulateWaveform (AtSize 10) name 8 counter
              P.length xs Hedgehog.=== 8
          assertBool "the property passes" ok
          (v, j) <- filesFor name
          assertBool "both halves written" (v P.&& j)
    , -- …and only once per name: a second property with the same name, run
      -- in the same process, does not write it again.
      testCase "AtSize keeps one case per name, once" $
        withName "capture-prop-once" $ \name -> do
          _ <-
            Hedgehog.check . Hedgehog.withTests 5 . Hedgehog.property $
              () <$ simulateWaveform (AtSize 0) name 8 counter
          first <- filesFor name
          first @?= (True, True)
          removeFile (vcdOf name)
          removeFile (vcdOf name -<.> "json")
          _ <-
            Hedgehog.check . Hedgehog.withTests 5 . Hedgehog.property $
              () <$ simulateWaveform (AtSize 0) name 8 counter
          second <- filesFor name
          second @?= (False, False)
    ]
