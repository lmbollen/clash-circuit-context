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

import Control.Exception (ErrorCall, SomeException, evaluate, try)
import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map.Strict as Map
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
  makeAbsolute,
  removeFile,
  removePathForcibly,
 )
import System.Posix.Files (accessModes, fileMode, getFileStatus, intersectFileModes)
import System.FilePath ((-<.>), (<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.CircuitContext.Recording (discardRecording, writeVCD)
import Clash.CircuitContext.Waveform
import Clash.CircuitContext.Waveform.Hedgehog (Capture (..), simulateWaveform)
import Clash.Signal.Internal (Signal ((:-)))

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Config (UseColor (DisableColor))
import Hedgehog.Internal.Property (Property (..), footnote)
import qualified Hedgehog.Internal.Report as Report
import Hedgehog.Internal.Runner (checkReport)
import qualified Hedgehog.Internal.Seed as Seed
import Hedgehog.Internal.Source (Span (spanFile))
import qualified Hedgehog.Range as Range

import Test.Vcd (asInts, decodeVCD)

-- | A trivial traced design; the payload type is irrelevant to what we assert.
counter :: (CircuitContextOnly) => Signal System (Unsigned 8)
counter = traceSignalC "count" (fromList [0 ..])
{-# OPAQUE counter #-}

{- | Two traced signals identical in every cycle but the last forced one —
which the tap has not recorded yet when the run stops (it records a cycle when
the next cell is forced), so it is recorded only when the run ends. Sampled 4
cycles, both record @[0, 0, 0]@ while the run goes, and the divergent cycle 3
when it ends.
-}
twins :: (CircuitContextOnly) => Signal System (Unsigned 8, Unsigned 8)
twins =
  bundle
    ( traceSignalC "same" (fromList [0, 0, 0, 0, 0])
    , traceSignalC "diff" (fromList [0, 0, 0, 1, 0])
    )

{- | A trace that the run does not evaluate: in reset, the register does not
need the value of "next", so 2 sampled cycles force the spine of "next" but
not "step". "step" is first evaluated when the run ends and finishes the last
cycle of "next".
-}
lateStep :: (CircuitContextOnly) => Signal System (Unsigned 8)
lateStep = cnt
 where
  cnt = traceSignalC "cnt" (register clockGen resetGen enableGen 0 next)
  next = traceSignalC "next" (mux (pure True) step cnt)
  step = traceSignalC "step" (cnt + 1)
{-# OPAQUE lateStep #-}

{- | A traced pipeline of 6 one-cycle stages, @s0@ to @s5@, after a traced
source @src@. Stage /i/ starts at /i/, so in cycle 1 it shows /i + 1/. A
stage is a cons cell, not a Clash primitive, so how far a run reaches does
not depend on how lazy 'register' is.

A run of 2 cycles of @s0@ reaches @s1@ only. Finishing @s1@ evaluates cycle 0
of @s2@, which registers then. The source sets the flag when the simulation
evaluates it.
-}
pipeline :: (CircuitContextOnly) => IORef Bool -> Signal System (Unsigned 8)
pipeline reached = stage 0
 where
  stage :: (CircuitContextOnly) => Int -> Signal System (Unsigned 8)
  stage 6 =
    traceSignalC "src" (unsafePerformIO (writeIORef reached True >> pure (fromList [6 ..])))
  stage i = traceSignalC ("s" <> P.show i) (fromIntegral i :- stage (i P.+ 1))
{-# OPAQUE pipeline #-}

{- | A pipeline without a last stage: each stage traces itself and the stage
after it. A run reaches a stage per cycle, but finishing a stage up to the
end of the run reaches the stages after it, without end.
-}
endless :: (CircuitContextOnly) => Unsigned 8 -> Signal System (Unsigned 8)
endless n = traceSignalC "stim" (n :- endless (n P.+ 1))
{-# OPAQUE endless #-}

-- | A design that must not run: sampling it throws.
ran :: Signal System (Unsigned 8)
ran = P.error "the design ran"

{- | The options of the tests that read a waveform back as text. The FST and
spill tests below use their own.
-}
vcdOptions :: WaveformOptions
vcdOptions = defaultOptions{format = VCD}

vcdOf :: FilePath -> FilePath
vcdOf path = path <.> "vcd"

-- | Leave the waveform of a failing run behind, as an earlier run would.
plantFailure :: FilePath -> IO ()
plantFailure name = do
  _ <- try @SomeException $ withWaveform vcdOptions name (0, 8) counter $ \xs -> evaluate (P.length xs) >> errorX "an earlier failure"
  (v, j) <- filesFor name
  assertBool "the earlier failure left its waveform" (v P.&& j)

-- | Do the files of a waveform's path exist? (VCD and its ADT sidecar.)
filesFor :: FilePath -> IO (Bool, Bool)
filesFor name = (,) <$> doesFileExist (vcdOf name) <*> doesFileExist (vcdOf name -<.> "json")

{- | A test with its own, scrubbed waveform name: its body gets the
waveform's path, @waveforms\/\<name\>@. Afterwards, the recording's spill
file must be gone, whatever the test did: written, failed or passed.
-}
withName :: String -> (FilePath -> Assertion) -> Assertion
withName name body = do
  let path = "waveforms" </> name
      vcd = vcdOf path
  P.mapM_ (\p -> doesFileExist p >>= \e -> when e (removeFile p)) [vcd, vcd -<.> "json"]
  body path
  spilled <- doesFileExist (vcd -<.> "spill")
  assertBool "the spill file is deleted" (P.not spilled)

{- | How many cycles a VCD has: one @#@ timestamp per cycle, and the last one
is the end of the last cycle.
-}
cyclesOf :: FilePath -> IO Int
cyclesOf name = do
  vcd <- P.readFile (vcdOf name)
  pure (P.length [l | l <- P.lines vcd, P.take 1 l P.== "#"] P.- 1)

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
          out <- writeWaveform vcdOptions name (0, 8) counter
          out @?= [0 .. 7]
          (v, j) <- filesFor name
          assertBool "both halves written" (v P.&& j)
    , -- The files went through openTempFile, which makes them 0600 whatever
      -- the umask: unreadable to a CI artifact step or a colleague. A file
      -- written the ordinary way, beside them, has the modes to expect.
      testCase "a waveform has the default permissions, not owner-only ones" $
        withName "capture-modes" $ \name -> do
          _ <- writeWaveform vcdOptions name (0, 3) counter
          let reference = vcdOf name -<.> "reference"
              modes p = intersectFileModes accessModes . fileMode <$> getFileStatus p
          P.writeFile reference ""
          expected <- modes reference
          removeFile reference
          vcd <- modes (vcdOf name)
          json <- modes (vcdOf name -<.> "json")
          (vcd, json) @?= (expected, expected)
          -- The same for an FST and its sidecar.
          _ <- writeWaveform defaultOptions name (0, 3) counter
          fst' <- modes (vcdOf name -<.> "fst")
          json' <- modes (vcdOf name -<.> "json")
          removeFile (vcdOf name -<.> "fst")
          (fst', json') @?= (expected, expected)
    , -- withRecording is the API for a design with more than one domain. It
      -- used to open its spill file without making the directory, so the
      -- first run in a fresh checkout failed with "does not exist".
      testCase "withRecording makes the directory of its spill file" $ do
        let dir = "waveforms" </> "capture-new-directory"
        removePathForcibly dir
        (out, rec) <- withRecording (Just (dir </> "run.spill")) (evaluate (sampleN 3 counter))
        P.either (P.error . P.show) pure out >>= (@?= [0, 1, 2])
        spilled <- doesFileExist (dir </> "run.spill")
        assertBool "the spill file is in the new directory" spilled
        discardRecording rec
        removePathForcibly dir
    , -- A window shows its cycles only, and the samples are all of them.
      testCase "writeWaveform shows the window, and returns every sample" $
        withName "capture-window" $ \name -> do
          out <- writeWaveform vcdOptions name (3, 8) counter
          out @?= [0 .. 7]
          vcd <- P.readFile (vcdOf name)
          P.take 1 [l | l <- P.lines vcd, P.take 1 l P.== "#"] @?= ["#3"]
          n <- cyclesOf name
          n @?= 5
    , -- A suite that does not know how long its firmware runs passes
      -- maxBound and lets the check decide. The window's end in ps must not
      -- overflow: the waveform has the cycles the check forced.
      testCase "a window ending at maxBound records what the check forced" $
        withName "capture-unbounded" $ \name -> do
          _ <-
            try @SomeException $
              withWaveform vcdOptions name (0, P.maxBound) counter $ \xs -> do
                _ <- evaluate (P.length (P.take 5 xs))
                errorX "deliberate failure"
          (v, j) <- filesFor name
          assertBool "both halves written" (v P.&& j)
          n <- cyclesOf name
          n @?= 5
    , -- As dumpVCD, a window must have 0 <= start <= stop.
      testCase "a window that ends before it starts is refused" $
        withName "capture-bad-window" $ \name -> do
          outcome <- try @ErrorCall (writeWaveform vcdOptions name (5, 3) counter)
          assertBool "refused" (P.either (P.const True) (P.const False) outcome)
          (v, _) <- filesFor name
          assertBool "no VCD" (P.not v)
    , -- A PASSING run writes nothing at all: the simulation runs without a
      -- recording context, so there is no history to render or discard.
      testCase "withWaveform writes nothing when the test passes" $
        withName "capture-passing" $ \name -> do
          _ <- withWaveform vcdOptions name (0, 8) counter evaluate
          (v, j) <- filesFor name
          assertBool "no VCD, no sidecar" (P.not v P.&& P.not j)
    , -- A file left by an earlier FAILING run is deleted when the test
      -- passes: from withWaveform, a file means "this test failed".
      testCase "withWaveform deletes an earlier failure's files when the test passes" $
        withName "capture-stale" $ \name -> do
          plantFailure name
          _ <- withWaveform vcdOptions name (0, 8) counter evaluate
          (v, j) <- filesFor name
          assertBool "no stale VCD, no stale sidecar" (P.not v P.&& P.not j)
    , -- …but keeps a waveform that was written on purpose, by writeWaveform.
      testCase "withWaveform keeps a waveform written on purpose when the test passes" $
        withName "capture-requested" $ \name -> do
          _ <- writeWaveform vcdOptions name (0, 8) counter
          _ <- withWaveform vcdOptions name (0, 8) counter evaluate
          (v, j) <- filesFor name
          assertBool "the requested waveform is still there" (v P.&& j)
    , -- A FAILING run writes both, and the check's exception propagates.
      testCase "withWaveform writes both files when the test fails" $
        withName "capture-failing" $ \name -> do
          outcome <-
            try @SomeException $
              withWaveform vcdOptions name (0, 8) counter $ \xs -> do
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
              withWaveform vcdOptions name (0, 8) counter $ \xs -> do
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
          _ <- writeWaveform vcdOptions name (0, 4) twins
          vcd <- P.readFile (vcdOf name)
          assertBool
            "the divergent change line is present"
            (P.any ("b00000001 " `isPrefixOf`) (P.lines vcd))
    , -- A trace that the end of the run reaches first is finished too, up
      -- to the end of the run: else its last cycle shows as not evaluated.
      testCase "a trace that only the end of the run evaluates has every cycle" $
        withName "capture-late-trace" $ \name -> do
          out <- writeWaveform vcdOptions name (0, 2) lateStep
          out @?= [0, 0]
          vcd <- P.readFile (vcdOf name)
          Map.lookup "step" (decodeVCD 2 vcd) @?= P.Just ["00000001", "00000001"]
    , -- Finishing a trace of the run can reach a trace the run did not:
      -- that one has the cycles that were evaluated, and is not finished
      -- itself, so the stages after it are not reached (see the next test).
      testCase "a stage that only finishing reaches has the cycles it evaluated" $
        withName "capture-late-pipeline" $ \name -> do
          reached <- newIORef False
          (out, rec) <- withRecording (Just (vcdOf name -<.> "spill")) $ do
            let xs = sampleN 2 (pipeline reached)
            _ <- evaluate (xs `deepseqX` ())
            (,) xs <$> readIORef reached
          P.either (P.error . P.show) pure out >>= (@?= ([0, 1], False))
          _ <- writeVCD (0, 2) rec (vcdOf name)
          discardRecording rec
          vcd <- P.readFile (vcdOf name)
          P.fmap asInts (decodeVCD 2 vcd)
            @?= Map.fromList
              [ ("s0", [P.Just 0, P.Just 1])
              , ("s1", [P.Just 1, P.Just 2])
              , ("s2", [P.Just 2, P.Nothing])
              ]
    , -- A design whose stages never end makes no recording that never ends:
      -- finishing it stops, and the recording closes.
      testCase "a pipeline without a last stage finishes" $
        withName "capture-endless" $ \name -> do
          done <- timeout 20000000 $ do
            (out, rec) <- withRecording (Just (vcdOf name -<.> "spill")) $ do
              let xs = sampleN 3 (endless 0)
              _ <- evaluate (xs `deepseqX` ())
              pure xs
            discardRecording rec
            pure out
          P.fmap (P.either (P.error . P.show) P.id) done @?= P.Just [0, 1, 2]
    , -- The same contract inside a hedgehog property, where a failure is a
      -- VALUE in the property monad and no amount of `try` in IO can see it.
      testCase "simulateWaveform writes nothing when the property passes" $
        withName "capture-prop-passing" $ \name -> do
          ok <-
            Hedgehog.check . Hedgehog.property $ do
              xs <- simulateWaveform vcdOptions Counterexample name (0, 8) counter
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
              xs <- simulateWaveform vcdOptions Counterexample name (0, 8) counter
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
              out <- simulateWaveform vcdOptions Counterexample name (0, P.length xs) counter
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
    , -- A path that cannot be the path of a waveform without extension is
      -- refused before the design runs (see Test.Formats for the others).
      testCase "simulateWaveform refuses a path with the extension of a waveform" $ do
        seed <- Seed.random
        let Property config test =
              Hedgehog.withTests 1
                . Hedgehog.property
                $ ()
                <$ simulateWaveform vcdOptions Counterexample "waveforms/capture-bad.vcd" (0, 8) ran
        report <- checkReport config 0 seed test (\_ -> pure ())
        text <- Report.renderResult DisableColor Nothing report
        -- The exception the report shows (its source excerpt has this test's
        -- own lines, so the whole text would say anything).
        let thrown = P.drop 1 (P.dropWhile (P.not . ("Exception (ErrorCall)" `isInfixOf`)) (P.lines text))
        assertBool
          ("reported: " <> text)
          ( P.any
              ("clash-circuit-context: the path \"waveforms/capture-bad.vcd\" ends in .vcd" `isInfixOf`)
              (P.take 1 thrown)
          )
    , -- A directory that cannot hold the waveform is refused before the
      -- design runs too (see Test.Formats for the others). Before, the case
      -- ran, and the footnote said that no waveform was written.
      testCase "simulateWaveform refuses a directory that is a file" $ do
        aFile <- makeAbsolute "waveforms/capture-a-file"
        createDirectoryIfMissing True "waveforms"
        removePathForcibly aFile
        P.writeFile aFile ""
        seed <- Seed.random
        let Property config test =
              Hedgehog.withTests 1
                . Hedgehog.property
                $ ()
                <$ simulateWaveform vcdOptions Counterexample (aFile </> "x") (0, 8) ran
        report <- checkReport config 0 seed test (\_ -> pure ())
        text <- Report.renderResult DisableColor Nothing report
        removeFile aFile
        assertBool
          ("reported: " <> text)
          ((aFile </> "x" <> ": " <> aFile <> " is a file, not a directory") `isInfixOf` text)
    , -- AtSize reads the size of the case without a forAll: Hedgehog's
      -- report of a failure lists every forAll, and AtSize's showed as an
      -- empty "forAll0 =".
      testCase "AtSize adds nothing to the report of a failure"
        $ withName "capture-prop-report"
        $ \name -> do
          seed <- Seed.random
          let Property config test =
                Hedgehog.withTests 1 . Hedgehog.withShrinks 0 . Hedgehog.property $ do
                  _ <- Hedgehog.forAll (Gen.int (Range.linear 0 9))
                  _ <- simulateWaveform vcdOptions (AtSize 0) name (0, 4) counter
                  Hedgehog.failure
          report <- checkReport config 0 seed test (\_ -> pure ())
          text <- Report.renderResult DisableColor Nothing report
          -- The test's own forAll is the report's only annotation. Where the
          -- report cannot read the source of an annotation (the library's,
          -- in a project that depends on it), it shows a line "forAll0 =".
          case Report.reportStatus report of
            Report.Failed failed ->
              P.map (P.fmap spanFile . Report.failedSpan) (Report.failureAnnotations failed)
                @?= [P.Just "tests/Test/Capture.hs"]
            _ -> assertFailure ("reported: " <> text)
          assertBool ("reported: " <> text) ("waveform: " `isInfixOf` text)
          P.mapM_
            removeFile
            [ vcdOf name
            , vcdOf name -<.> "json"
            , vcdOf (name <> "_0")
            , vcdOf (name <> "_0") -<.> "json"
            ]
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
              xs <- simulateWaveform vcdOptions (AtSize 10) name (0, 8) counter
              P.length xs Hedgehog.=== 8
          assertBool "the property passes" ok
          -- Under its own name, so that a counterexample does not replace it.
          (v, j) <- filesFor (name <> "_10")
          assertBool "both halves written" (v P.&& j)
          filesFor name >>= (@?= (False, False))
          P.mapM_ removeFile [vcdOf (name <> "_10"), vcdOf (name <> "_10") -<.> "json"]
    , -- …and only once per name: a second property with the same name, run
      -- in the same process, does not write it again.
      testCase "AtSize keeps one case per name, once" $
        withName "capture-prop-once" $ \name -> do
          _ <-
            Hedgehog.check . Hedgehog.withTests 5 . Hedgehog.property $
              () <$ simulateWaveform vcdOptions (AtSize 0) name (0, 8) counter
          first <- filesFor (name <> "_0")
          first @?= (True, True)
          removeFile (vcdOf (name <> "_0"))
          removeFile (vcdOf (name <> "_0") -<.> "json")
          _ <-
            Hedgehog.check . Hedgehog.withTests 5 . Hedgehog.property $
              () <$ simulateWaveform vcdOptions (AtSize 0) name (0, 8) counter
          second <- filesFor (name <> "_0")
          second @?= (False, False)
    , -- …also when the second property spells the path another way.
      testCase "AtSize keeps one case per path, however it is spelled" $
        withName "capture-prop-spelled" $ \name -> do
          _ <-
            Hedgehog.check . Hedgehog.withTests 5 . Hedgehog.property $
              () <$ simulateWaveform vcdOptions (AtSize 0) name (0, 8) counter
          first <- filesFor (name <> "_0")
          first @?= (True, True)
          removeFile (vcdOf (name <> "_0"))
          removeFile (vcdOf (name <> "_0") -<.> "json")
          _ <-
            Hedgehog.check . Hedgehog.withTests 5 . Hedgehog.property $
              () <$ simulateWaveform vcdOptions (AtSize 0) ("." </> name) (0, 8) counter
          second <- filesFor (name <> "_0")
          second @?= (False, False)
    ]
