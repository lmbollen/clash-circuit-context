{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
-- 'slow' waits with 'threadDelay', whose 'onException' rethrows a timeout
-- synchronously, which stores it in the thunk that waited. Floated to the
-- top level, that thunk would be shared, and the next test would get the
-- previous test's timeout.
{-# OPTIONS_GHC -fno-full-laziness #-}

{- | The waveform options: the format a waveform is written as, and where a
recording keeps its values until then. Each choice must give the same
waveform as the others, and a spill file must outlive a run that is stopped.

Every test uses its own waveform names and scrubs their files first.
-}
module Test.Formats (tests) where

import qualified Prelude as P

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, when)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist, getFileSize, removeFile)
import System.FilePath ((<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import qualified Data.ByteString as ByteString

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.CircuitContext.Recording (
  discardRecording,
  recordingEnd,
  recoverRecording,
  writeVCD,
 )
import Clash.CircuitContext.Waveform

-- | A traced counter, described in the sidecar; it runs as long as it is sampled.
counter :: (HasCircuitContext) => Signal System (Unsigned 8)
counter = traceSignalSW "count" (fromList (P.cycle [0 .. 255]))
{-# OPAQUE counter #-}

{- | A traced signal whose value at cycle 2000 takes two seconds to compute,
seen only through its spine, so that only the recorder computes its values: a
run that is cancelled during the wait is cancelled while the recorder packs
that value. A run that is not cancelled ends after 4000 cycles.
-}
slow :: (HasCircuitContext) => Signal System (Unsigned 8)
slow = P.const 0 <$> traceSignalSW "count" (fromList [valueAt i | i <- [0 :: Int ..]])
{-# OPAQUE slow #-}

valueAt :: Int -> Unsigned 8
valueAt i
  | i P.== 2000 = unsafePerformIO (threadDelay 2000000 >> pure 0)
  | P.otherwise = P.fromIntegral i
{-# NOINLINE valueAt #-}

-- | A file of a waveform name.
file :: String -> String -> FilePath
file name ext = "waveforms" </> name <.> ext

-- | Delete every file of these waveform names.
scrub :: [String] -> IO ()
scrub names =
  forM_ [file n e | n <- names, e <- ["vcd", "fst", "json", "spill"]] $ \p ->
    doesFileExist p >>= \there -> when there (removeFile p)

-- | Which of these files of a waveform name exist.
existing :: String -> [String] -> IO [Bool]
existing name = P.mapM (doesFileExist . file name)

-- | A VCD's lines without its @$date@ line, which differs between two runs.
vcdBody :: FilePath -> IO [String]
vcdBody path = P.filter (P.not . ("$date" `isPrefixOf`)) . P.lines <$> P.readFile path

-- | How many timestamps a VCD has.
timestamps :: FilePath -> IO Int
timestamps path = P.length . P.filter ("#" `isPrefixOf`) . P.lines <$> P.readFile path

vcd, vcdOnDisk :: WaveformOptions
vcd = defaultOptions{format = VCD}
vcdOnDisk = defaultOptions{format = VCD, spill = OnDisk}

tests :: TestTree
tests =
  testGroup
    "Test.Formats"
    [ testCase "the default options write an FST and its sidecar, and no spill file" $ do
        let name = "formats-default"
        scrub [name]
        _ <- writeWaveform defaultOptions name (0, 8) counter
        existing name ["fst", "json", "vcd", "spill"] >>= (@?= [True, True, False, False])
        -- An FST file starts with its header block, whose tag is 0.
        header <- ByteString.take 1 <$> ByteString.readFile (file name "fst")
        header @?= ByteString.pack [0]
    , testCase "a spill file on disk gives the waveform a spill in memory gives" $ do
        let (onDisk, inMemory) = ("formats-on-disk", "formats-in-memory")
        scrub [onDisk, inMemory]
        _ <- writeWaveform vcdOnDisk onDisk (0, 40) counter
        _ <- writeWaveform vcd inMemory (0, 40) counter
        a <- vcdBody (file onDisk "vcd")
        b <- vcdBody (file inMemory "vcd")
        a @?= b
        existing onDisk ["spill"] >>= (@?= [False])
    , testCase "an FST has the sidecar of the VCD" $ do
        let (v, f) = ("formats-sidecar-vcd", "formats-sidecar-fst")
        scrub [v, f]
        _ <- writeWaveform vcd v (0, 40) counter
        _ <- writeWaveform defaultOptions f (0, 40) counter
        a <- P.readFile (file v "json")
        b <- P.readFile (file f "json")
        assertBool "the counter is described" ("logic.count" `P.elem` P.words (P.map unquote a))
        a @?= b
    , testCase "withWaveform deletes an earlier failing run's FST when the test passes" $ do
        let name = "formats-stale-fst"
        scrub [name]
        failed <-
          try @SomeException
            (withWaveform defaultOptions name (0, 8) counter (\xs -> P.length xs @?= 9))
        assertBool "the check failed" (P.either (P.const True) (P.const False) failed)
        existing name ["fst", "json"] >>= (@?= [True, True])
        _ <- withWaveform defaultOptions name (0, 8) counter (\xs -> P.length xs @?= 8)
        existing name ["fst", "json"] >>= (@?= [False, False])
    , testCase "withWaveform keeps an FST written on purpose when the test passes" $ do
        let name = "formats-requested-fst"
        scrub [name]
        _ <- writeWaveform defaultOptions name (0, 8) counter
        _ <- withWaveform defaultOptions name (0, 8) counter (\xs -> P.length xs @?= 8)
        existing name ["fst", "json"] >>= (@?= [True, True])
    , testCase "a cancelled run keeps its spill file, and recoverWaveform writes its waveform" $ do
        let name = "formats-cancelled"
        scrub [name]
        stopped <- timeout 500000 (writeWaveform vcdOnDisk name (0, 4000) slow)
        assertBool "the run was cancelled" (P.null stopped)
        existing name ["spill", "vcd"] >>= (@?= [True, False])
        recovered <- recoverWaveform vcd name
        recovered @?= Right (file name "vcd")
        cycles <- timestamps (file name "vcd")
        assertBool "the recovered waveform has cycles" (cycles P.> 1)
        existing name ["spill"] >>= (@?= [False])
    , testCase "a cancelled run in memory leaves nothing" $ do
        let name = "formats-cancelled-in-memory"
        scrub [name]
        stopped <- timeout 500000 (writeWaveform defaultOptions name (0, 4000) slow)
        assertBool "the run was cancelled" (P.null stopped)
        existing name ["spill", "fst", "json"] >>= (@?= [False, False, False])
    , testCase "a spill file cut off in a block is read up to its last whole block" $ do
        let
          spillFile = file "formats-cut" "spill"
          cutFile = file "formats-cut-short" "spill"
        scrub ["formats-cut", "formats-cut-short", "formats-cut-whole", "formats-cut-read"]
        -- Enough cycles for several chunks of records.
        (r, rec) <-
          withRecording
            (Just spillFile)
            (evaluate (P.sum (P.map P.toInteger (sampleN 20000 counter))))
        assertBool "the run finished" (P.either (P.const False) (P.const True) r)
        _ <- writeVCD (0, recordingEnd rec) rec (file "formats-cut-whole" "vcd")
        -- The whole file reads back as the same recording.
        whole <- recoverRecording spillFile
        case whole of
          Left err -> assertFailure err
          Right rec' -> do
            recordingEnd rec' @?= recordingEnd rec
            _ <- writeVCD (0, recordingEnd rec') rec' (file "formats-cut-read" "vcd")
            a <- vcdBody (file "formats-cut-whole" "vcd")
            b <- vcdBody (file "formats-cut-read" "vcd")
            a @?= b
        -- Cut off inside the last block: the run up to its last whole chunks.
        size <- getFileSize spillFile
        ByteString.readFile spillFile
          >>= ByteString.writeFile cutFile
          . ByteString.take (P.fromIntegral size P.- 100)
        cut <- recoverRecording cutFile
        case cut of
          Left err -> assertFailure err
          Right rec' -> do
            assertBool "fewer cycles than the whole run" (recordingEnd rec' P.< recordingEnd rec)
            assertBool "but some" (recordingEnd rec' P.> 0)
            written <- writeVCD (0, recordingEnd rec') rec' (file "formats-cut-short" "vcd")
            written @?= Right ()
        -- Followed by a block that claims more bytes than the file has, as
        -- one that was cut off would: read up to it, without trying to.
        let garbledFile = file "formats-cut-garbled" "spill"
        ByteString.readFile spillFile
          >>= ByteString.writeFile garbledFile
          . (<> ByteString.pack [2, 255, 255, 255, 127, 1, 2, 3])
        garbled <- recoverRecording garbledFile
        P.fmap recordingEnd garbled @?= Right (recordingEnd rec)
        discardRecording rec
        P.mapM_ removeFile [cutFile, garbledFile]
    ]
 where
  unquote c = if c P.== '"' then ' ' else c
