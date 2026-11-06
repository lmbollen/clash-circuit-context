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
import Control.Exception (AsyncException (ThreadKilled), SomeException, evaluate, throwIO, try)
import Control.Monad (forM_, when)
import Data.List (isPrefixOf)
import Foreign.Ptr (plusPtr)
import System.Directory (doesFileExist, getFileSize, makeAbsolute, removeFile)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath ((<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.IO (OpenMode (ReadOnly), closeFd, defaultFileFlags, fdReadBuf, openFd)
import System.Timeout (timeout)

import SpillMain (spillWith)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString (createAndTrim)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.CircuitContext.Recording (
  discardRecording,
  recordingEnd,
  recordingStart,
  recoverRecording,
  writeFST,
  writeVCD,
 )
import Clash.CircuitContext.Waveform

-- | A traced counter, described in the sidecar; it runs as long as it is sampled.
counter :: (CircuitContextOnly) => Signal System (Unsigned 8)
counter = traceSignalSW "count" (fromList (P.cycle [0 .. 255]))
{-# OPAQUE counter #-}

{- | A traced signal whose value at cycle 2000 takes two seconds to compute,
seen only through its spine, so that only the recorder computes its values: a
run that is cancelled during the wait is cancelled while the recorder packs
that value. A run that is not cancelled ends after 4000 cycles.
-}
slow :: (CircuitContextOnly) => Signal System (Unsigned 8)
slow = P.const 0 <$> traceSignalSW "count" (fromList [valueAt i | i <- [0 :: Int ..]])
{-# OPAQUE slow #-}

-- | A signal that changes every cycle, and one that never changes.
busyAndQuiet :: (CircuitContextOnly) => Signal System (Unsigned 8)
busyAndQuiet =
  (+)
    <$> traceSignalSW "busy" (fromList (P.cycle [0 .. 255]))
    <*> traceSignalSW "quiet" (pure 7)
{-# OPAQUE busyAndQuiet #-}

valueAt :: Int -> Unsigned 8
valueAt i
  | i P.== 2000 = unsafePerformIO (threadDelay 2000000 >> pure 0)
  | P.otherwise = P.fromIntegral i
{-# NOINLINE valueAt #-}

{- | The bytes of a file this process has open for writing, which GHC's own
file functions refuse to open.
-}
unlockedRead :: FilePath -> IO ByteString.ByteString
unlockedRead path = do
  size <- P.fromIntegral <$> getFileSize path
  fd <- openFd path ReadOnly defaultFileFlags
  bytes <- ByteString.createAndTrim size $ \p ->
    let go n
          | n P.>= size = pure n
          | P.otherwise = do
              got <- P.fromIntegral <$> fdReadBuf fd (p `plusPtr` n) (P.fromIntegral (size P.- n))
              if got P.== 0 then pure n else go (n P.+ got)
     in go 0
  closeFd fd
  pure bytes

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
        absolute <- makeAbsolute (file name "vcd")
        recovered @?= Right absolute
        cycles <- timestamps (file name "vcd")
        assertBool "the recovered waveform has cycles" (cycles P.> 1)
        existing name ["spill"] >>= (@?= [False])
    , testCase "a cancelled run in memory leaves nothing" $ do
        let name = "formats-cancelled-in-memory"
        scrub [name]
        stopped <- timeout 500000 (writeWaveform defaultOptions name (0, 4000) slow)
        assertBool "the run was cancelled" (P.null stopped)
        existing name ["spill", "fst", "json"] >>= (@?= [False, False, False])
    , testCase "spill2vcd and spill2fst write the waveform the library writes" $ do
        let spillFile = file "formats-cli" "spill"
        scrub ["formats-cli", "formats-cli-library", "formats-cli-exe"]
        (_, rec) <-
          withRecording
            (Just spillFile)
            (evaluate (P.sum (P.map P.toInteger (sampleN 300 counter))))
        _ <-
          writeVCD (recordingStart rec, recordingEnd rec) rec (file "formats-cli-library" "vcd")
        code <- spillWith VCD "spill2vcd" ["-o", file "formats-cli-exe" "vcd", spillFile]
        code @?= ExitSuccess
        a <- vcdBody (file "formats-cli-library" "vcd")
        b <- vcdBody (file "formats-cli-exe" "vcd")
        a @?= b
        -- By default next to the spill file, which --delete deletes.
        code' <- spillWith FST "spill2fst" ["--delete", spillFile]
        code' @?= ExitSuccess
        existing "formats-cli" ["fst", "json", "spill"] >>= (@?= [True, True, False])
        -- A file that is not a spill file is refused.
        code'' <- spillWith VCD "spill2vcd" [file "formats-cli" "json"]
        assertBool "refused" (code'' P./= ExitSuccess)
    , -- A killed process writes nothing more: what it leaves must have every
      -- signal, also one whose records never filled a chunk.
      testCase "a killed run's spill file has every signal, up to about a second before" $ do
        let
          spillFile = file "formats-killed" "spill"
          snapshot = file "formats-killed-snapshot" "spill"
        scrub ["formats-killed", "formats-killed-snapshot"]
        (r, rec) <- withRecording (Just spillFile) $ do
          _ <- evaluate (P.sum (P.map P.toInteger (sampleN 3000 busyAndQuiet)))
          threadDelay 1500000
          -- What the process would leave if it were killed now. Through a
          -- descriptor: GHC refuses to open a file this process writes.
          unlockedRead spillFile >>= ByteString.writeFile snapshot
        P.either (assertFailure . P.show) (P.const (pure ())) r
        discardRecording rec
        killed <- recoverRecording snapshot
        case killed of
          Left err -> assertFailure err
          Right rec' -> do
            -- The last forced cycle of a trace is recorded with the next one.
            assertBool "every cycle but the last" (recordingEnd rec' P.>= 2999)
            written <- writeVCD (0, recordingEnd rec') rec' (file "formats-killed-snapshot" "vcd")
            written @?= Right ()
            body <- vcdBody (file "formats-killed-snapshot" "vcd")
            let values = [l | l <- body, P.take 1 l P.== "b"]
            assertBool "both signals have values" (P.length values P.> 2)
            assertBool "no signal is z" (P.not (P.any ('z' `P.elem`) values))
        removeFile snapshot
    , -- A cancelled run writes everything it recorded: also the progress of a
      -- signal that has not changed since the last tick.
      testCase "a cancelled run's spill file has every signal up to the end" $ do
        let spillFile = file "formats-cancelled-quiet" "spill"
        scrub ["formats-cancelled-quiet"]
        stopped <- try @AsyncException $ withRecording (Just spillFile) $ do
          -- One instance, sampled in two steps.
          let xs = P.map P.toInteger (sample busyAndQuiet)
          _ <- evaluate (P.sum (P.take 3000 xs))
          -- A tick writes "quiet" and its progress; after it, "quiet" has no
          -- new records.
          threadDelay 1500000
          _ <- evaluate (P.sum (P.take 6000 xs))
          throwIO ThreadKilled
        assertBool "the run was cancelled" (P.either (P.const True) (P.const False) stopped)
        recovered <- recoverRecording spillFile
        case recovered of
          Left err -> assertFailure err
          Right rec -> do
            assertBool "every cycle but the last" (recordingEnd rec P.>= 5999)
            written <- writeVCD (0, recordingEnd rec) rec (file "formats-cancelled-quiet" "vcd")
            written @?= Right ()
            body <- vcdBody (file "formats-cancelled-quiet" "vcd")
            assertBool
              "no signal is z"
              (P.not (P.any ('z' `P.elem`) [l | l <- body, P.take 1 l P.== "b"]))
            discardRecording rec
    , testCase "writeVCD and writeFST refuse a window that shows nothing, and IO errors" $ do
        let name = "formats-windows"
        scrub [name]
        (_, rec) <-
          withRecording Nothing (evaluate (P.sum (P.map P.toInteger (sampleN 40 counter))))
        recordingEnd rec @?= 40
        let refused w = do
              v <- writeVCD w rec (file name "vcd")
              f <- writeFST w rec (file name "fst")
              assertBool
                ("VCD of " <> P.show w <> " refused")
                (P.either (P.const True) (P.const False) v)
              assertBool
                ("FST of " <> P.show w <> " refused")
                (P.either (P.const True) (P.const False) f)
        refused (30, 20)
        refused (10, 10)
        refused (40, 50)
        refused (1000, 2000)
        existing name ["vcd", "fst", "json"] >>= (@?= [False, False, False])
        -- A file that cannot be written is a Left, not an exception.
        unwritable <- writeVCD (0, 40) rec "/proc/no-such-directory/x.vcd"
        assertBool "cannot write" (P.either (P.const True) (P.const False) unwritable)
        unwritable' <- writeFST (0, 40) rec "/proc/no-such-directory/x.fst"
        assertBool "cannot write" (P.either (P.const True) (P.const False) unwritable')
        -- A window past the end ends at the end; its last timestamp is the end
        -- of the last cycle, as in the FST.
        written <- writeVCD (35, 100) rec (file name "vcd")
        written @?= Right ()
        body <- vcdBody (file name "vcd")
        P.filter ("#" `isPrefixOf`) body @?= ["#35", "#36", "#37", "#38", "#39", "#40"]
    , testCase
        "spill2vcd says why it cannot read a file, and refuses a spill file without cycles"
        $ do
          let spillFile = file "formats-cli-empty" "spill"
          scrub ["formats-cli-empty"]
          missing <- spillWith VCD "spill2vcd" [file "formats-cli-missing" "spill"]
          missing @?= ExitFailure 1
          (_, rec) <- withRecording (Just spillFile) (pure ())
          empty <- spillWith VCD "spill2vcd" [spillFile]
          empty @?= ExitFailure 1
          existing "formats-cli-empty" ["vcd", "json"] >>= (@?= [False, False])
          discardRecording rec
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
