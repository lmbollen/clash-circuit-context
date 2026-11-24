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

Every test uses its own waveform paths and scrubs their files first.
-}
module Test.Formats (tests, children) where

import qualified Prelude as P

import Control.Concurrent (
  forkFinally,
  killThread,
  newEmptyMVar,
  putMVar,
  takeMVar,
  threadDelay,
  yield,
 )
import Control.Exception (
  AsyncException (ThreadKilled),
  IOException,
  SomeException,
  evaluate,
  finally,
  throwIO,
  try,
 )
import Control.Monad (forM, forM_, when)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import GHC.Stats (gc, gcdetails_live_bytes, getRTSStats)
import System.Directory (
  canonicalizePath,
  createDirectoryIfMissing,
  doesFileExist,
  getFileSize,
  listDirectory,
  makeAbsolute,
  removeDirectory,
  removeFile,
  removePathForcibly,
 )
import System.Environment (setEnv, unsetEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath ((<.>), (</>))
import System.IO (hFlush, hGetLine, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performMajorGC)
import System.Posix.Files (readSymbolicLink)
import System.Posix.Resource (
  Resource (ResourceFileSize),
  ResourceLimit (ResourceLimit),
  ResourceLimits (softLimit),
  getResourceLimit,
  setResourceLimit,
 )
import System.Posix.Signals (
  Handler (Ignore),
  installHandler,
  sigKILL,
  sigXFSZ,
  signalProcess,
 )
import System.Process (getPid, waitForProcess)
import System.Timeout (timeout)

import SpillMain (spillWith)
import Test.Child (Child, assertChild, inChild, startChild)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteStringLazy

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.CircuitContext.Internal.SpillTesting (withoutMappings)
import Clash.CircuitContext.Recording (
  discardRecording,
  recordedChanges,
  recordedSignals,
  recordingEnd,
  recordingStart,
  recoverRecording,
  writeFST,
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

{- | Values that do not compress: an FST of them is as large as they are.
Eight bytes a cycle.
-}
noise :: (HasCircuitContext) => Signal System (Unsigned 64)
noise =
  traceSignalSW
    "noise"
    (fromList (P.iterate (\x -> x P.* 6364136223846793005 P.+ 1442695040888963407) 1))
{-# OPAQUE noise #-}

-- | 300 traced signals that each change every 64 cycles.
quietMany :: (HasCircuitContext) => Signal System (Unsigned 16)
quietMany =
  P.foldr1
    (liftA2 (+))
    [ traceSignalSW ("q" <> P.show i) ((`shiftR` 6) . (+ P.fromIntegral i) <$> count)
    | i <- [1 .. 300 :: Int]
    ]
 where
  count = fromList [0 :: Unsigned 16 ..]
{-# OPAQUE quietMany #-}

-- | A signal that changes every cycle, and one that never changes.
busyAndQuiet :: (HasCircuitContext) => Signal System (Unsigned 8)
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

{- | The regions of a spill file: each one's offset, size and owner (0: the
metadata, @i + 1@: signal @i@).
-}
regionsOf :: ByteString.ByteString -> [(Int, Int, Int)]
regionsOf contents = go (word 24)
 where
  word o =
    P.foldr
      (\i acc -> acc P.* 256 P.+ P.fromIntegral (ByteString.index contents (o P.+ i)))
      0
      [0 .. 7]
  go off
    | off P.+ 32 P.> ByteString.length contents P.|| word off P.== 0 = []
    | P.otherwise = (off, word off, word (off P.+ 8)) : go (off P.+ word off)

-- | The page size the spill file's regions are multiples of: its first region's offset.
pageOf :: ByteString.ByteString -> Int
pageOf contents = case regionsOf contents of
  (off, _, _) : _ -> off
  [] -> 0

-- | The path of a waveform name: in @waveforms@, as every test's.
pathOf :: String -> FilePath
pathOf name = "waveforms" </> name

-- | A file of a waveform name.
file :: String -> String -> FilePath
file name ext = pathOf name <.> ext

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

-- | The tests of this module that run in a process of their own ("Test.Child").
children :: [Child]
children =
  [
    ( "formats-cut-fst"
    , do
        let name = "formats-cut-fst"
        scrub [name]
        failed <-
          try @SomeException
            (withWaveform defaultOptions (pathOf name) (0, 8) counter (\xs -> P.length xs @?= 9))
        assertBool "the check failed" (P.either (P.const True) (P.const False) failed)
        whole <- ByteString.readFile (file name "fst")
        ByteString.writeFile (file name "fst") (ByteString.take 100 whole)
        _ <- withWaveform defaultOptions (pathOf name) (0, 8) counter (\xs -> P.length xs @?= 8)
        existing name ["fst", "json"] >>= (@?= [False, False])
    )
  , -- A cancelled run: one that recorded nothing, and one that did.
    ("formats-stopped-empty", stoppedRun "formats-stopped-empty" (pure ()))
  ,
    ( "formats-stopped-recorded"
    , stoppedRun
        "formats-stopped-recorded"
        (() <$ evaluate (P.sum (P.map P.toInteger (sampleN 100 counter))))
    )
  ,
    ( "formats-cancel-storm"
    , do
        -- Many recordings, each cancelled at a different moment: none may
        -- leave its spill file open. It counts the open files of the process.
        let dir = pathOf "formats-cancel-storm"
        removePathForcibly dir
        createDirectoryIfMissing True dir
        ends <- forM [1 .. 20000 :: Int] $ \i -> do
          ended <- newEmptyMVar
          run <-
            forkFinally
              (withRecording (Just (dir </> P.show (i `P.mod` 50) <.> "spill")) (pure ()))
              (\_ -> putMVar ended ())
          P.sequence_ (P.replicate (i `P.mod` 40) yield)
          killThread run
          pure ended
        P.mapM_ takeMVar ends
        fds <- listDirectory "/proc/self/fd"
        links <- P.mapM (try @IOException . readSymbolicLink . ("/proc/self/fd" </>)) fds
        [l | P.Right l <- links, dir `isInfixOf` l] @?= []
        removePathForcibly dir
    )
  ,
    ( "formats-read-big-spill"
    , do
        -- Reading a spill file keeps where its records are, not the records:
        -- the live heap stays far below the file's 64 MiB. It measures the
        -- heap of the whole process.
        let spillFile = file "formats-read-big-spill" "spill"
            regions = 1024 :: Int
            -- Records of 0 at every cycle: 3 bytes each, as many as fit.
            used = (65536 P.- 32) `P.div` 3 P.* 3
            perRegion = used `P.div` 3
            varint :: Int -> Builder.Builder
            varint x
              | x P.< 0x80 = Builder.word8 (P.fromIntegral x)
              | P.otherwise =
                  Builder.word8 (P.fromIntegral (x `P.mod` 0x80) P.+ 0x80) <> varint (x `P.div` 0x80)
            word = Builder.word64LE . P.fromIntegral
            -- Signal 0: a trace of 8 bits, at 10000 ps, named "big", undescribed.
            declared =
              Builder.toLazyByteString
                $ varint 0
                <> Builder.word8 0
                <> varint 10000
                <> varint 8
                <> varint 3
                <> Builder.string7 "big"
                <> Builder.word8 0
            block =
              Builder.word8 1
                <> Builder.word32LE (P.fromIntegral (ByteStringLazy.length declared))
                <> Builder.lazyByteString declared
            padded n b =
              Builder.toLazyByteString b
                <> ByteStringLazy.replicate (n P.- ByteStringLazy.length (Builder.toLazyByteString b)) 0
            records i =
              word 65536
                <> word 1
                <> word used
                <> word (i P.* perRegion)
                <> Builder.byteString (ByteString.replicate used 0)
        createDirectoryIfMissing True "waveforms"
        ByteStringLazy.writeFile spillFile
          $ padded
            4096
            ( Builder.string7 "CCCSPILL"
                <> Builder.word8 2
                <> Builder.byteString (ByteString.replicate 7 0)
                <> word 0
                <> word 4096
            )
          <> padded
            4096
            ( word 4096
                <> word 0
                <> word (P.fromIntegral (ByteStringLazy.length (Builder.toLazyByteString block)))
                <> word 0
                <> block
            )
          <> P.foldMap (padded 65536 . records) [1 .. regions]
        flip finally (removeFile spillFile) $ do
          read' <- recoverRecording spillFile
          rec <- P.either assertFailure pure read'
          performMajorGC
          live <- gcdetails_live_bytes . gc <$> getRTSStats
          assertBool ("the live heap is " <> P.show live <> " bytes") (live P.< 16000000)
          -- The recording is alive until here.
          recordingEnd rec @?= P.fromIntegral (regions P.* perRegion)
    )
  ,
    ( "formats-killed"
    , do
        -- Records, says how far, and waits to be killed (see the test).
        let name = "formats-killed"
        _ <- withRecording (Just (file name "spill")) $ do
          _ <- evaluate (P.sum (P.map P.toInteger (sampleN 3000 busyAndQuiet)))
          P.putStrLn "recorded 3000 cycles"
          hFlush stdout
          threadDelay 60000000
        assertFailure "the child was not killed"
    )
  ,
    ( "formats-killed-unmapped"
    , do
        -- The same, written without mappings.
        let name = "formats-killed-unmapped"
        _ <- withoutMappings $ withRecording (Just (file name "spill")) $ do
          _ <- evaluate (P.sum (P.map P.toInteger (sampleN 3000 busyAndQuiet)))
          P.putStrLn "recorded 3000 cycles"
          hFlush stdout
          threadDelay 60000000
        assertFailure "the child was not killed"
    )
  ,
    ( "formats-full-disk"
    , do
        -- A file system that takes no more than 256 KiB per file: the spill
        -- file of a counter that changes every cycle cannot hold 200000
        -- cycles. The recording stops where it cannot grow; the run goes on.
        let name = "formats-full-disk"
            cycles = 200000
        scrub [name]
        _ <- installHandler sigXFSZ Ignore P.Nothing
        limits <- getResourceLimit ResourceFileSize
        setResourceLimit ResourceFileSize limits{softLimit = ResourceLimit 262144}
        (r, rec) <-
          withRecording
            (Just (file name "spill"))
            (evaluate (P.sum (P.map P.toInteger (sampleN cycles counter))))
        setResourceLimit ResourceFileSize limits
        P.either (Left . P.show) Right r
          @?= Right (P.sum (P.map P.toInteger (P.take cycles (P.cycle [0 .. 255 :: Int]))))
        let end = recordingEnd rec
        assertBool
          ("recorded " <> P.show end <> " cycles")
          (end P.> 1000 P.&& end P.< P.fromIntegral cycles)
        -- The spill file holds the same: every cycle up to where it stopped.
        recovered <- recoverRecording (file name "spill")
        P.fmap recordingEnd recovered @?= Right end
        written <- writeVCD (0, end) rec (file name "vcd")
        written @?= Right ()
        body <- vcdBody (file name "vcd")
        assertBool
          "no cycle is z"
          (P.not (P.any ('z' `P.elem`) [l | l <- body, P.take 1 l P.== "b"]))
        discardRecording rec
    )
  ,
    ( "formats-full-disk-unmapped"
    , do
        -- The same, written without mappings: a full region that cannot be
        -- written is lost, and the recording ends where it began, in the
        -- spill file and in the recording alike.
        let name = "formats-full-disk-unmapped"
            cycles = 200000
        scrub [name, "formats-full-disk-unmapped-read"]
        _ <- installHandler sigXFSZ Ignore P.Nothing
        limits <- getResourceLimit ResourceFileSize
        setResourceLimit ResourceFileSize limits{softLimit = ResourceLimit 262144}
        (r, rec) <-
          withoutMappings
            $ withRecording
              (Just (file name "spill"))
              (evaluate (P.sum (P.map P.toInteger (sampleN cycles counter))))
        setResourceLimit ResourceFileSize limits
        P.either (Left . P.show) Right r
          @?= Right (P.sum (P.map P.toInteger (P.take cycles (P.cycle [0 .. 255 :: Int]))))
        let end = recordingEnd rec
        assertBool
          ("recorded " <> P.show end <> " cycles")
          (end P.> 1000 P.&& end P.< P.fromIntegral cycles)
        recovered <- recoverRecording (file name "spill")
        rec' <- P.either assertFailure pure recovered
        recordingEnd rec' @?= end
        _ <- writeVCD (0, end) rec (file name "vcd")
        _ <- writeVCD (0, end) rec' (file "formats-full-disk-unmapped-read" "vcd")
        body <- vcdBody (file name "vcd")
        body' <- vcdBody (file "formats-full-disk-unmapped-read" "vcd")
        body @?= body'
        assertBool
          "no cycle is z"
          (P.not (P.any ('z' `P.elem`) [l | l <- body, P.take 1 l P.== "b"]))
        discardRecording rec
    )
  ,
    ( "formats-unmapped"
    , do
        -- Every region a buffer: no mapping, and the waveform a recording
        -- in memory gives, also read back from the spill file.
        let name = "formats-unmapped"
        scrub [name, "formats-unmapped-memory", "formats-unmapped-read"]
        createDirectoryIfMissing True "waveforms"
        spillFile <- (</> (name <.> "spill")) <$> canonicalizePath "waveforms"
        (r, rec) <- withoutMappings $ withRecording (Just spillFile) $ do
          _ <- evaluate (P.sum (P.map P.toInteger (sampleN 50000 busyAndQuiet)))
          P.readFile "/proc/self/maps"
            >>= evaluate
            . P.length
            . P.filter (spillFile `isSuffixOf`)
            . P.lines
        P.either (Left . P.show) Right r @?= Right 0
        (_, inMemory) <-
          withRecording Nothing (evaluate (P.sum (P.map P.toInteger (sampleN 50000 busyAndQuiet))))
        _ <- writeVCD (0, recordingEnd inMemory) inMemory (file "formats-unmapped-memory" "vcd")
        _ <- writeVCD (0, recordingEnd rec) rec (file name "vcd")
        recovered <- recoverRecording spillFile
        rec' <- P.either assertFailure pure recovered
        _ <- writeVCD (0, recordingEnd rec') rec' (file "formats-unmapped-read" "vcd")
        a <- vcdBody (file "formats-unmapped-memory" "vcd")
        b <- vcdBody (file name "vcd")
        c <- vcdBody (file "formats-unmapped-read" "vcd")
        a @?= b
        a @?= c
        discardRecording rec
    )
  ,
    ( "formats-flat-memory"
    , do
        -- 300 signals that each change every 64 cycles, recorded for some
        -- seconds: the live heap at the end of the run is what it was a
        -- quarter of the way in. It measures the heap of the whole process.
        let name = "formats-flat-memory"
            cycles = 40000
        scrub [name]
        (r, rec) <- withRecording (Just (file name "spill")) $ do
          let go :: Int -> [Unsigned 16] -> [Integer] -> IO [Integer]
              go i xs acc
                | i P.== cycles = pure (P.reverse acc)
                | P.otherwise = case xs of
                    x : rest -> do
                      _ <- evaluate x
                      acc' <-
                        if i P.== cycles `P.div` 4 P.|| i P.== cycles P.- 1
                          then do
                            performMajorGC
                            live <- gcdetails_live_bytes . gc <$> getRTSStats
                            pure (P.toInteger live : acc)
                          else pure acc
                      go (i P.+ 1) rest acc'
                    [] -> pure (P.reverse acc)
          go 0 (sample quietMany) []
        lives <- P.either (assertFailure . P.show) pure r
        case lives of
          [quarter, whole] ->
            assertBool
              ("the live heap grew from " <> P.show quarter <> " to " <> P.show whole <> " bytes")
              (whole P.- quarter P.< 32768)
          _ -> assertFailure ("measured " <> P.show lives)
        discardRecording rec
    )
  ,
    ( "formats-fst-too-large"
    , do
        -- A file system that takes no more than 64 KiB per file: an FST of
        -- 20000 cycles of noise, about 160 KiB, cannot be written. That is
        -- an error, and the spill file stays.
        let name = "formats-fst-too-large"
        scrub [name]
        (r, _) <-
          withRecording
            (Just (file name "spill"))
            (evaluate (P.sum (P.map P.toInteger (sampleN 20000 noise))))
        assertBool "the run finished" (P.either (P.const False) (P.const True) r)
        _ <- installHandler sigXFSZ Ignore P.Nothing
        limits <- getResourceLimit ResourceFileSize
        setResourceLimit ResourceFileSize limits{softLimit = ResourceLimit 65536}
        recovered <- recoverWaveform defaultOptions (pathOf name)
        assertBool
          ("recovered: " <> P.show recovered)
          (P.either (P.const True) (P.const False) recovered)
        existing name ["spill", "fst", "json"] >>= (@?= [True, False, False])
        code <- spillWith FST "spill2fst" ["--delete", file name "spill"]
        code @?= ExitFailure 1
        existing name ["spill", "fst", "json"] >>= (@?= [True, False, False])
        left <- P.filter (name `isPrefixOf`) <$> listDirectory "waveforms"
        left @?= [name <.> "spill"]
    )
  ]
 where
  stoppedRun :: String -> ((HasCircuitContext) => IO ()) -> IO ()
  stoppedRun name act = do
    scrub [name]
    stopped <-
      try @AsyncException
        (withRecording (Just (file name "spill")) (act >> throwIO ThreadKilled))
    assertBool "the run was cancelled" (P.either (P.const True) (P.const False) stopped)

tests :: TestTree
tests =
  testGroup
    "Test.Formats"
    [ testCase "the default options write an FST and its sidecar, and no spill file" $ do
        let name = "formats-default"
        scrub [name]
        _ <- writeWaveform defaultOptions (pathOf name) (0, 8) counter
        existing name ["fst", "json", "vcd", "spill"] >>= (@?= [True, True, False, False])
        -- An FST file starts with its header block, whose tag is 0.
        header <- ByteString.take 1 <$> ByteString.readFile (file name "fst")
        header @?= ByteString.pack [0]
    , testCase "a spill file on disk gives the waveform a spill in memory gives" $ do
        let (onDisk, inMemory) = ("formats-on-disk", "formats-in-memory")
        scrub [onDisk, inMemory]
        _ <- writeWaveform vcdOnDisk (pathOf onDisk) (0, 40) counter
        _ <- writeWaveform vcd (pathOf inMemory) (0, 40) counter
        a <- vcdBody (file onDisk "vcd")
        b <- vcdBody (file inMemory "vcd")
        a @?= b
        existing onDisk ["spill"] >>= (@?= [False])
    , testCase "an FST has the sidecar of the VCD" $ do
        let (v, f) = ("formats-sidecar-vcd", "formats-sidecar-fst")
        scrub [v, f]
        _ <- writeWaveform vcd (pathOf v) (0, 40) counter
        _ <- writeWaveform defaultOptions (pathOf f) (0, 40) counter
        a <- P.readFile (file v "json")
        b <- P.readFile (file f "json")
        assertBool "the counter is described" ("logic.count" `P.elem` P.words (P.map unquote a))
        a @?= b
    , -- The path is the caller's: relative or absolute, in directories that
      -- do not exist yet. The spill file goes beside the waveform.
      testCase "a waveform goes where its path says, absolute or in new directories" $ do
        root <- makeAbsolute (pathOf "formats-paths")
        removePathForcibly root
        let absolute = root </> "new" </> "counter"
        _ <- writeWaveform vcdOnDisk absolute (0, 8) counter
        P.mapM (doesFileExist . (absolute <.>)) ["vcd", "json", "spill"]
          >>= (@?= [True, True, False])
        let nested = pathOf "formats-paths" </> "a" </> "b" </> "counter"
        failed <-
          try @SomeException
            (withWaveform defaultOptions nested (0, 8) counter (\xs -> P.length xs @?= 9))
        assertBool "the check failed" (P.either (P.const True) (P.const False) failed)
        P.mapM (doesFileExist . (nested <.>)) ["fst", "json"] >>= (@?= [True, True])
        removePathForcibly root
    , testCase "withWaveform deletes an earlier failing run's FST when the test passes" $ do
        let name = "formats-stale-fst"
        scrub [name]
        failed <-
          try @SomeException
            (withWaveform defaultOptions (pathOf name) (0, 8) counter (\xs -> P.length xs @?= 9))
        assertBool "the check failed" (P.either (P.const True) (P.const False) failed)
        existing name ["fst", "json"] >>= (@?= [True, True])
        _ <- withWaveform defaultOptions (pathOf name) (0, 8) counter (\xs -> P.length xs @?= 8)
        existing name ["fst", "json"] >>= (@?= [False, False])
    , -- Telling a failure's waveform from one written on purpose reads the
      -- sidecar, never the waveform: libfst's reader crashes on a cut-off FST.
      testCase "withWaveform deletes a failing run's FST that is cut off when the test passes"
        $ assertChild "formats-cut-fst" []
    , testCase "withWaveform keeps an FST written on purpose when the test passes" $ do
        let name = "formats-requested-fst"
        scrub [name]
        _ <- writeWaveform defaultOptions (pathOf name) (0, 8) counter
        _ <- withWaveform defaultOptions (pathOf name) (0, 8) counter (\xs -> P.length xs @?= 8)
        existing name ["fst", "json"] >>= (@?= [True, True])
    , testCase "a cancelled run keeps its spill file, and recoverWaveform writes its waveform" $ do
        let name = "formats-cancelled"
        scrub [name]
        stopped <- timeout 500000 (writeWaveform vcdOnDisk (pathOf name) (0, 4000) slow)
        assertBool "the run was cancelled" (P.null stopped)
        existing name ["spill", "vcd"] >>= (@?= [True, False])
        recovered <- recoverWaveform vcd (pathOf name)
        absolute <- makeAbsolute (file name "vcd")
        recovered @?= Right absolute
        cycles <- timestamps (file name "vcd")
        assertBool "the recovered waveform has cycles" (cycles P.> 1)
        existing name ["spill"] >>= (@?= [False])
    , -- A cancelled run names its spill file only when there is something
      -- to recover from it; recoverWaveform refuses one without a cycle.
      testCase "a cancelled run that recorded nothing leaves no spill file, and says nothing" $ do
        (code, _, err) <- inChild "formats-stopped-empty" []
        code @?= ExitSuccess
        assertBool ("said: " <> err) (P.not ("was stopped" `isInfixOf` err))
        existing "formats-stopped-empty" ["spill"] >>= (@?= [False])
        (code', _, err') <- inChild "formats-stopped-recorded" []
        code' @?= ExitSuccess
        spillFile <- makeAbsolute (file "formats-stopped-recorded" "spill")
        assertBool
          ("said: " <> err')
          (("the run was stopped; what it recorded is in " <> spillFile) `isInfixOf` err')
        existing "formats-stopped-recorded" ["spill"] >>= (@?= [True])
        removeFile spillFile
    , testCase "a recording cancelled at any moment leaves no file open"
        $ assertChild "formats-cancel-storm" []
    , testCase "an FST that cannot be written whole is an error, and the spill file stays"
        $ assertChild "formats-fst-too-large" []
        >> removeFile (file "formats-fst-too-large" "spill")
    , testCase "reading a spill file does not keep its records in memory"
        $ assertChild "formats-read-big-spill" []
    , testCase "a cancelled run in memory leaves nothing" $ do
        let name = "formats-cancelled-in-memory"
        scrub [name]
        stopped <- timeout 500000 (writeWaveform defaultOptions (pathOf name) (0, 4000) slow)
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
      -- signal, up to the cycle it got to, also one that never changes.
      testCase "a killed run's spill file has every signal, up to the cycle it got to" $ do
        let name = "formats-killed"
        scrub [name]
        (out, process) <- startChild "formats-killed"
        line <- hGetLine out
        line @?= "recorded 3000 cycles"
        getPid process >>= P.maybe (assertFailure "the child has no pid") (signalProcess sigKILL)
        code <- waitForProcess process
        code @?= ExitFailure (-9)
        killed <- recoverRecording (file name "spill")
        case killed of
          Left err -> assertFailure err
          Right rec -> do
            -- The last forced cycle of a trace is recorded with the next one.
            recordingEnd rec @?= 2999
            written <- writeVCD (0, recordingEnd rec) rec (file name "vcd")
            written @?= Right ()
            body <- vcdBody (file name "vcd")
            let values = [l | l <- body, P.take 1 l P.== "b"]
            assertBool "both signals have values" (P.length values P.> 2)
            assertBool "no signal is z" (P.not (P.any ('z' `P.elem`) values))
            discardRecording rec
    , -- Without mappings, a killed run loses what it had not written yet:
      -- the newest records of a signal, and all of one that changes rarely.
      testCase "a killed run's spill file written without mappings lacks the unwritten records" $ do
        let name = "formats-killed-unmapped"
        scrub [name]
        (out, process) <- startChild name
        line <- hGetLine out
        line @?= "recorded 3000 cycles"
        getPid process >>= P.maybe (assertFailure "the child has no pid") (signalProcess sigKILL)
        _ <- waitForProcess process
        killed <- recoverRecording (file name "spill")
        case killed of
          Left err -> assertFailure err
          Right rec -> do
            -- "busy" wrote its first region (4 KiB, about 1300 cycles), not
            -- the one it was filling; "quiet" never filled its first.
            let end = recordingEnd rec
            assertBool ("recovered " <> P.show end <> " cycles") (end P.> 1000 P.&& end P.< 2999)
            changes <- P.mapM (recordedChanges rec) (recordedSignals rec)
            P.map P.null changes @?= [False, True]
            discardRecording rec
    , -- A cancelled run writes everything it recorded: also the progress of a
      -- signal that has not changed since its first cycle.
      testCase "a cancelled run's spill file has every signal up to the end" $ do
        let spillFile = file "formats-cancelled-quiet" "spill"
        scrub ["formats-cancelled-quiet"]
        stopped <- try @AsyncException $ withRecording (Just spillFile) $ do
          _ <- evaluate (P.sum (P.map P.toInteger (sampleN 6000 busyAndQuiet)))
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
    , -- A full disk stops the recording, not the run: the recording keeps
      -- what it has, and says so.
      testCase "a spill file that cannot grow stops the recording, and the run goes on" $ do
        (code, _, err) <- inChild "formats-full-disk" []
        code @?= ExitSuccess
        assertBool ("said: " <> err) ("The spill file cannot grow" `isInfixOf` err)
        assertBool ("said: " <> err) ("File too large" `isInfixOf` err)
    , testCase "a spill file written without mappings that cannot grow stops the recording too" $ do
        (code, _, err) <- inChild "formats-full-disk-unmapped" []
        code @?= ExitSuccess
        assertBool ("said: " <> err) ("The spill file cannot grow" `isInfixOf` err)
    , -- Where regions cannot be mapped, they are buffers: the same waveform.
      testCase "a spill file written without mappings gives the same waveform"
        $ assertChild "formats-unmapped" []
    , -- A full region is unmapped: a signal holds one mapping, however long
      -- it records, and a closed recording none.
      testCase "a spill file maps one region per signal, and none once closed" $ do
        let name = "formats-mappings"
        scrub [name]
        createDirectoryIfMissing True "waveforms"
        spillFile <- (</> (name <.> "spill")) <$> canonicalizePath "waveforms"
        let mappings =
              P.readFile "/proc/self/maps"
                >>= evaluate
                . P.length
                . P.filter (spillFile `isSuffixOf`)
                . P.lines
        -- No environment variable turns mapping off.
        setEnv "CCC_SPILL_NO_MMAP" "1"
        (r, rec) <- withRecording (Just spillFile) $ do
          -- About 600 KiB of records: regions of a page, two pages, and then
          -- (at 4 KiB pages) 16 KiB each.
          _ <- evaluate (P.sum (P.map P.toInteger (sampleN 200000 counter)))
          mappings
        unsetEnv "CCC_SPILL_NO_MMAP"
        -- The counter's region, and the metadata's.
        P.either (Left . P.show) Right r @?= Right 2
        mappings >>= (@?= 0)
        -- A region grows to 16 KiB, or a page where a page is larger.
        contents <- ByteString.readFile spillFile
        let sizes = [size | (_, size, owner) <- regionsOf contents, owner P.== 1]
        P.maximum sizes @?= P.max 16384 (pageOf contents)
        P.take 1 sizes @?= [pageOf contents]
        discardRecording rec
    , -- Between storing a signal's newest record and its progress, a killed
      -- run's spill file has the record but not yet the progress.
      testCase "a signal's newest record counts when its progress lags" $ do
        let name = "formats-lagging"
        scrub [name, "formats-lagging-whole"]
        (_, rec) <-
          withRecording
            (Just (file name "spill"))
            (evaluate (P.sum (P.map P.toInteger (sampleN 20000 counter))))
        _ <- writeVCD (0, recordingEnd rec) rec (file "formats-lagging-whole" "vcd")
        -- The progress of the counter's last region: back to 0.
        contents <- ByteString.readFile (file name "spill")
        let lastOfCounter = P.last [off | (off, _, owner) <- regionsOf contents, owner P.== 1]
            progressAt = lastOfCounter P.+ 24
            lagging =
              ByteString.take progressAt contents
                <> ByteString.replicate 8 0
                <> ByteString.drop (progressAt P.+ 8) contents
        ByteString.writeFile (file name "spill") lagging
        recovered <- recoverRecording (file name "spill")
        case recovered of
          Left err -> assertFailure err
          Right rec' -> do
            _ <- writeVCD (0, recordingEnd rec') rec' (file name "vcd")
            a <- vcdBody (file "formats-lagging-whole" "vcd")
            b <- vcdBody (file name "vcd")
            a @?= b
        discardRecording rec
    , testCase "a long recording on disk keeps its memory flat"
        $ assertChild "formats-flat-memory" []
    , -- A waveform that cannot be written must not cost the recording: the
      -- spill file stays, for recoverWaveform.
      testCase "a spill file stays when its waveform cannot be written" $ do
        let name = "formats-blocked"
        scrub [name]
        -- A directory where the waveform should go makes the write fail.
        createDirectoryIfMissing True (file name "vcd")
        _ <- writeWaveform vcdOnDisk (pathOf name) (0, 8) counter
        existing name ["spill"] >>= (@?= [True])
        removeDirectory (file name "vcd")
        recovered <- recoverWaveform vcd (pathOf name)
        assertBool "recovered" (P.either (P.const False) (P.const True) recovered)
        existing name ["vcd", "spill"] >>= (@?= [True, False])
    , -- A waveform that cannot be put in place (a directory has its name)
      -- leaves no sidecar either, and the sidecar of an earlier waveform of
      -- the same path (here: of the other format) as it was.
      testCase "writeVCD and writeFST write no sidecar when the waveform cannot land" $ do
        let name = "formats-no-landing"
        scrub [name]
        (_, rec) <-
          withRecording Nothing (evaluate (P.sum (P.map P.toInteger (sampleN 8 counter))))
        (_, other) <-
          withRecording Nothing (evaluate (P.sum (P.map P.toInteger (sampleN 8 busyAndQuiet))))
        let blocked ext write earlier = do
              createDirectoryIfMissing True (file name ext)
              refused <- write (0, 8) rec (file name ext)
              assertBool (ext <> " refused") (P.either (P.const True) (P.const False) refused)
              existing name ["json"] >>= (@?= [False])
              -- The other recording's waveform, of the other format, and its sidecar.
              earlier (0, 8) other (file name (if ext P.== "vcd" then "fst" else "vcd"))
                >>= (@?= Right ())
              sidecar <- ByteString.readFile (file name "json")
              refused' <- write (0, 8) rec (file name ext)
              assertBool (ext <> " refused") (P.either (P.const True) (P.const False) refused')
              ByteString.readFile (file name "json") >>= (@?= sidecar)
              removeDirectory (file name ext)
              scrub [name]
              -- No temporary file is left behind.
              left <- P.filter (name `isPrefixOf`) <$> listDirectory "waveforms"
              left @?= []
        blocked "vcd" writeVCD writeFST
        blocked "fst" writeFST writeVCD
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
    , testCase "a spill file cut off in a region is read up to its last whole region" $ do
        let
          spillFile = file "formats-cut" "spill"
          cutFile = file "formats-cut-short" "spill"
        scrub ["formats-cut", "formats-cut-short", "formats-cut-whole", "formats-cut-read"]
        -- Enough cycles for several regions of records.
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
        -- Cut off in the middle: the counter's records of its whole regions.
        size <- getFileSize spillFile
        ByteString.readFile spillFile
          >>= ByteString.writeFile cutFile
          . ByteString.take (P.fromIntegral size `P.div` 2)
        cut <- recoverRecording cutFile
        case cut of
          Left err -> assertFailure err
          Right rec' -> do
            let changes r' = P.concat <$> P.mapM (recordedChanges r') (recordedSignals r')
            wholeChanges <- changes rec
            cutChanges <- changes rec'
            assertBool
              "fewer changes than the whole run"
              (P.length cutChanges P.< P.length wholeChanges)
            assertBool "but some" (P.length cutChanges P.> 0)
            cutChanges @?= P.take (P.length cutChanges) wholeChanges
            written <- writeVCD (0, recordingEnd rec') rec' (file "formats-cut-short" "vcd")
            written @?= Right ()
        -- Followed by a region that claims more bytes than the file has, as
        -- one that was cut off would: read up to it, without trying to.
        let garbledFile = file "formats-cut-garbled" "spill"
        ByteString.readFile spillFile
          >>= ByteString.writeFile garbledFile
          . ( <>
                ByteStringLazy.toStrict
                  (Builder.toLazyByteString (P.foldMap Builder.word64LE [65536, 1, 65504, 0]))
            )
        garbled <- recoverRecording garbledFile
        P.fmap recordingEnd garbled @?= Right (recordingEnd rec)
        discardRecording rec
        P.mapM_ removeFile [cutFile, garbledFile]
    ]
 where
  unquote c = if c P.== '"' then ' ' else c
