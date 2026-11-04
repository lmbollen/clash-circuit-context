{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

What "Clash.CircuitContext.Waveform" and "Clash.CircuitContext.Waveform.Hedgehog"
share: the options, where a waveform goes, how a recording becomes the pair
of files a viewer reads, and how a signal is simulated with or without
recording.

== Memory

A recording keeps its values as compact records ('Spill'): in memory, or in
a spill file, @waveforms\/\<name\>.spill@, which is deleted when the
waveform is written. With a spill file, a recording needs about the same
memory for a long run as for a short one. Nothing here serializes tests
either, so a parallel runner is free to use every core.
-}
module Clash.CircuitContext.Internal.Waveform (
  WaveformOptions (..),
  Format (..),
  Spill (..),
  defaultOptions,
  waveformPath,
  spillPath,
  checkWindow,
  domainPeriod,
  psWindow,
  simulate,
  record,
  Origin (..),
  save,
  discard,
  removeFailureWaveform,
  announce,
  recover,
) where

import Control.Exception (
  ErrorCall (..),
  IOException,
  SomeException,
  evaluate,
  onException,
  throwIO,
  try,
 )
import Control.Monad (filterM, when)
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
  makeAbsolute,
  removePathForcibly,
 )
import System.FilePath ((<.>), (</>))
import System.IO (IOMode (ReadMode), hGetLine, hPutStrLn, stderr, withFile)

import Clash.Explicit.Signal (Signal, sampleN)
import Clash.Prelude (KnownDomain, NFDataX, deepseqX, snatToNum)
import Clash.Signal.Internal (SDomainConfiguration (..), knownDomain)

import Clash.CircuitContext.Core (
  HasCircuitContext,
  withRecordingFrom,
  withoutCircuitContext,
 )
import qualified Clash.CircuitContext.Internal.Fst as Fst
import Clash.CircuitContext.Internal.FstOut (writeFst)
import Clash.CircuitContext.Internal.Recorder (
  Recording (..),
  readSpill,
  ticks,
  timescale,
 )
import Clash.CircuitContext.Internal.Vcd (writeFiles)

-- | How a waveform is recorded and written.
data WaveformOptions = WaveformOptions
  { format :: Format
  -- ^ The file the waveform is written as.
  , spill :: Spill
  -- ^ Where the recording keeps its values until then.
  }
  deriving (Eq, Show)

{- | The file a waveform is written as, with its @.json@ sidecar beside it.
Both GTKWave and Surfer read either.
-}
data Format
  = -- | @waveforms\/\<name\>.vcd@: text, readable by any waveform viewer.
    VCD
  | {- | @waveforms\/\<name\>.fst@: compressed, often tens of times smaller
    than the VCD, and as fast to write.
    -}
    FST
  deriving (Eq, Show)

-- | Where a recording keeps its values until the waveform is written.
data Spill
  = {- | In memory, as compact records: nothing is written to disk but the
    waveform. A recording that is stopped (cancelled, or killed) leaves
    nothing.
    -}
    InMemory
  | {- | In @waveforms\/\<name\>.spill@, deleted when the waveform is
    written: memory does not grow with the length of the run, and a recording
    that is stopped leaves the file, holding what it recorded (a run of
    @writeWaveform@; @withWaveform@ and @simulateWaveform@ record only when
    they run a failed case again). Write its waveform
    with @recoverWaveform@, or from the command line with the package's
    executables @spill2fst@ and @spill2vcd@. The values are written twice: to
    the spill file, and to the waveform.
    -}
    OnDisk
  deriving (Eq, Show)

-- | An FST waveform, recorded in memory.
defaultOptions :: WaveformOptions
defaultOptions = WaveformOptions{format = FST, spill = InMemory}

{- | The path of a waveform name in this format: @waveforms\/\<name\>.vcd@
or @.fst@, relative to the process's WORKING DIRECTORY. That is not the same
place under every way of running a suite (@cabal test@ runs in the package
directory, @cabal run@ where it was invoked), which is why a failure's
waveform is announced with its absolute path.
-}
waveformPath :: Format -> String -> FilePath
waveformPath fmt name = "waveforms" </> name <.> extension fmt

extension :: Format -> String
extension VCD = "vcd"
extension FST = "fst"

-- | The spill file of a waveform name: @waveforms\/\<name\>.spill@.
spillPath :: String -> FilePath
spillPath name = "waveforms" </> name <.> "spill"

-- | Throw, as 'Clash.Signal.Trace.dumpVCD' does, unless @0 <= start <= stop@.
checkWindow :: String -> (Int, Int) -> IO ()
checkWindow name (start, stop) =
  when (start < 0 || stop < start) $
    throwIO
      ( ErrorCall
          ( "clash-circuit-context: "
              <> name
              <> ": the window "
              <> show (start, stop)
              <> " is not a (start, stop) with 0 <= start <= stop"
          )
      )

-- | The clock period of a domain, in ps.
domainPeriod :: forall dom. (KnownDomain dom) => Int
domainPeriod = case knownDomain @dom of
  SDomainConfiguration{sPeriod} -> snatToNum sPeriod

{- | A window of cycles of a domain, in ps. Saturates at 'maxBound', so a
window that ends at @maxBound@ cycles (\"as long as the check runs\") stays
the end of time instead of overflowing.
-}
psWindow :: forall dom. (KnownDomain dom) => (Int, Int) -> (Int, Int)
psWindow (start, stop) = (toPs start, toPs stop)
 where
  period = domainPeriod @dom
  toPs c
    | c > maxBound `div` period = maxBound
    | otherwise = c * period

{- | @n@ samples of the signal, forced, without recording: every recorder is
the identity under 'withoutCircuitContext', so nothing registers and nothing
accumulates.
-}
simulate :: (NFDataX a) => Int -> ((HasCircuitContext) => Signal dom a) -> IO [a]
simulate n sig = evaluate (let xs = withoutCircuitContext (sampleN n sig) in xs `deepseqX` xs)

{- | Run an action under a fresh recording context that records from this
time (ps) on, keeping its values as the options say. Returns what the action
returned or threw, and the recording; 'save' deletes a spill file. When the
action is cancelled with a spill file on disk, says on stderr where it is.
-}
record ::
  WaveformOptions ->
  String ->
  Int ->
  ((HasCircuitContext) => IO r) ->
  IO (Either SomeException r, Recording)
record opts name start k = do
  createDirectoryIfMissing True "waveforms"
  withRecordingFrom start spillFile k `onException` kept
 where
  spillFile = if spill opts == OnDisk then Just (spillPath name) else Nothing
  -- Only a cancellation gets here: what the action throws is returned.
  kept = case spillFile of
    Nothing -> pure ()
    Just f -> do
      there <- doesFileExist f
      when there $ do
        path <- makeAbsolute f
        hPutStrLn
          stderr
          ( "clash-circuit-context: "
              <> name
              <> ": the run was stopped; what it recorded is in "
              <> path
              <> ". Write its waveform with recoverWaveform, or with spill2fst or"
              <> " spill2vcd (executables of clash-circuit-context)"
          )

{- | Why a waveform is written. A 'Failure' waveform is marked in its
comment (a VCD's @$comment@ line, an FST's comment), so that a later passing
run can tell it from one written on purpose ('removeFailureWaveform').
-}
data Origin = Requested | Failure
  deriving (Eq)

-- | The comment of a 'Failure' waveform.
failureComment :: String
failureComment = "clash-circuit-context: the run of a failing test"

{- | Write a recording as the options' waveform, @waveforms\/\<name\>.vcd@ or
@.fst@ and its @.json@ sidecar, over the window @[start, stop)@ in ps, and
delete its spill file. 'False', and nothing written, when the run recorded
nothing or the files could not be written; an earlier file is then left
alone.
-}
save :: Origin -> WaveformOptions -> String -> (Int, Int) -> Recording -> IO Bool
save origin opts name window rec = do
  written <- writeWaveformFiles origin opts name window rec
  discard rec
  pure written

-- | 'save', but keep the spill file.
writeWaveformFiles ::
  Origin -> WaveformOptions -> String -> (Int, Int) -> Recording -> IO Bool
writeWaveformFiles origin opts name (start, stop) rec = do
  written <-
    if recordingEndPs rec == 0
      then pure (Right (Left "nothing was evaluated"))
      else try (writer comment window rec (waveformPath (format opts) name))
  case written of
    Right (Right ()) -> pure True
    Right (Left _) -> pure False
    Left (err :: IOException) -> do
      hPutStrLn
        stderr
        ("clash-circuit-context: " <> name <> ": no waveform written: " <> show err)
      pure False
 where
  comment = if origin == Failure then failureComment else "No comment"
  window = (start `div` timescale rec, ticks rec stop)
  writer = case format opts of
    VCD -> writeFiles
    FST -> writeFst

-- | Delete a recording's spill file, if it has one, without writing a waveform.
discard :: Recording -> IO ()
discard = mapM_ removePathForcibly . recordingSpill

{- | Delete @waveforms\/\<name\>.vcd@ or @.fst@, and its @.json@, if it is
the waveform of a failing run ('Failure'). A waveform written on purpose (by
@writeWaveform@) is kept, also when another test of the same name passes.
-}
removeFailureWaveform :: String -> IO ()
removeFailureWaveform name = do
  failed <- filterM isFailure [VCD, FST]
  mapM_ (removePathForcibly . (`waveformPath` name)) failed
  -- The sidecar goes with the last waveform of its name.
  left <- filterM (doesFileExist . (`waveformPath` name)) [VCD, FST]
  when (not (null failed) && null left) $
    removePathForcibly ("waveforms" </> name <.> "json")
 where
  isFailure VCD = do
    -- Only the header: the VCD can be gigabytes.
    header <-
      try
        ( withFile
            (waveformPath VCD name)
            ReadMode
            (\h -> mapM (const (hGetLine h)) [1 :: Int .. 3])
        )
    pure $ case header of
      Right ls -> ("$comment " <> failureComment <> " $end") `elem` ls
      Left (_ :: IOException) -> False
  isFailure FST = do
    there <- doesFileExist (waveformPath FST name)
    if there
      then (== Just True) <$> Fst.hasComment (waveformPath FST name) failureComment
      else pure False

{- | Say on stderr where a failure's waveform landed, as an ABSOLUTE path: the
directory is relative to the working directory, which differs between ways of
running a suite.
-}
announce :: Format -> String -> IO ()
announce fmt name = do
  path <- makeAbsolute (waveformPath fmt name)
  hPutStrLn stderr ("clash-circuit-context: wrote " <> path)

{- | Write the waveform of a run that was stopped, from its spill file
@waveforms\/\<name\>.spill@, in the options' format: everything it recorded.
Deletes the spill file once the waveform is written, and keeps it otherwise. Returns the waveform's
path, or why there is none.
-}
recover :: WaveformOptions -> String -> IO (Either String FilePath)
recover opts name = do
  there <- doesFileExist (spillPath name)
  if not there
    then pure (Left (spillPath name <> " does not exist"))
    else do
      read' <- readSpill (spillPath name)
      case read' of
        Left err -> pure (Left err)
        Right rec -> do
          written <-
            writeWaveformFiles Requested opts name (recordingStartPs rec, recordingEndPs rec) rec
          when written (discard rec)
          pure $
            if written
              then Right (waveformPath (format opts) name)
              else Left (spillPath name <> " holds no recorded cycle")
