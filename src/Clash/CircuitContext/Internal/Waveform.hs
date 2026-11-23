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
a spill file, @\<path\>.spill@, which is deleted when the waveform is
written. With a spill file, a recording needs about the same
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
  SomeException,
  evaluate,
  throwIO,
 )
import Control.Monad (filterM, when)
import System.Directory (
  doesFileExist,
  makeAbsolute,
  removePathForcibly,
 )
import System.FilePath ((<.>))
import System.IO (hPutStrLn, stderr)

import Clash.Explicit.Signal (Signal, sampleN)
import Clash.Prelude (KnownDomain, NFDataX, deepseqX, snatToNum)
import Clash.Signal.Internal (SDomainConfiguration (..), knownDomain)

import Clash.CircuitContext.Core (
  HasCircuitContext,
  withRecordingFrom,
  withoutCircuitContext,
 )
import Clash.CircuitContext.Internal.FstOut (writeFst)
import Clash.CircuitContext.Internal.Recorder (
  Recording (..),
  holdsCycles,
  readSpill,
  ticks,
  timescale,
 )
import Clash.CircuitContext.Internal.Sidecar (Origin (..), failureWaveform)
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
  = -- | @\<path\>.vcd@: text, readable by any waveform viewer.
    VCD
  | {- | @\<path\>.fst@: compressed, often tens of times smaller than the
    VCD, and as fast to write.
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
  | {- | In @\<path\>.spill@, beside the waveform, deleted when the waveform
    is written: memory does not grow with the length of the run, and a recording
    that is stopped, or whose waveform cannot be written, leaves the file,
    holding what it recorded, when it recorded a cycle (a run of
    @writeWaveform@; @withWaveform@ and @simulateWaveform@ record only when
    they run a failed case again). Write its waveform
    with @recoverWaveform@, or from the command line with the package's
    executables @spill2fst@ and @spill2vcd@. The values are written twice: to
    the spill file, and to the waveform.

    A value is in the file as soon as it is recorded, also when the process is
    killed right after. The file's disk space is reserved ahead, up to 64 KiB
    per signal, so a full disk stops the recording (not the run) with a
    message, and the waveform ends where the recording stopped.
    -}
    OnDisk
  deriving (Eq, Show)

-- | An FST waveform, recorded in memory.
defaultOptions :: WaveformOptions
defaultOptions = WaveformOptions{format = FST, spill = InMemory}

{- | The file of a waveform in this format: its path, which has no
extension, with @.vcd@ or @.fst@ added. A relative path is relative to the
process's WORKING DIRECTORY. That is not the same place under every way of
running a suite (@cabal test@ runs in the package directory, @cabal run@
where it was invoked), which is why a failure's waveform is announced with
its absolute path.
-}
waveformPath :: Format -> FilePath -> FilePath
waveformPath fmt path = path <.> extension fmt

extension :: Format -> String
extension VCD = "vcd"
extension FST = "fst"

-- | The spill file of a waveform's path: @\<path\>.spill@.
spillPath :: FilePath -> FilePath
spillPath path = path <.> "spill"

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
returned or threw, and the recording; 'save' deletes a spill file. A
cancelled run keeps its spill file, and says so on stderr (see
'Clash.CircuitContext.Core.withRecording').
-}
record ::
  WaveformOptions ->
  FilePath ->
  Int ->
  ((HasCircuitContext) => IO r) ->
  IO (Either SomeException r, Recording)
record opts path start =
  withRecordingFrom start (if spill opts == OnDisk then Just (spillPath path) else Nothing)

{- | Write a recording as the options' waveform, @\<path\>.vcd@ or @.fst@ and
its @.json@ sidecar (making missing directories), over the window
@[start, stop)@ in ps, and then delete its spill file. Returns the
waveform's ABSOLUTE path: a relative path is relative to the working
directory, which differs between ways of running a suite. 'Left', with the
reason, and nothing written, when the run recorded nothing in the window or
the files could not be written; an earlier file is then left alone. The
spill file of a run that recorded cycles is then kept, for 'recover', and
the reason says where it is.
-}
save ::
  Origin ->
  WaveformOptions ->
  FilePath ->
  (Int, Int) ->
  Recording ->
  IO (Either String FilePath)
save origin opts path window rec = do
  written <- writeWaveformFiles origin opts path window rec
  case (written, recordingSpill rec) of
    (Left err, Just spillFile) | holdsCycles rec -> do
      kept <- makeAbsolute spillFile
      pure (Left (err <> "; what the run recorded is kept in " <> kept))
    _ -> discard rec >> pure written

-- | 'save', but keep the spill file.
writeWaveformFiles ::
  Origin ->
  WaveformOptions ->
  FilePath ->
  (Int, Int) ->
  Recording ->
  IO (Either String FilePath)
writeWaveformFiles origin opts path (start, stop) rec = do
  written <- writer origin window rec (waveformPath (format opts) path)
  traverse (const (makeAbsolute (waveformPath (format opts) path))) written
 where
  window = (start `div` timescale rec, ticks rec stop)
  writer = case format opts of
    VCD -> writeFiles
    FST -> writeFst

-- | Delete a recording's spill file, if it has one, without writing a waveform.
discard :: Recording -> IO ()
discard = mapM_ removePathForcibly . recordingSpill

{- | Delete @\<path\>.vcd@ or @.fst@, and its @.json@, if it is the waveform
of a failing run ('Failure'), as its sidecar says. A waveform written on
purpose (by @writeWaveform@) is kept, also when another test with the same
path passes. Only the sidecar is read, not the waveform.
-}
removeFailureWaveform :: FilePath -> IO ()
removeFailureWaveform path = do
  failed <- failureWaveform (path <.> "json")
  case [fmt | fmt <- [VCD, FST], Just (extension fmt) == failed] of
    [] -> pure ()
    fmt : _ -> do
      removePathForcibly (waveformPath fmt path)
      -- The sidecar goes with the last waveform of its path.
      left <- filterM (doesFileExist . (`waveformPath` path)) [VCD, FST]
      when (null left) (removePathForcibly (path <.> "json"))

{- | Say on stderr what became of the waveform of this path: where it
landed (as the absolute path 'save' gives), or why there is none.
-}
announce :: FilePath -> Either String FilePath -> IO ()
announce path written =
  hPutStrLn stderr $ case written of
    Right absolute -> "clash-circuit-context: wrote " <> absolute
    Left err -> "clash-circuit-context: " <> path <> ": no waveform written: " <> err

{- | Write the waveform of a run that was stopped, from its spill file
@\<path\>.spill@, in the options' format: everything it recorded. Deletes
the spill file once the waveform is written, and keeps it otherwise.
Returns the waveform's absolute path, or why there is none.
-}
recover :: WaveformOptions -> FilePath -> IO (Either String FilePath)
recover opts path = do
  spillFile <- makeAbsolute (spillPath path)
  there <- doesFileExist spillFile
  if not there
    then pure (Left (spillFile <> " does not exist"))
    else do
      read' <- readSpill spillFile
      case read' of
        Left err -> pure (Left err)
        Right rec
          | not (holdsCycles rec) ->
              pure (Left (spillFile <> " holds no recorded cycle"))
          | otherwise -> do
              written <-
                writeWaveformFiles Requested opts path (recordingStartPs rec, recordingEndPs rec) rec
              mapM_ (const (discard rec)) written
              pure written
