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
share: where a waveform goes, how a recording becomes the pair of files a
viewer reads, and how a signal is simulated with or without recording.

== Memory

A recording's values go to a spill file, @waveforms\/\<name\>.spill@, while
the simulation runs, and are read from there when the files are written; the
spill file is deleted after. So a recording needs about the same memory for
a long run as for a short one. Nothing here serializes tests either, so a
parallel runner is free to use every core.
-}
module Clash.CircuitContext.Internal.Waveform (
  waveformPath,
  checkWindow,
  domainPeriod,
  simulate,
  record,
  Origin (..),
  save,
  discard,
  removeFailureWaveform,
  announce,
) where

import Control.Exception (
  ErrorCall (..),
  IOException,
  SomeException,
  evaluate,
  throwIO,
  try,
 )
import Control.Monad (when)
import System.Directory (
  createDirectoryIfMissing,
  makeAbsolute,
  removePathForcibly,
 )
import System.FilePath ((-<.>), (<.>), (</>))
import System.IO (IOMode (ReadMode), hGetLine, hPutStrLn, stderr, withFile)

import Clash.Explicit.Signal (Signal, sampleN)
import Clash.Prelude (KnownDomain, NFDataX, deepseqX, snatToNum)
import Clash.Signal.Internal (SDomainConfiguration (..), knownDomain)

import Clash.CircuitContext.Core (
  HasCircuitContext,
  withRecordingFrom,
  withoutCircuitContext,
 )
import Clash.CircuitContext.Internal.Recorder (Recording (..), ticks, timescale)
import Clash.CircuitContext.Internal.Vcd (writeFiles)

{- | The @.vcd@ path of a waveform name: @waveforms\/\<name\>.vcd@, relative to
the process's WORKING DIRECTORY. That is not the same place under every way
of running a suite (@cabal test@ runs in the package directory, @cabal run@
where it was invoked), which is why a failure's waveform is announced with
its absolute path.
-}
waveformPath :: String -> FilePath
waveformPath name = "waveforms" </> name <.> "vcd"

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

{- | @n@ samples of the signal, forced, without recording: every recorder is
the identity under 'withoutCircuitContext', so nothing registers and nothing
accumulates.
-}
simulate :: (NFDataX a) => Int -> ((HasCircuitContext) => Signal dom a) -> IO [a]
simulate n sig = evaluate (let xs = withoutCircuitContext (sampleN n sig) in xs `deepseqX` xs)

{- | Run an action under a fresh recording context that records from this
time (ps) on, into the spill file of this waveform name. Returns what the
action returned or threw, and the recording; 'save' deletes the spill file.
-}
record ::
  String -> Int -> ((HasCircuitContext) => IO r) -> IO (Either SomeException r, Recording)
record name start k = do
  createDirectoryIfMissing True "waveforms"
  withRecordingFrom start (waveformPath name -<.> "spill") k

{- | Why a waveform is written. A 'Failure' waveform is marked in its VCD's
@$comment@ line, so that a later passing run can tell it from one written on
purpose ('removeFailureWaveform').
-}
data Origin = Requested | Failure
  deriving (Eq)

-- | The @$comment@ text of a 'Failure' waveform.
failureComment :: String
failureComment = "clash-circuit-context: the run of a failing test"

{- | Write a recording as @waveforms\/\<name\>.vcd@ and its @.json@ sidecar,
over the window @[start, stop)@ in ps, and delete its spill file. 'False',
and nothing written, when the run recorded nothing or the files could not be
written; an earlier file is then left alone.
-}
save :: Origin -> String -> (Int, Int) -> Recording -> IO Bool
save origin name (start, stop) rec = do
  written <-
    if recordingEndPs rec == 0
      then pure (Right (Left "nothing was evaluated"))
      else
        try
          (writeFiles comment (start `div` timescale rec, ticks rec stop) rec (waveformPath name))
  discard rec
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

-- | Delete a recording's spill file, without writing a waveform.
discard :: Recording -> IO ()
discard = removePathForcibly . recordingSpill

{- | Delete @waveforms\/\<name\>.vcd@ and its @.json@ if the VCD is the
waveform of a failing run ('Failure'). A waveform written on purpose (by
@writeWaveform@) is kept, also when another test of the same name passes.
-}
removeFailureWaveform :: String -> IO ()
removeFailureWaveform name = do
  -- Only the header: the VCD can be gigabytes.
  header <-
    try
      (withFile (waveformPath name) ReadMode (\h -> mapM (const (hGetLine h)) [1 :: Int .. 3]))
  case header of
    Right ls
      | ("$comment " <> failureComment <> " $end") `elem` ls ->
          mapM_ removePathForcibly [waveformPath name, waveformPath name -<.> "json"]
    Right _ -> pure ()
    Left (_ :: IOException) -> pure ()

{- | Say on stderr where a failure's waveform landed, as an ABSOLUTE path: the
directory is relative to the working directory, which differs between ways of
running a suite.
-}
announce :: String -> IO ()
announce name = do
  path <- makeAbsolute (waveformPath name)
  hPutStrLn stderr ("clash-circuit-context: wrote " <> path)
