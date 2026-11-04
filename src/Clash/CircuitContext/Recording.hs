{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Recording a simulation by hand: for what "Clash.CircuitContext.Waveform"
does not cover, such as a design with outputs in more than one domain.

> (result, rec) <- withRecording (Just "waveforms/top.spill") $ do
>   let (a, b) = top clkA clkB
>   evaluate (deepseqX (sampleN 100 a, sampleN 50 b) ())
> _ <- writeFST (0, recordingEnd rec) rec "waveforms/top.fst"
> discardRecording rec

A recording keeps its values as compact records: in a spill file at the path
given to 'withRecording', so a long simulation does not need more memory than
a short one; or, given 'Nothing', in memory. A spill file describes itself:
when a run is stopped (cancelled, or the process killed), the file holds what
the run recorded, and 'recoverRecording' reads it. 'writeSpill' writes a spill
file's waveform in one step; the package's executables @spill2vcd@ and
@spill2fst@ do the same from the command line.
-}
module Clash.CircuitContext.Recording (
  -- * Recording a simulation
  withRecording,
  Recording,
  recordingStart,
  recordingEnd,
  discardRecording,

  -- * Writing the waveform
  writeVCD,
  writeFST,

  -- * A spill file
  recoverRecording,
  writeSpill,
  Format (..),

  -- * What was recorded
  RecordedSignal,
  Kind (..),
  recordedSignals,
  signalKey,
  signalKind,
  signalPeriod,
  signalWidth,
  signalDescribed,
  recordedChanges,
) where

import Control.Exception (evaluate)
import Data.Maybe (fromMaybe)
import GHC.Natural (Natural)
import System.Directory (removePathForcibly)

import Clash.CircuitContext.Core (withRecording)
import Clash.CircuitContext.Internal.FstOut (writeFst)
import Clash.CircuitContext.Internal.Recorder (
  Change (..),
  Description (..),
  Kind (..),
  RecordedSignal (..),
  Recording (..),
  readChanges,
  readSpill,
  ticks,
  timescale,
  withReader,
 )
import Clash.CircuitContext.Internal.Vcd (writeFiles)
import Clash.CircuitContext.Internal.Waveform (Format (..))

{- | The time from which the recording has cycles, in units of the timescale
(see 'recordingEnd'): the start of the window it was recorded with.
-}
recordingStart :: Recording -> Int
recordingStart rec = recordingStartPs rec `div` timescale rec

{- | The time at which the simulation stopped, in units of the VCD timescale
(the greatest common divisor of the recorded signals' periods). For a design
in one domain: the number of cycles the simulation evaluated.
-}
recordingEnd :: Recording -> Int
recordingEnd rec = ticks rec (recordingEndPs rec)

{- | Delete the recording's spill file, if it has one. The recording cannot be
used after this.
-}
discardRecording :: Recording -> IO ()
discardRecording = mapM_ removePathForcibly . recordingSpill

{- | Read the spill file at this path as a recording: also the file of a run
that was stopped, with what its signals had recorded up to their last whole
chunks of values. 'Left' when it is not a spill file.
-}
recoverRecording :: FilePath -> IO (Either String Recording)
recoverRecording = readSpill

{- | Write the waveform of the window @[start, stop)@: the VCD at this path,
and beside it the @.json@ sidecar a typed-waveform viewer reads. As in
'Clash.Signal.Trace.dumpVCD', the window is in units of the timescale
(for a design in one domain: cycles), and @start@ is the first value shown.
The window ends at 'recordingEnd' at the latest: what was not simulated
cannot be shown.

'Left', and no file written, when nothing was recorded, or when a name has a
character VCD does not allow.
-}
writeVCD :: (Int, Int) -> Recording -> FilePath -> IO (Either String ())
writeVCD = writeFiles "No comment"

{- | 'writeVCD', as an FST at this path: the same signals, names, scopes and
window, with times in ps. Compressed, so much smaller than the VCD.

'Left', and no file written, when nothing was recorded.
-}
writeFST :: (Int, Int) -> Recording -> FilePath -> IO (Either String ())
writeFST = writeFst "No comment"

-- | The signals recorded, in the order they were registered.
recordedSignals :: Recording -> [RecordedSignal]
recordedSignals = recordingSignals

{- | Does the signal have a @clash-shockwaves@ description of its payload
type, which a typed-waveform viewer shows as constructors and fields?
-}
signalDescribed :: RecordedSignal -> Bool
signalDescribed s = case signalDescription s of
  Described{} -> True
  _ -> False

{- | A signal's recorded changes: from this cycle on, the signal has this
@(mask, value)@, where a set mask bit is an undefined bit. 'Nothing' means
not evaluated, as it is before the first change. Reads the whole history
into memory: for tests, not for long runs.
-}
recordedChanges :: Recording -> RecordedSignal -> IO [(Int, Maybe (Natural, Natural))]
recordedChanges rec s = withReader rec $ \h -> do
  changes <- readChanges h s
  _ <- evaluate (length changes)
  pure [(c, v) | Change c v <- changes]

{- | Write the waveform of the spill file at the first path, as this format,
at the second path: over this window, in units of the timescale (see
'writeVCD'), or, given 'Nothing', everything the run recorded. Also for the
spill file of a run that was stopped. The spill file is kept.

'Left', and no file written, when the file is not a spill file or holds no
recorded cycle.
-}
writeSpill :: Format -> Maybe (Int, Int) -> FilePath -> FilePath -> IO (Either String ())
writeSpill fmt window spill out = do
  read' <- readSpill spill
  case read' of
    Left err -> pure (Left err)
    Right rec ->
      writer (fromMaybe (recordingStart rec, recordingEnd rec) window) rec out
 where
  writer = case fmt of
    VCD -> writeVCD
    FST -> writeFST
