{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Recording a simulation by hand: for what "Clash.CircuitContext.Waveform"
does not cover, such as a design with outputs in more than one domain.

> (result, rec) <- withRecording "waveforms/top.spill" $ do
>   let (a, b) = top clkA clkB
>   evaluate (deepseqX (sampleN 100 a, sampleN 50 b) ())
> _ <- writeVCD (0, recordingEnd rec) rec "waveforms/top.vcd"
> discardRecording rec

A recording's values are in a spill file while it exists, not in memory, so
a long simulation does not need more memory than a short one.
-}
module Clash.CircuitContext.Recording (
  -- * Recording a simulation
  withRecording,
  Recording,
  recordingEnd,
  discardRecording,

  -- * Writing the waveform
  writeVCD,

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
import Data.Maybe (isJust)
import GHC.Natural (Natural)
import System.Directory (removePathForcibly)

import Clash.CircuitContext.Core (withRecording)
import Clash.CircuitContext.Internal.Recorder (
  Change (..),
  Kind (..),
  RecordedSignal (..),
  Recording (..),
  readChanges,
  ticks,
  withReader,
 )
import Clash.CircuitContext.Internal.Vcd (writeFiles)

{- | The time at which the simulation stopped, in units of the VCD timescale
(the greatest common divisor of the recorded signals' periods). For a design
in one domain: the number of cycles the simulation evaluated.
-}
recordingEnd :: Recording -> Int
recordingEnd rec = ticks rec (recordingEndPs rec)

-- | Delete the recording's spill file. The recording cannot be used after this.
discardRecording :: Recording -> IO ()
discardRecording = removePathForcibly . recordingSpill

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

-- | The signals recorded, in the order they were registered.
recordedSignals :: Recording -> [RecordedSignal]
recordedSignals = recordingSignals

{- | Does the signal have a @clash-shockwaves@ description of its payload
type, which a typed-waveform viewer shows as constructors and fields?
-}
signalDescribed :: RecordedSignal -> Bool
signalDescribed = isJust . signalAdt

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
