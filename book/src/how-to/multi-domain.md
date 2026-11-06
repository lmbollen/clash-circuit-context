# How to record a design with more than one domain

The functions of `Clash.CircuitContext.Waveform` take one `Signal`, and a
`Signal` has one domain. This guide shows how to record a simulation with
outputs in more than one domain, and write one waveform for it. Record the
simulation with `withRecording`, and write the files with the functions of
`Clash.CircuitContext.Recording`: `writeFST` or `writeVCD`.

```haskell,clash group=domains hidden
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}
module Domains where

import Clash.Explicit.Prelude
import Clash.CircuitContext
```

```haskell,clash group=domains
import Clash.CircuitContext.Recording (discardRecording, recordingEnd, writeFST, writeVCD)
import Control.Exception (evaluate, throwIO)
import Numeric.Natural (Natural)

createDomain vSystem{vName = "Slow", vPeriod = 20000}

ticks :: (HasCircuitContext, KnownDomain dom) => Clock dom -> Signal dom (Unsigned 4)
ticks clk = count
 where
  count = register clk resetGen enableGen 0 (count + 1)
{-# OPAQUE ticks #-}

-- | Record 8 cycles of System and 4 cycles of Slow in one waveform. Return
-- the end of the recording.
recordBoth :: IO Natural
recordBoth = do
  (result, rec) <- withRecording Nothing $ do
    let fast = ticks (clockGen @System)
        slow = ticks (clockGen @Slow)
    evaluate (deepseqX (sampleN 8 fast, sampleN 4 slow) ())
  written <- writeFST (0, recordingEnd rec) rec "waveforms/both.fst"
  either throwIO pure result
  either fail pure written
  pure (recordingEnd rec)

>>> recordBoth
8
```

- `createDomain` is Template Haskell: the module needs the extension
  `TemplateHaskell`.
- `withRecording` gives the design its context, as `writeWaveform` does. It
  returns what the action returned or threw (`Either SomeException r`), and
  the `Recording`. A cancellation, such as Ctrl-C, is not returned: it goes
  on after the recording stops.
- The first argument of `withRecording` says where the recorded values
  wait. `Nothing` keeps them in memory. `Just path` writes them to a spill
  file at `path`, and makes its directory. Delete the spill file with
  `discardRecording` when the waveform is written.
- `writeFST` and `writeVCD` make the directory of the waveform, and return
  `Left` with the reason when they write nothing. Do not ignore it.
- The window of `writeFST` and `writeVCD` is `[start, stop)` in units of the
  timescale, not in cycles of one domain. Here the timescale is 10 ns, the
  greatest common divisor of the two periods, so the 8 cycles of `System`
  end at 8. For the timescale, see [Waveform
  files](../reference/waveform-files.md#time).

## Record into a spill file, and write both formats

Give `withRecording` a path to keep the values in a spill file while the
simulation runs. One recording can give an FST and a VCD:

```haskell,clash group=domains
-- | Record into a spill file, and write the waveform as FST and as VCD.
recordBothSpilled :: IO ()
recordBothSpilled = do
  (result, rec) <- withRecording (Just "waveforms/both.spill") $ do
    let fast = ticks (clockGen @System)
        slow = ticks (clockGen @Slow)
    evaluate (deepseqX (sampleN 8 fast, sampleN 4 slow) ())
  asFst <- writeFST (0, recordingEnd rec) rec "waveforms/both.fst"
  asVcd <- writeVCD (0, recordingEnd rec) rec "waveforms/both.vcd"
  discardRecording rec
  either throwIO pure result
  either fail pure asFst
  either fail pure asVcd

>>> recordBothSpilled
```

## Keep the waveform of a stopped run

When the action of `withRecording` is cancelled, the spill file stays, and a
message on stderr gives its absolute path. A process that is killed leaves
the spill file too. Write its waveform with `writeSpill` of
`Clash.CircuitContext.Recording`, or with an executable:

```haskell
writeSpill FST Nothing "waveforms/both.spill" "waveforms/both.fst"
```

```bash
spill2fst waveforms/both.spill
```

`recoverWaveform` takes the name of a waveform, and reads the spill file
`waveforms/<name>.spill`. It works for a spill file of `withRecording` only
at that path. For more, see [How to keep the waveform of a stopped
run](recover.md).
