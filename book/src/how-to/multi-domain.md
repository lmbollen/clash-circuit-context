# How to record a design with more than one domain

The functions of `Clash.CircuitContext.Waveform` take one `Signal`, and a
`Signal` has one domain. This guide shows how to record a simulation with
outputs in more than one domain, and write one waveform for it. Record the
simulation with `withRecording`, and write the files with the functions of
`Clash.CircuitContext.Recording`.

```haskell,clash group=domains hidden
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}
module Domains where

import Clash.Explicit.Prelude
import Clash.CircuitContext
```

```haskell,clash group=domains
import Clash.CircuitContext.Recording (discardRecording, recordingEnd, writeVCD)
import Control.Exception (evaluate, throwIO)
import System.Directory (createDirectoryIfMissing)

createDomain vSystem{vName = "Slow", vPeriod = 20000}

ticks :: (HasCircuitContext, KnownDomain dom) => Clock dom -> Signal dom (Unsigned 4)
ticks clk = count
 where
  count = register clk resetGen enableGen 0 (count + 1)
{-# OPAQUE ticks #-}

-- | Record 8 cycles of System and 4 cycles of Slow in one VCD. Return the
-- end of the recording.
recordBoth :: IO Int
recordBoth = do
  createDirectoryIfMissing True "waveforms"
  (result, rec) <- withRecording "waveforms/both.spill" $ do
    let fast = ticks (clockGen @System)
        slow = ticks (clockGen @Slow)
    evaluate (deepseqX (sampleN 8 fast, sampleN 4 slow) ())
  written <- writeVCD (0, recordingEnd rec) rec "waveforms/both.vcd"
  discardRecording rec
  either throwIO pure result
  either fail pure written
  pure (recordingEnd rec)

>>> recordBoth
8
```

- `withRecording` gives the design its context, as `writeWaveform` does. It
  returns what the action returned or threw (`Either SomeException r`),
  and the `Recording`.
- `withRecording` does not make the directory of the spill file. Make it
  first.
- `writeVCD` returns `Left` with the reason when it writes nothing. Do not
  ignore it.
- The window of `writeVCD` is `[start, stop)` in units of the VCD
  timescale, not in cycles of one domain. Here the timescale is 10 ns, the
  greatest common divisor of the two periods, so the 8 cycles of `System`
  end at 8. For the timescale, see [Waveform
  files](../reference/waveform-files.md#the-vcd).
