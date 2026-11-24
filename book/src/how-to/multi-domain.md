# How to record a design with more than one domain

The functions of `Clash.CircuitContext.Waveform` take one `Signal`, and a
`Signal` has one domain. These functions record each signal of the design
that the simulation evaluates, also the signals in other domains. Their
window is in cycles of the domain of the signal that you give. If you check
the outputs of one domain only, use these functions: see [How to keep
waveforms from a test suite](test-suite.md). A signal in another domain is
then recorded as far as the simulation evaluates it to make the outputs.

This guide shows how to record a simulation with outputs in more than one
domain, and write one waveform for it. Record the simulation with
`withRecording`, and write the files with the functions of
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
- `recordBoth` calls `ticks` two times, so the waveform has two instances
  of `ticks`. If one design has outputs in the two domains, call the design
  one time, and take the two outputs from that call, for example
  `let (fast, slow) = design clkFast clkSlow`. Two calls of the design are
  two instances, and each instance simulates the whole design.
- `withRecording` gives the design its context, as `writeWaveform` does. It
  returns what the action returned or threw (`Either SomeException r`), and
  the `Recording`. A cancellation, such as Ctrl-C, is not returned: it goes
  on after the recording stops.
- The first argument of `withRecording` says where the recorded values
  wait. `Nothing` keeps them in memory. `Just path` writes them to a spill
  file at `path`, and makes its directory. Delete the spill file with
  `discardRecording` when the waveform is written.
- `withRecording` makes the spill file before the action. If it cannot,
  for example because a part of the path is a file or a directory is not
  writable, it throws an `IOError` that names the spill file, and the
  action does not run. The `Either` holds only what the action returned or
  threw.
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

## Record a long run

`deepseqX (sampleN 8 fast, sampleN 4 slow)` keeps all samples of the two
domains in memory until it has evaluated all of them. It also evaluates
all cycles of `fast` before the first cycle of `slow`. If the two outputs
come from one design, the simulation then makes the values of the `slow`
side as far as the `fast` side needs them, and these values also stay in
memory until `slow` is evaluated. For a few cycles, this is not a problem.
For a long run, the memory grows with the length of the run, also with a
spill file.

For a long run, evaluate the samples of the two domains in the order of
their time, and do not keep them:

```haskell,clash group=domains
-- | Evaluate the samples of two domains in the order of their time, up to
-- the time end, and keep no sample. pa and pb are the clock periods of the
-- two domains. All times are in ps.
walk :: (NFDataX a, NFDataX b) => Natural -> Natural -> Natural -> [a] -> [b] -> IO ()
walk pa pb end = go 0 0
 where
  go ta tb xs ys
    | ta < end && (ta <= tb || tb >= end), x : xs' <- xs = evaluate (rnfX x) >> go (ta + pa) tb xs' ys
    | tb < end, y : ys' <- ys = evaluate (rnfX y) >> go ta (tb + pb) xs ys'
    | otherwise = pure ()

-- | Record 200 ns of the two domains into a spill file: 20 cycles of
-- System and 10 cycles of Slow. Return the end of the recording.
recordLong :: IO Natural
recordLong = do
  (result, rec) <- withRecording (Just "waveforms/long.spill") $ do
    let fast = ticks (clockGen @System)
        slow = ticks (clockGen @Slow)
    walk (snatToNum (clockPeriod @System)) (snatToNum (clockPeriod @Slow)) 200000 (sample fast) (sample slow)
  written <- writeFST (0, recordingEnd rec) rec "waveforms/long.fst"
  discardRecording rec
  either throwIO pure result
  either fail pure written
  pure (recordingEnd rec)

>>> recordLong
20
```

- `sample` gives all samples of a signal, as a list without an end.
  `walk` takes one sample at a time from the list whose next sample comes
  first in time, and stops at `end`.
- `clockPeriod @dom` gives the clock period of a domain in ps.
- No sample stays in memory after `walk` evaluated it. With a spill file,
  the memory of the run then does not grow with its length. Without a spill
  file, only the recorded values grow, as compact records (see [About the
  cost of recording](../explanation/cost.md#where-a-recording-keeps-its-history)).
- For one domain, evaluate the samples one at a time, for example with
  `mapM_ (evaluate . rnfX) (sampleN n design)` as the action of
  `withRecording`.

## Keep the waveform of a stopped run

When the action of `withRecording` is cancelled after it recorded a cycle,
the spill file stays, and a message on stderr gives its absolute path. A
process that is killed leaves the spill file too. Write its waveform with
`writeSpill` of `Clash.CircuitContext.Recording`, or with an executable:

```haskell
writeSpill FST Nothing "waveforms/both.spill" "waveforms/both.fst"
```

```bash
spill2fst waveforms/both.spill
```

`recoverWaveform` takes the path of a waveform without an extension, and
reads the spill file `<path>.spill`. For the spill file above, give it
`"waveforms/both"`. For more, see [How to keep the waveform of a stopped
run](recover.md).
