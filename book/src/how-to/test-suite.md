# How to keep waveforms from a test suite

This guide shows how to write the waveform of a simulation, and how to write
it only when a test fails. The functions are in
`Clash.CircuitContext.Waveform`.

Each function takes what `Clash.Signal.Trace.dumpVCD` takes: the `Signal` to
observe, and a window `(start, stop)` of cycles. The simulation runs `stop`
cycles. The waveform shows the cycles from `start` to `stop - 1`. Use
`(0, n)` to see all `n` cycles.

The signal must be one `Signal`:

- If the design has more outputs in one domain, combine them with `bundle`.
- If the design is a `clash-protocols` circuit, make a signal of it with
  `toSignals`.
- If the outputs are in more than one domain, see [Record a design with more
  than one domain](#record-a-design-with-more-than-one-domain).

## Always write the waveform

Use `writeWaveform "<name>"`. It simulates the design, writes
`waveforms/<name>.vcd` and `waveforms/<name>.json`, and returns the samples.
In a [tasty](https://hackage.haskell.org/package/tasty) suite with HUnit:

```haskell
case_accumulate :: Assertion
case_accumulate = do
  out <- writeWaveform "accumulate" (0, 4) (accumulate (pure 1))
  out @?= [0, 0, 1, 2]
```

If the simulation throws an exception, the waveform up to that cycle is
written, and the exception comes out as usual.

## Write the waveform only when the test fails

Recording has a cost in time and memory (see [About the cost of
recording](../explanation/cost.md)). In most runs, the waveform of a passing
test is not useful. Use `withWaveform`, and give it the check of the test.
It runs the simulation without recording. If the check throws an exception,
it runs the simulation again with recording, and writes that waveform.

```haskell,clash group=suite hidden
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}
module Suite where

import Clash.Explicit.Prelude
import Clash.CircuitContext
import Clash.CircuitContext.Waveform
import Control.Exception (ErrorCall, try)
import Control.Monad (unless)
import System.Directory (doesFileExist, removePathForcibly)
import qualified Prelude as P

accumulate :: (HasCircuitContext) => Signal System (Unsigned 8) -> Signal System (Unsigned 8)
accumulate inp = total
 where
  total = register clockGen resetGen enableGen 0 (total + inp)
{-# OPAQUE accumulate #-}
```

```haskell,clash group=suite
-- | Throw if the samples are not the ones we expect.
check :: [Unsigned 8] -> [Unsigned 8] -> IO ()
check expected out = unless (out == expected) (P.error ("got " <> show out))

testWith :: [Unsigned 8] -> IO (Either ErrorCall ())
testWith expected = try $
  withWaveform "accumulate" (0, 4) (accumulate (pure 1)) (check expected)
```

When the check passes, no waveform is written:

```haskell,clash group=suite
>>> removePathForcibly "waveforms/accumulate.vcd"
>>> testWith [0, 0, 1, 2]
Right ()
>>> doesFileExist "waveforms/accumulate.vcd"
False
```

When the check fails, the exception of the check comes out as usual. The
waveform of the failed run is written, and a message on stderr gives its
absolute path:

```haskell,clash group=suite
>>> testWith [0, 1, 2, 3]
clash-circuit-context: wrote .../waveforms/accumulate.vcd
Left got [0,0,1,2]
CallStack (from HasCallStack):
...
>>> doesFileExist "waveforms/accumulate.vcd"
True
```

When the test passes again, `withWaveform` deletes the waveform of the
failed run. It does not delete a waveform that `writeWaveform` wrote:

```haskell,clash group=suite
>>> testWith [0, 0, 1, 2]
Right ()
>>> doesFileExist "waveforms/accumulate.vcd"
False
```

Keep these rules in mind:

- The check runs twice when it fails: once without recording, and once with.
  Give `withWaveform` a check that only tests values. Do not give it work
  that must happen once, for example a write to a file.
- The second run must fail in the same way. If it passes, no waveform is
  written, and a message on stderr says so. A design that resolves undefined
  values at random can do this. For such a design, use `writeWaveform`, and
  check the samples that it returns.
- The check gets the samples lazily. The waveform has the cycles that the
  check evaluated. A check that stops at the first wrong value gives a
  waveform that ends at that value.

## Show only some cycles

Give a window that starts later than cycle 0. The cycles before `start` are
simulated, but they are not recorded:

```haskell
case_late :: Assertion
case_late = do
  out <- writeWaveform "late" (100_000, 100_100) (accumulate (pure 1))
  P.length out @?= 100_100
```

The function returns all `stop` samples. Only the waveform starts at
`start`.

The window cannot start before cycle 0, because a VCD has no negative time.

## Let the test decide how long to simulate

Some tests read output until a value appears, for example the text that a
CPU writes to a UART. Give `maxBound` as `stop`. The check then decides how
many cycles to evaluate, and the waveform ends at the last cycle that it
evaluated:

```haskell
case_firmware :: Assertion
case_firmware =
  withWaveform "firmware" (0, maxBound) (uartBytes design) $ \bytes ->
    takeWhile (/= '\n') (decode bytes) @?= "RESULT: OK"
```

## Record a design with more than one domain

A `Signal` has one domain. For outputs in more than one domain, record the
simulation with `Clash.CircuitContext.Recording`:

```haskell
main :: IO ()
main = do
  (result, rec) <- withRecording "waveforms/top.spill" $ do
    let (a, b) = top clkA clkB
    evaluate (deepseqX (sampleN 100 a, sampleN 50 b) ())
  _ <- writeVCD (0, recordingEnd rec) rec "waveforms/top.vcd"
  discardRecording rec
  either throwIO pure result
```

The window of `writeVCD` is in units of the VCD timescale, not in cycles of
one domain. For the timescale, see [Waveform files](../reference/waveform-files.md).

## Write no waveforms at all

Build without the plugin. Then only the recorders that you wrote yourself
record. For the plugin flags, see [How to add clash-circuit-context to a
project](add-to-project.md).

For property tests, see [How to keep the waveform of a failing
property](hedgehog.md). For the names and the place of the files, see
[Waveform files](../reference/waveform-files.md).
