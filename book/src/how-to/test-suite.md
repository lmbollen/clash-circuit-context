# How to keep waveforms from a test suite

This guide shows how to write the waveform of a simulation, and how to write
it only when a test fails. The functions are in
`Clash.CircuitContext.Waveform`.

Each function takes the options of the waveform, and then what
`Clash.Signal.Trace.dumpVCD` takes: the `Signal` to observe, and a window
`(start, stop)` of cycles. The simulation runs `stop` cycles. The waveform
shows the cycles from `start` to `stop - 1`. Use `(0, n)` to see all `n`
cycles. For the options, see [Choose the file and where the values
wait](#choose-the-file-and-where-the-values-wait); `defaultOptions` writes
an FST file.

The signal must be one `Signal`:

- If the design has more outputs in one domain, combine them with `bundle`.
- If the design is a `clash-protocols` circuit, make a signal of it with
  `toSignals`.
- If the outputs are in more than one domain, see [How to record a design
  with more than one domain](multi-domain.md).

## Always write the waveform

Use `writeWaveform defaultOptions "<name>"`. It simulates the design, writes
`waveforms/<name>.fst` and `waveforms/<name>.json`, and returns the samples.
It makes the directory `waveforms` in the working directory if necessary.
For the types of the functions, see [Modules](../reference/modules.md#in-a-simulation).
In a [tasty](https://hackage.haskell.org/package/tasty) suite with HUnit:

```haskell
case_accumulate :: Assertion
case_accumulate = do
  out <- writeWaveform defaultOptions "accumulate" (0, 4) (accumulate (pure 1))
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

A failed HUnit assertion, such as `@?=`, is an exception, so a check can be
an ordinary assertion:

```haskell
case_accumulate :: Assertion
case_accumulate =
  withWaveform defaultOptions "accumulate" (0, 4) (accumulate (pure 1)) $ \out ->
    out @?= [0, 0, 1, 2]
```

The examples below use a check that calls `error`, so that the book can
show what happens:

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
  withWaveform defaultOptions "accumulate" (0, 4) (accumulate (pure 1)) (check expected)
```

When the check passes, no waveform is written:

```haskell,clash group=suite
>>> removePathForcibly "waveforms/accumulate.fst"
>>> testWith [0, 0, 1, 2]
Right ()
>>> doesFileExist "waveforms/accumulate.fst"
False
```

When the check fails, the exception of the check comes out as usual. The
waveform of the failed run is written, and a message on stderr gives its
absolute path:

```haskell,clash group=suite
>>> testWith [0, 1, 2, 3]
clash-circuit-context: wrote .../waveforms/accumulate.fst
Left got [0,0,1,2]
CallStack (from HasCallStack):
...
>>> doesFileExist "waveforms/accumulate.fst"
True
```

When the test passes again, `withWaveform` deletes the waveform of the
failed run. It does not delete a waveform that `writeWaveform` wrote:

```haskell,clash group=suite
>>> testWith [0, 0, 1, 2]
Right ()
>>> doesFileExist "waveforms/accumulate.fst"
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
- The path of the waveform is on stderr, not in the message of the failed
  test. A test runner that redraws its progress line, such as tasty in a
  terminal, can print that line in the middle of its own output. Look for
  `clash-circuit-context: wrote`, or open `waveforms/<name>.fst`.

## Show only some cycles

Give a window that starts later than cycle 0. The cycles before `start` are
simulated, but they are not recorded:

```haskell
case_late :: Assertion
case_late = do
  out <- writeWaveform defaultOptions "late" (100_000, 100_100) (accumulate (pure 1))
  P.length out @?= 100_100
```

The function returns all `stop` samples. Only the waveform starts at
`start`.

The window cannot start before cycle 0, because a waveform has no negative
time.

## Let the test decide how long to simulate

Some tests read output until a value appears, for example the text that a
CPU writes to a UART. Give `maxBound` as `stop`. The check then decides how
many cycles to evaluate, and the waveform ends at the last cycle that it
evaluated:

```haskell
case_firmware :: Assertion
case_firmware =
  withWaveform defaultOptions "firmware" (0, maxBound) (uartBytes design) $ \bytes ->
    takeWhile (/= '\n') (decode bytes) @?= "RESULT: OK"
```

## Choose the file and where the values wait

The first argument of each function is a `WaveformOptions` record with two
fields. Change the fields of `defaultOptions`:

```haskell
vcdOnDisk :: WaveformOptions
vcdOnDisk = defaultOptions{format = VCD, spill = OnDisk}
```

| Field | Value | Effect |
|---|---|---|
| `format` | `FST` (default) | writes `waveforms/<name>.fst`: compressed, often tens of times smaller than a VCD file |
| | `VCD` | writes `waveforms/<name>.vcd`: text |
| `spill` | `InMemory` (default) | keeps the recorded values in memory until the waveform is written. Nothing is written to the disk but the waveform. A run that is stopped leaves nothing |
| | `OnDisk` | writes the recorded values to `waveforms/<name>.spill` while the simulation runs. The memory does not grow with the recorded history. A run that is stopped leaves the spill file, and you can write its waveform (see [How to keep the waveform of a stopped run](recover.md)) |

Surfer and GTKWave open both formats. Use one value for the whole suite: for
example, define it once and give it to each function.

## Write no waveforms at all

Use `withWaveform` for each test: it writes a waveform only when a test
fails. To simulate an instrumented design without recording, call it under
`withoutCircuitContext` and sample it as usual (see [How to leave a binding
or a function out](opt-out.md#one-call-and-everything-under-it)).

To build a project without the plugin is not a general solution. A design
with an empty `Traceable` instance does not compile without it, and the
recorders that you wrote yourself still record.

For property tests, see [How to keep the waveform of a failing
property](hedgehog.md). For the names and the place of the files, see
[Waveform files](../reference/waveform-files.md).
