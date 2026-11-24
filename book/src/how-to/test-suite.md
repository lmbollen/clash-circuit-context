# How to keep waveforms from a test suite

This guide shows how to write the waveform of a simulation, and how to write
it only when a test fails. The functions are in
`Clash.CircuitContext.Waveform`.

Each function takes the options of the waveform, the path of the waveform,
and then what `Clash.Signal.Trace.dumpVCD` takes: a window `(start, stop)` of
cycles, and the `Signal` to observe. The simulation runs `stop` cycles. The waveform
shows the cycles from `start` to `stop - 1`. Use `(0, n)` to see all `n`
cycles. For the path, see [Choose the path of the
waveform](#choose-the-path-of-the-waveform). For the options, see [Choose
the file and where the values wait](#choose-the-file-and-where-the-values-wait);
`defaultOptions` writes an FST file.

The signal must be one `Signal`:

- If the design has more outputs in one domain, combine them with `bundle`.
- If the design is a `clash-protocols` circuit, make a signal of it with
  `toSignals`.
- If the outputs are in more than one domain, see [How to record a design
  with more than one domain](multi-domain.md).

## Set up the test suite

For example, with tasty and HUnit, where the library `counters` holds the
instrumented design:

```cabal
test-suite tests
  type:             exitcode-stdio-1.0
  hs-source-dirs:   tests
  main-is:          Main.hs
  default-language: Haskell2010
  build-depends:
    base,
    clash-prelude,
    clash-circuit-context,
    counters,
    tasty,
    tasty-hunit,
```

The test suite needs the plugin only if it defines functions with
`HasCircuitContext` itself. Give it the `default-extensions` and type-checker
plugins that your test code uses.

Give the design to a function of this guide directly, as its argument. If
you bind it with `let` first, give the binding a signature with
`CircuitContextOnly`, the context that these functions supply.
`Clash.CircuitContext.Waveform` exports it:

```haskell
let design :: (CircuitContextOnly) => Signal System (Unsigned 8)
    design = top (pure True)
```

Without a signature, the error names `CircuitContextOnly`.

## Choose the path of the waveform

The second argument of each function is the path of the waveform, without
an extension. The function adds the extension of the format, and writes the
sidecar with the extension `.json` beside the waveform. For example,
`"waveforms/accumulate"` gives `waveforms/accumulate.fst` and
`waveforms/accumulate.json`.

- A relative path is relative to the working directory of the process. The
  working directory of `cabal test` and of `cabal run` can differ. The
  messages of the functions give the absolute path of each file that they
  write or keep.
- An absolute path, for example `"/home/you/project/out/accumulate"`, is
  the same from each working directory.
- If a directory of the path does not exist, the function makes it.
- Give each test its own path. The functions do not check this. Two tests
  with the same path write the same files: the waveform of one test can
  replace the waveform of the other, and a passing test can delete the
  waveform of a failed test. Two tests that record at the same time with
  `spill = OnDisk` also use the same spill file.

## Always write the waveform

Use `writeWaveform defaultOptions "<path>"`. It simulates the design, writes
`<path>.fst` and `<path>.json`, and returns the samples. For the types of
the functions, see [Modules](../reference/modules.md#in-a-simulation). In a [tasty](https://hackage.haskell.org/package/tasty) suite with HUnit:

```haskell
case_accumulate :: Assertion
case_accumulate = do
  out <- writeWaveform defaultOptions "waveforms/accumulate" (0, 4) (accumulate (pure 1))
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
  withWaveform defaultOptions "waveforms/accumulate" (0, 4) (accumulate (pure 1)) $ \out ->
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
import Clash.CircuitContext.Recording (discardRecording, recordingEnd, writeFST)
import Control.Exception (ErrorCall, evaluate, throwIO, try)
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
  withWaveform defaultOptions "waveforms/accumulate" (0, 4) (accumulate (pure 1)) (check expected)
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
failed run. The sidecar of that waveform marks it as the waveform of a
failed run. `withWaveform` does not delete a waveform without this mark,
for example one that `writeWaveform` wrote:

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
  `clash-circuit-context: wrote`, or open `<path>.fst`.

## Show only some cycles

Give a window that starts later than cycle 0. The cycles before `start` are
simulated, but they are not recorded:

```haskell
case_late :: Assertion
case_late = do
  out <- writeWaveform defaultOptions "waveforms/late" (100_000, 100_100) (accumulate (pure 1))
  P.length out @?= 100_100
```

The function returns all `stop` samples. Only the waveform starts at
`start`.

The window cannot start before cycle 0, because a waveform has no negative
time.

## Let the test decide how long to simulate

Some tests read output until a value appears, for example the text that a
CPU writes to a UART. With `withWaveform`, give `maxBound` as `stop`. The
check then decides how many cycles to evaluate, and the waveform ends at the
last cycle that it evaluated. Do not give `maxBound` to `writeWaveform` or
`simulateWaveform`: they evaluate each sample up to `stop`, so they do not
end.

```haskell
case_firmware :: Assertion
case_firmware =
  withWaveform defaultOptions "waveforms/firmware" (0, maxBound) (uartBytes design) $ \bytes ->
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
| `format` | `FST` (default) | writes `<path>.fst`: compressed, often tens of times smaller than a VCD file |
| | `VCD` | writes `<path>.vcd`: text |
| `spill` | `InMemory` (default) | keeps the recorded values in memory until the waveform is written. Nothing is written to the disk but the waveform. A run that is stopped leaves nothing |
| | `OnDisk` | writes the recorded values to `<path>.spill`, beside the waveform, while the simulation runs. The recorded values do not stay in memory, but the samples that a function returns do: see [Record a long run](#record-a-long-run). A recording that is stopped after it recorded a cycle leaves the spill file, and you can write its waveform (see [How to keep the waveform of a stopped run](recover.md)). `withWaveform` records only when it runs a failed check again |

Surfer and GTKWave open both formats. Use one value for the whole suite: for
example, define it once and give it to each function.

A spill file uses the disk only when its path is on a disk. A file system
in RAM (tmpfs) keeps its files in RAM: for example `/tmp` on many systems,
and the working directory of some CI containers. A spill file there uses
as much RAM as it has bytes, so `spill = OnDisk` does not save memory. The
spill file is always beside the waveform, so give such a test a path on a
disk.

## Record a long run

`writeWaveform` returns all `stop` samples, and keeps them in memory until
it returns. So its memory grows with `stop`, also with `spill = OnDisk`.
`simulateWaveform` does the same. `withWaveform` gives the samples to the
check, and the check decides which samples stay in memory.

For a long run whose samples you do not need, record with `withRecording`,
evaluate the samples one at a time, and keep none of them:

```haskell,clash group=suite
-- | Record n cycles into a spill file, and keep no sample.
recordLong :: Int -> IO ()
recordLong n = do
  (result, rec) <- withRecording (Just "waveforms/long.spill") $
    mapM_ (evaluate . rnfX) (sampleN n (accumulate (pure 1)))
  written <- writeFST (0, recordingEnd rec) rec "waveforms/long.fst"
  discardRecording rec
  either throwIO pure result
  either fail pure written

>>> recordLong 1000
```

With a spill file, the memory of this run does not grow with `n`.
`withRecording` takes the path of the spill file with its extension, and
`writeFST` takes the path of the waveform with its extension. Its window is
in units of the timescale: for one domain, one unit is one cycle. For more
than one domain, see [How to record a design with more than one
domain](multi-domain.md#record-a-long-run).

## Write no waveforms at all

Use `withWaveform` for each test: it writes a waveform only when a test
fails. To simulate an instrumented design without recording, call it under
`withoutCircuitContext` and sample it as usual (see [How to leave a binding
or a function out](opt-out.md#one-call-and-everything-under-it)).

To build a project without the plugin is not a solution: an instrumented
design does not compile without it.

For property tests, see [How to keep the waveform of a failing
property](hedgehog.md). For the names and the life of the files, see
[Waveform files](../reference/waveform-files.md).
