# How to keep waveforms from a test suite

This guide shows how to write a waveform from a test, and how to write one
only when you need it. The functions are in `Clash.CircuitContext.Waveform`.

## Give each test a slot

A slot is the pending waveform of one test. `withWaveformSlot "<name>"`
makes a slot, runs the test, and writes `waveforms/<name>.vcd` and
`waveforms/<name>.json`. It writes the files also when the test throws an
exception, so a failed test keeps the waveform that shows why it failed.

In a [tasty](https://hackage.haskell.org/package/tasty) suite with HUnit, a
test that always keeps its waveform looks like this:

```haskell
case_accumulate :: Assertion
case_accumulate = withWaveformSlot "accumulate" $ \slot -> do
  out <- withWaveform slot 4 (sampleN 4 (accumulate (pure 1)))
  out @?= [0, 0, 1, 2]
```

## Keep only the waveform of a failed test

Recording has a cost in time and memory (see [About the cost of
recording](../explanation/cost.md)). In most runs, a waveform of a passing
test is not useful. Use `withWaveformOnFailure` instead of `withWaveform`. It
runs the simulation without recording. If the check fails, it runs the
simulation again with recording, and keeps that waveform.

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
testWith expected = try $ withWaveformSlot "accumulate" $ \slot ->
  withWaveformOnFailure slot 4 (sampleN 4 (accumulate (pure 1))) (check expected)
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

The check runs twice when it fails: once without recording, and once with.
Give `withWaveformOnFailure` a check that only tests values. Do not give it
work that must happen once, for example a write to a file.

If the second run passes, the failure did not repeat. Then no waveform is
written, because it would show a run that passed, and a message on stderr
says so. If the design does not give the same output in each run (for
example, it resolves undefined values at random), use
`withWaveformOnFailure'`. It records the first run, and writes the waveform
only if the check fails.

## Keep waveforms on request

Use `withWaveformWhen keep` to record only when `keep` is `True`. The
function `waveformsRequested` is `True` when the environment variable
`CCC_WAVEFORMS` is set to a value other than empty, `0` or `no`:

```haskell
case_accumulate :: Assertion
case_accumulate = withWaveformSlot "accumulate" $ \slot -> do
  keep <- waveformsRequested
  out <- withWaveformWhen keep slot 4 (sampleN 4 (accumulate (pure 1)))
  out @?= [0, 0, 1, 2]
```

Then `CCC_WAVEFORMS=1 cabal test` writes all the waveforms, and a plain
`cabal test` writes none.

## Record as many cycles as the test uses

`withWaveform` records a fixed number of cycles. If the test decides how long
to simulate, for example a test that reads output until a value appears, use
`withWaveformLazy`. It records the cycles that the test evaluated.

For property tests, see [How to keep the waveform of a failing
property](hedgehog.md). For the names and the place of the files, see
[Waveform files](../reference/waveform-files.md).
