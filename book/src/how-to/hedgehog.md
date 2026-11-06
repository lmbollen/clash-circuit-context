# How to keep the waveform of a failing property

A [Hedgehog](https://hackage.haskell.org/package/hedgehog) property runs
many cases. This guide shows how to keep one waveform for it: the waveform
of the counterexample when the property fails, and, if you want it, the
waveform of one passing case. The function is `simulateWaveform`, in
`Clash.CircuitContext.Waveform.Hedgehog`.

Do not use `withWaveform` in a property. A failed Hedgehog check is not an
exception, so `withWaveform` cannot see the failure.

## Keep the waveform of the counterexample

Simulate the design with `simulateWaveform defaultOptions Counterexample`,
and check the samples that it returns. The first argument has the options of
the waveform, as for `writeWaveform` (see [How to keep waveforms from a test
suite](test-suite.md#choose-the-file-and-where-the-values-wait)). The second
argument has the type `Capture`, with the constructors `Counterexample` and
`AtSize Size`. The other arguments are the same as for `writeWaveform`:

```haskell
simulateWaveform ::
  (KnownDomain dom, NFDataX a) =>
  WaveformOptions -> Capture -> String -> (Int, Int) ->
  ((HasCircuitContext) => Signal dom a) -> PropertyT IO [a]
```

```haskell,clash group=hedgehog hidden
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}
module Prop where

import Clash.Explicit.Prelude
import Clash.CircuitContext
import Clash.CircuitContext.Waveform.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Prelude as P

accumulate :: (HasCircuitContext) => Signal System (Unsigned 8) -> Signal System (Unsigned 8)
accumulate inp = total
 where
  total = register clockGen resetGen enableGen 0 (total + inp)
{-# OPAQUE accumulate #-}
```

```haskell,clash group=hedgehog
-- | The reset holds the register in cycle 0, so the first input is not
-- added, and each sum comes one cycle late.
model :: [Unsigned 8] -> [Unsigned 8]
model xs = 0 : P.scanl (+) 0 (P.drop 1 xs)

prop_accumulate :: Property
prop_accumulate = property $ do
  xs <- forAll (Gen.list (Range.linear 1 20) (Gen.integral Range.linearBounded))
  let n = P.length xs
  out <- simulateWaveform defaultOptions Counterexample "prop" (0, n) (accumulate (fromList xs))
  out === P.take n (model xs)

>>> :t prop_accumulate
prop_accumulate :: Property
```

Each case runs without recording, so a property that passes costs nothing
extra and writes no file. When the property fails, Hedgehog shrinks the
case, and reports the smallest counterexample. Only that case is simulated
again with recording, and its waveform is written once. The report ends
with the path of the file:

```text
       ┃     │ ━━━ Failed (- lhs) (+ rhs) ━━━
       ┃     │ - [ 0 , 0 , 1 ]
       ┃     │ + [ 0 , 0 , 0 ]
    waveform: /home/you/project/waveforms/prop.fst
```

Keep these rules in mind:

- Give each property its own name. One name is one pair of files.
- The second simulation must give the same result as the first. This is
  true when the property supplies all inputs of the design.
- The waveform is written when Hedgehog prints the report. tasty-hedgehog
  prints it. A runner that does not print the report of a failure writes no
  waveform.
- The waveform of a counterexample stays when the property passes again
  later. `simulateWaveform` does not delete files, as `withWaveform` does.
  Delete `waveforms/<name>.fst` yourself when you do not need it.

## Keep the waveform of a passing case too

Use `AtSize size` instead of `Counterexample`. The first case whose size is
at least `size` is recorded, and its waveform is kept, also when it passes.
This happens once for each waveform name in a process. A small size gives a
short waveform that is easy to read:

```haskell
out <- simulateWaveform defaultOptions (AtSize 10) "prop" (0, n) (accumulate (fromList xs))
```

The waveform of this case has the size in its name:
`waveforms/prop_10.fst`. The waveform of a counterexample is
`waveforms/prop.fst`, so the two do not replace each other.

Hedgehog gives the tests of a property the sizes `0`, `1`, `2`, … up to
`99`, and then starts at `0` again. So choose a size below the number of
tests, and below 100.

## Record a failed case again

A failed property prints a `recheckAt` line with a seed. Run that line to
run the failed case again. Hedgehog reports it again, so its waveform is
written again.

For the names and the place of the files, see [Waveform
files](../reference/waveform-files.md).
