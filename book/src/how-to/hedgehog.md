# How to keep the waveform of a failing property

A [Hedgehog](https://hackage.haskell.org/package/hedgehog) property runs
many cases. This guide shows how to keep one waveform for it: the waveform
of the counterexample when the property fails, and, if you want it, the
waveform of one passing case. The functions are in
`Clash.CircuitContext.Waveform.Hedgehog`.

Do not use `withWaveformOnFailure` in a property. A failed Hedgehog check is
not an exception, so it cannot see the failure.

## Keep the waveform of the counterexample

Put the simulation and the check in `withWaveformOnCounterexample`:

```haskell,clash group=hedgehog hidden
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}
module Prop where

import Clash.Explicit.Prelude
import Clash.CircuitContext
import Clash.CircuitContext.Waveform
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

prop_accumulate :: WaveformSlot -> Property
prop_accumulate slot = property $ do
  xs <- forAll (Gen.list (Range.linear 1 20) (Gen.integral Range.linearBounded))
  let n = P.length xs
  withWaveformOnCounterexample slot n (sampleN n (accumulate (fromList xs))) $ \out ->
    out === P.take n (model xs)

>>> :t prop_accumulate
prop_accumulate :: WaveformSlot -> Property
```

Each case runs without recording, so a property that passes costs nothing
extra and writes no file. When a case fails, that case runs again with
recording, and its waveform replaces the earlier one in the slot. Hedgehog
then shrinks the case, and each smaller case that fails replaces the
waveform again. The waveform that stays is the one of the smallest
counterexample: the case that Hedgehog reports. The report ends with the
path of the file:

```text
       ┃     │ ━━━ Failed (- lhs) (+ rhs) ━━━
       ┃     │ - [ 0 , 0 , 1 ]
       ┃     │ + [ 0 , 0 , 0 ]
    waveform: /home/you/project/waveforms/prop.vcd
```

The check runs twice for a failed case. Do not put `forAll` or other
one-time work inside it. The second run must fail in the same way, which is
true when the test supplies all inputs of the simulation.

## Keep the waveform of a passing case too

Use `withWaveformCase keep` instead. When `keep` is `True`, the case is
recorded as it runs, and its waveform is kept even if it passes. A failure
still replaces it with the counterexample.

To choose the case, use a flag that fires once:

- `recordLargestCase fired` is `True` for the first case at the largest
  size.
- `recordCaseOfSize fired size` is `True` for the first case at least that
  size. A small size gives a small waveform that is easy to read.

`fired` is an `IORef Bool` that you make, one for each property. It stays
`True` after the first case, also when Hedgehog runs the generators again
during shrinking.

## Record one case again

A failed property prints a size and a seed. To record that case later, run
`recheckWithWaveform size seed property`. The property must record that run,
for example with `recordThisCase fired (const True)` as its flag.
