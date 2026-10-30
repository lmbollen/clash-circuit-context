# How to probe the state of a mealy machine

The state of a mealy machine is not a signal: it is a value inside the step
function. This guide shows how to record it, one value for each cycle.

1. Use `mealyProbed` instead of `mealy`. It takes a clock, a reset and an
   enable, as `Clash.Explicit.Prelude.mealy` does.
2. Give the step function a signature with `HasProbe`.
3. Give each value that you want to see a name in a `where` or `let`
   binding.

```haskell,clash group=probe hidden
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}
module Probe where

import Clash.Explicit.Prelude
import Clash.CircuitContext
import Clash.Shockwaves.Waveform (Waveform)
import BookSupport
```

```haskell,clash group=probe
data Phase = Idle | Busy | Done
  deriving (Generic, NFDataX, BitPack, Waveform, Show, Eq)

control :: (HasCircuitContext) => Signal System Bool -> Signal System Bool
control start = done
 where
  done = mealyProbed clockGen resetGen enableGen step Idle start

  step :: (HasProbe) => Phase -> Bool -> (Phase, Bool)
  step phase go = (phase', phase' == Done)
   where
    phase' = case phase of
      Idle | go -> Busy
      Busy -> Done
      _ -> Idle
{-# OPAQUE control #-}
```

The named value `phase'` of the step function is a probe. It is in the scope
of the component that calls `mealyProbed`, beside the signals of that
component:

```haskell,clash group=probe
>>> printWaveform 4 (control (fromList [True, False, False, False]))
control
  phase'
  done
```

## Variants

- For a Moore machine, use `mooreProbed`.
- For a machine with a bundled input and output, use `mealyBProbed` or
  `mooreBProbed`.
- To probe a value without a name, apply `probe "<name>"` to it inside the
  step function. `probe` needs `BitPack` of the value.

## What a probe can record

A probe records a value that Clash can represent, and that is not itself
sequential. A `Signal`, a clock, a list or a function in a step function is
not probed, and the plugin does not warn about it. For the full rules, see
[Types that can be recorded](../reference/recordable-types.md).

A probe has no cost in hardware. When Clash generates HDL, `mealyProbed` is
an ordinary `mealy`, without the cycle counter that it uses in simulation.
