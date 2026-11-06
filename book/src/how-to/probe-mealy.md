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
import BookSupport
```

The type of the state derives `Waveform`, so that a viewer can show its
constructors (see [How to show payload types in a waveform
viewer](typed-waveforms.md)). `Waveform` is from the package
clash-shockwaves: add it to `build-depends`.

```haskell,clash group=probe
import Clash.Shockwaves.Waveform (Waveform)

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
component. (`printWaveform` is a helper of this book: see [the
introduction](../introduction.md#the-examples-are-checked).)

```haskell,clash group=probe
>>> printWaveform 4 (control (fromList [True, False, False, False]))
control
  done
  phase'
```

The arguments of the step function (`phase`, `go`) are not probed. `phase'`
is the new state, so it shows each state one cycle before the machine holds
it.

## Why a probe is `z` in the first and the last cycle

A probe records a value when the simulation computes it. The simulation
computes a value of the step function only when something uses it, and in
two cycles nothing uses the new state:

- In cycle 0, the reset is active, so the register of the machine takes its
  reset value, not the new state.
- In the last cycle, there is no later cycle to take the new state.

These values are never computed, so the probes that depend only on them are
`z` in those cycles. A probe of a value that the output also uses, for
example a value that both the new state and the output read, has a value in
these cycles.

The same is true in each cycle, not only in the first and the last: a probe
is `z` in each cycle in which the simulation does not need its value. For
example, a value that only the untaken branch of an `if` reads is not
computed in that cycle.

## A Moore machine

Use `mooreProbed`. It takes two functions: the transition function and the
output function. Only the transition function has `HasProbe` and is probed.
The output function is an ordinary function of the state.

```haskell,clash group=probe
blink :: (HasCircuitContext) => Signal System Bool -> Signal System Bool
blink run = led
 where
  led = mooreProbed clockGen resetGen enableGen next (== 3) (0 :: Index 4) run

  next :: (HasProbe) => Index 4 -> Bool -> Index 4
  next n go = n'
   where
    n' = if go then satSucc SatWrap n else n
{-# OPAQUE blink #-}

>>> printWaveform 4 (blink (pure True))
blink
  led
  n'
```

For a machine with a bundled input and output, use `mealyBProbed` or
`mooreBProbed`. For the types, see [Modules](../reference/modules.md#probes).

## Probe a value by hand

To probe a value that has no name, or an argument of the step function,
apply `probe "<name>"` to it inside the step function:

```haskell,clash group=probe
toggle :: (HasCircuitContext) => Signal System Bool -> Signal System Bool
toggle press = on
 where
  on = mealyProbed clockGen resetGen enableGen step False press

  step :: (HasProbe) => Bool -> Bool -> (Bool, Bool)
  step s p = (if probe "pressed" p then not s else s, s)
{-# OPAQUE toggle #-}

>>> printWaveform 4 (toggle (fromList [False, True, False, True]))
toggle
  on
  pressed
```

- `probe` needs `BitPack` and `NFDataX` of the value. It records the value
  as bits only: the sidecar does not describe it, also when its type has a
  `Waveform` instance.
- To have the sidecar describe the value, use `probeSW`. It also needs
  `Waveform` of the value.

A probe that the plugin adds is described when the type has a `Waveform`
instance, and is recorded as bits when it has not.

## What a probe can record

A probe records a value that Clash can represent, and that is not itself
sequential. A `Signal`, a clock, a list or a function in a step function is
not probed, and the plugin does not warn about it. For the full rules, see
[Types that can be recorded](../reference/recordable-types.md).

A probe has no cost in hardware. When Clash generates HDL, `mealyProbed` is
an ordinary `mealy`, without the cycle counter that it uses in simulation.
