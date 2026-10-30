# How to make a function a component

This guide shows how to give a function its own scope in the waveform, and
how to name the instances when a component is used more than once. For all
the rules, see [What the plugin instruments](../reference/instrumentation.md).

## Make a function a component

Give the function both annotations:

- `HasCircuitContext` in its signature
- an `{-# OPAQUE #-}` pragma

```haskell,clash group=component hidden
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}
module Component where

import Clash.Explicit.Prelude
import Clash.CircuitContext
import BookSupport
```

```haskell,clash group=component
lag :: (HasCircuitContext) => Signal System Int -> Signal System Int
lag inp = late
 where
  early = register clockGen resetGen enableGen 0 inp
  late = register clockGen resetGen enableGen 0 early
{-# OPAQUE lag #-}

pair :: (HasCircuitContext) => Signal System Int -> Signal System Int
pair inp = both
 where
  both = lag inp + lag (inp + 1)
{-# OPAQUE pair #-}
```

Each named local binding is a signal in the scope of its function. Two calls
of the same function are two scopes, numbered in the order of the calls in
the source:

```haskell,clash group=component
>>> printWaveform 4 (pair (pure 1))
pair
  both
  lag_0
    late
    early
  lag_1
    late
    early
```

## Put a helper in the scope of its caller

If a small helper does not need a scope of its own, keep `HasCircuitContext`
and remove the `OPAQUE` pragma. Its named signals are then recorded in the
scope of the caller. The plugin warns about such a function, in the category
`x-circuit-context-uninstrumented`. If this is what you want for all such
helpers, see [How to control the warnings of the plugin](warnings.md).

## Name instances that come from one call site

Instances made by `fmap` or `map` over a vector all come from one call site,
so the plugin cannot order them. Use `imapComponents`. It makes element `i`
of the vector a component with the name `<name>_<i>`:

```haskell,clash group=lanes hidden
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}
module Lanes where

import Clash.Explicit.Prelude
import Clash.CircuitContext
import BookSupport
```

```haskell,clash group=lanes
-- | One lane: a register. No OPAQUE, so it is recorded in the scope that
-- imapComponents makes for it.
lane :: (HasCircuitContext) => Index 2 -> Signal System Int -> Signal System Int
lane _ s = held
 where
  held = register clockGen resetGen enableGen 0 s
{-# ANN lane NoCircuitScope #-}

lanes :: (HasCircuitContext) => Signal System (Vec 2 Int) -> Signal System (Vec 2 Int)
lanes inp = bundle outs
 where
  outs = imapComponents "lane" lane (unbundle inp)
{-# OPAQUE lanes #-}

>>> printWaveform 3 (lanes (pure (1 :> 2 :> Nil)))
lanes
  outs_1
  outs_0
  lane_0
    held
  lane_1
    held
```

The function you give `imapComponents` gets the index of its element, and
its named signals go in the scope of that element. `lane` has no `OPAQUE`
pragma, or each lane would have a second scope, `lane_0.lane`. The
`NoCircuitScope` annotation says that this is deliberate, so the plugin does
not warn about it (see [How to control the warnings of the
plugin](warnings.md)).

`outs` is a vector, so it is recorded element by element: `outs_0` and
`outs_1`.
