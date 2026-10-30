# How to leave a binding or a function out

The plugin changes only functions whose signature has `HasCircuitContext` or
`HasProbe`. This guide shows how to leave out less than that: one binding,
one function, one module, or one call.

## One binding

Start its name with `_`:

```haskell,clash group=optout hidden
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}
module OptOut where

import Clash.Explicit.Prelude
import Clash.CircuitContext
import BookSupport
```

```haskell,clash group=optout
scaled :: (HasCircuitContext) => Signal System Int -> Signal System Int
scaled inp = out
 where
  _doubled = inp + inp
  out = _doubled + inp
{-# OPAQUE scaled #-}

>>> printWaveform 2 (scaled (pure 1))
scaled
  out
```

## One function

- To remove its scope, but keep its signals in the scope of the caller,
  remove the `OPAQUE` pragma.
- To leave it out completely, remove `HasCircuitContext` from its signature.

## One module

Do not turn the plugin on for that module: use the `OPTIONS_GHC` pragma in
the modules that need it, not `ghc-options` for the whole package. You
usually do not need this. A module without `HasCircuitContext` or `HasProbe`
in its signatures is not changed by the plugin.

## One call, and everything under it

Call it under `withoutCircuitContext`:

```haskell
topEntity clk rst = withoutCircuitContext (myInstrumentedCircuit clk rst)
```

The call then records nothing, in simulation and in HDL. This is also how a
top entity calls an instrumented design: see [How to generate HDL from an
instrumented design](hdl.md).
