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

Give the functions of that module no `HasCircuitContext` and no `HasProbe`.
The plugin does not change a module without these constraints, so you do
not need to turn it off for the module.

Do not turn the plugin off for a module whose functions have
`HasCircuitContext`. Their bindings are then not recorded, and they are not
components. A call of such a function from code without
`HasCircuitContext`, in a module without the plugin, fails to compile, with
an error that names `TurnOnTheCircuitContextPlugin` (see [If the plugin is
not on](add-to-project.md#if-the-plugin-is-not-on)).

## One call, and everything under it

Call it under `withoutCircuitContext`:

```haskell
topEntity clk rst = withoutCircuitContext (myInstrumentedCircuit clk rst)
```

The call then records nothing, in simulation and in HDL: no scope and no
signal under it. A binding that holds the result of the call is in the
function that makes the call, so it is still recorded there, as each other
binding of that function. This is also how a top entity calls an
instrumented design: see [How to generate HDL from an
instrumented design](hdl.md).
