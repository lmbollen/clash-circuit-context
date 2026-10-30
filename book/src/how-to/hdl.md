# How to generate HDL from an instrumented design

An instrumented function has the constraint `HasCircuitContext`, and Clash
cannot use a top entity with a constraint. This guide shows how to make a
top entity from an instrumented design.

Call the design under `withoutCircuitContext`. It supplies a context that
records nothing:

```haskell,clash group=hdl hidden
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}
module Hdl where

import Clash.Explicit.Prelude
import Clash.CircuitContext
```

```haskell,clash group=hdl topEntity=topEntity
counter :: (HasCircuitContext) => Clock System -> Reset System -> Signal System Bool -> Signal System (Unsigned 8)
counter clk rst enable = count
 where
  count = register clk rst enableGen 0 next
  next = mux enable (count + 1) count
{-# OPAQUE counter #-}

topEntity :: Clock System -> Reset System -> Signal System Bool -> Signal System (Unsigned 8)
topEntity clk rst enable = withoutCircuitContext (counter clk rst enable)
```

When the book is built, Clash generates Verilog for this `topEntity`. You do
not need anything else: the instrumentation has no effect on the HDL.

- Each recording function (`component`, the traces and the probes) is the
  identity in HDL.
- `mealyProbed` is an ordinary `mealy`. The cycle counter that it uses in
  simulation is not in the HDL.

The HDL is therefore the same as the HDL of the design without
instrumentation. For the reason, see [About the cost of
recording](../explanation/cost.md).

## If you also simulate the top entity

`withoutCircuitContext` also turns recording off in simulation. To record a
simulation of the whole design, simulate the instrumented function under
`withCircuitContext` (or a helper of `Clash.CircuitContext.Waveform`), not
the top entity.
