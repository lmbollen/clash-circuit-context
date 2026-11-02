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
not need anything else. The instrumentation adds no registers and no logic to
the HDL:

- Each recording function (`component`, the traces and the probes) is the
  identity in HDL.
- `mealyProbed` is an ordinary `mealy`. The cycle counter that it uses in
  simulation is not in the HDL.

For the reason, see [About the cost of recording](../explanation/cost.md).

## Run `clash` on a source file

If you give a source file to the `clash` executable, for example
`clash Design.hs --verilog`, Clash does not load a plugin that only an
`OPTIONS_GHC` pragma names. Give the plugin on the command line:

```text
clash Design.hs --verilog -fplugin=Clash.CircuitContext.Plugin
```

If you do not, a design with an empty `Traceable` instance does not compile.
A module that `clash` loads from a package that was built with the plugin is
not affected. `clashi` loads the plugin from the pragma.

## Compare with the HDL of the design without instrumentation

The HDL does the same function as the HDL of the design without
instrumentation, but its text is different:

- The plugin moves the `where` bindings of a component into a `let` inside
  `component`. The names of some wires, and the order of the declarations,
  can therefore be different.
- A recorded `Vec` binding can give a generate loop that only copies each
  element.

Do not compare the two HDL files as text. Use an equivalence checker, for
example the `equiv_make`, `equiv_induct` and `equiv_status` commands of
Yosys.

## If you also simulate the top entity

`withoutCircuitContext` also turns recording off in simulation. To record a
simulation of the whole design, simulate the instrumented function under
`withCircuitContext` (or a helper of `Clash.CircuitContext.Waveform`), not
the top entity.
