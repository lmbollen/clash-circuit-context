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

The top entity itself has no `HasCircuitContext`, so Clash can use it. When
the book is built, Clash generates Verilog for this `topEntity`. The
instrumentation adds no registers and no logic to the HDL:

- Each recording function (`component`, the traces and the probes) is the
  identity in HDL. `withoutCircuitContext` is also the identity, and
  `imapComponents` is an ordinary `imap`.
- `mealyProbed` is an ordinary `mealy`. The cycle counter that it uses in
  simulation is not in the HDL.
- Each `OPAQUE` function is a module of its own in the HDL, so the
  components of the waveform are also the modules of the HDL.

For the reason, see [About the cost of recording](../explanation/cost.md).

## Get a `clash` executable

The `clash` executable comes from the package `clash-ghc`. It must be built
from the same commit as your clash-prelude. If you pin clash-prelude as in
[How to add clash-circuit-context to a project](add-to-project.md), add
`clash-lib` and `clash-ghc` to the `subdir` of that pin:

```cabal
source-repository-package
  type: git
  location: https://github.com/clash-lang/clash-compiler.git
  tag: e16599387a68d6361134b4fe7e63bf4ad3ae8408
  subdir:
    clash-prelude
    clash-lib
    clash-ghc
```

Then build it, together with your package:

```text
cabal build clash-ghc:exe:clash <your-package>
```

The first build takes a long time.

## Run `clash` on a module of your package

This is the recommended way. Build your package with the plugin, as in
[How to add clash-circuit-context to a
project](add-to-project.md), and run `clash` in the environment of the
project:

```text
cabal exec -- clash -package <your-package> <Module> --verilog
```

- `-package <your-package>` is necessary: `cabal exec` does not expose the
  packages of the project itself.
- If you build with another GHC than the default one, for example with
  `cabal build -w ghc-9.10.3`, give `cabal exec` the same `-w`:
  `cabal exec -w ghc-9.10.3 -- clash …`. Otherwise it uses the environment
  of the default GHC.
- Clash reads the code of a module of a package from its interface file.
  Build the package with `-fexpose-all-unfoldings -fno-worker-wrapper` in
  its `ghc-options`, as the Clash starter projects do.
- The module was compiled with the plugin when the package was built, so
  you do not give the plugin again.

## Run `clash` on a source file

If you give a source file to `clash`, for example
`clash Design.hs --verilog`, Clash compiles it again. It does not read the
`.cabal` file, and it does not load a plugin that only an `OPTIONS_GHC`
pragma names. Give the plugin on the command line, and also the source
directories, the language extensions and the other plugins of your package:

```text
cabal exec -- clash -isrc src/Design.hs --verilog -fplugin=Clash.CircuitContext.Plugin <extensions and plugins of the package>
```

If you do not give the plugin, an instrumented design does not compile.
The error names `TurnOnTheCircuitContextPlugin` (see [If the plugin is not
on](add-to-project.md#if-the-plugin-is-not-on)). `clashi` loads the plugin
from the pragma.

Clash writes the HDL to `verilog/<Module>.topEntity/` (or `vhdl/…`). Each
component is one module with the name of its function, also when it is
called from several places. The names of instances in the HDL are Clash's,
and can differ from the names of the scopes in the waveform. A module name
with a number, such as `adder_0`, is a second variant of `adder` that Clash
made, for example for a constant argument; in the waveform, `adder_0` is the
first instance of `adder`.

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
simulation of the whole design, simulate the instrumented function with a
function of `Clash.CircuitContext.Waveform`, not the top entity.
