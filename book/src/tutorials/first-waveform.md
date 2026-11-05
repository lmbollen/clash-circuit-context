# Your first waveform

In this tutorial, we make a small design with two counters, simulate it, and
write its waveform to a VCD file. At the end, the VCD has one scope for each
counter, and each scope holds the signals of that counter.

You need GHC and Cabal, in a version from
[Requirements](../reference/requirements.md).

## Make the project

Make a directory `counters`, with these two files.

`cabal.project`: the package is not on Hackage, so take it from Git. For
`<commit>`, use the full hash, of 40 characters, of a commit of its main
branch (Cabal does not accept a short hash). The second package is the
clash-prelude that the package is tested with. Copy its `tag` and the
`index-state` from the [`cabal.project` of the
package](https://github.com/lmbollen/clash-circuit-context/blob/main/cabal.project):

```cabal
packages: .

source-repository-package
  type: git
  location: https://github.com/lmbollen/clash-circuit-context.git
  tag: <commit>

source-repository-package
  type: git
  location: https://github.com/clash-lang/clash-compiler.git
  tag: e16599387a68d6361134b4fe7e63bf4ad3ae8408
  subdir: clash-prelude

index-state: 2026-09-23T00:00:00Z
```

`counters.cabal`: one library, with the plugin on for the whole package,
and the language extensions, type-checker plugins and flags that Clash code
uses:

```cabal
cabal-version: 2.4
name:          counters
version:       0.1.0.0

library
  hs-source-dirs:     src
  exposed-modules:    Counters
  default-language:   Haskell2010
  default-extensions:
    BinaryLiterals
    DataKinds
    DeriveAnyClass
    DeriveGeneric
    FlexibleContexts
    NoImplicitPrelude
    NoStarIsType
    ScopedTypeVariables
    TypeApplications
    TypeFamilies
    TypeOperators
  ghc-options:
    -fplugin=GHC.TypeLits.Normalise
    -fplugin=GHC.TypeLits.Extra.Solver
    -fplugin=GHC.TypeLits.KnownNat.Solver
    -fplugin=Clash.CircuitContext.Plugin
    -fexpose-all-unfoldings
    -fno-worker-wrapper
  build-depends:
    base,
    clash-prelude,
    clash-circuit-context,
    ghc-typelits-extra,
    ghc-typelits-knownnat,
    ghc-typelits-natnormalise,
```

The three `GHC.TypeLits` plugins solve the type-level arithmetic of Clash
types, for example `Index 4` in a data type, or `Unsigned (n + 1)`. The last
two flags are the ones that Clash needs to make HDL from the package.

## Write the design

Make the file `src/Counters.hs`, with this content:

```haskell,clash group=tutorial
module Counters where

import Clash.Explicit.Prelude
import Clash.CircuitContext
import Clash.CircuitContext.Waveform
import Data.List (isPrefixOf)
import qualified Prelude as P

-- | Counts the cycles in which 'enable' is high.
counter :: (HasCircuitContext) => Signal System Bool -> Signal System (Unsigned 8)
counter enable = count
 where
  count = register clockGen resetGen enableGen 0 next
  next = mux enable (count + 1) count
{-# OPAQUE counter #-}

-- | Two counters: one for the high cycles, one for the low cycles.
top :: (HasCircuitContext) => Signal System Bool -> Signal System (Unsigned 8)
top enable = out
 where
  out = counter enable + counter (not <$> enable)
{-# OPAQUE top #-}
```

Notice the two annotations on each function: the `HasCircuitContext`
constraint, and the `OPAQUE` pragma. They tell the plugin to make the
function a component. Nothing else in the code is about waveforms.

## Simulate the design and write the waveform

Add these two functions to the same file:

```haskell,clash group=tutorial
-- | Simulate 'top' for eight cycles, and write waveforms/counters.vcd.
runCounters :: IO [Unsigned 8]
runCounters =
  writeWaveform
    defaultOptions{format = VCD}
    "counters"
    (0, 8)
    (top (fromList (P.cycle [True, False, True])))

-- | Print the part of a VCD file that declares its scopes and signals.
declarations :: FilePath -> IO ()
declarations path = do
  vcd <- readFile path
  let fromScopes = P.dropWhile (not . ("$scope" `isPrefixOf`)) (P.lines vcd)
  mapM_ putStrLn (P.takeWhile (not . ("$enddefinitions" `isPrefixOf`)) fromScopes)
```

`writeWaveform defaultOptions{format = VCD} "counters" (0, 8)` simulates 8
cycles of the design, writes the waveform of the cycles 0 to 7, and returns
the 8 output values.

The first argument has the options of the waveform. `defaultOptions` writes
an FST file, which is compressed and smaller. This tutorial writes a VCD
file, because a VCD file is text, and we read it below.

`top` needs `HasCircuitContext`, but `runCounters` does not have that
constraint. `writeWaveform` supplies the context: its design argument has
the type `(HasCircuitContext) => Signal dom a`. This is where the hierarchy
starts.

Now start a session with `cabal repl`, and run the simulation:

```haskell,clash group=tutorial
>>> runCounters
[0,0,1,2,3,4,5,6]
```

The output of `top` is the sum of the two counters. The output is 0 in the
first two cycles: cycle 0 shows the initial value of each register, and
cycle 1 shows the reset value, because the reset is active in cycle 0. From
cycle 2, one of the counters counts in each cycle, so the sum goes up by one
in each cycle.

The simulation also wrote the file `waveforms/counters.vcd`, and a file
`waveforms/counters.json` beside it. The directory `waveforms` is in the
working directory of the session. Look at what the VCD declares:

```haskell,clash group=tutorial
>>> declarations "waveforms/counters.vcd"
$scope module top $end
  $var wire 8 ! out $end
  $scope module counter_0 $end
    $var wire 8 % count $end
    $var wire 8 $ next $end
  $upscope $end
  $scope module counter_1 $end
    $var wire 8 # count $end
    $var wire 8 " next $end
  $upscope $end
$upscope $end
```

Notice three things:

- `top` and each `counter` are scopes. This is the hierarchy of the source.
- Each named local binding (`out`, `count`, `next`) is a signal, with the
  name of the binding. The signals of a scope come in the order of their
  bindings in the source.
- The two calls of `counter` are two scopes, `counter_0` and `counter_1`.
  The numbers come from the order of the calls in the source (see [About
  the design hierarchy](../explanation/hierarchy.md#how-instances-are-named)).

## Look at the waveform

Open `waveforms/counters.vcd` in a waveform viewer, for example
[Surfer](https://surfer-project.org/) or GTKWave. Both viewers also open FST
files. Expand `top`, then
`counter_0` and `counter_1`. The `count` of one counter goes up in the high
cycles of the input. The `count` of the other goes up in the low cycles.

## What you made

You made a design with two components, and you recorded all its named
signals without a call to a trace function. The plugin did the work: it
wrapped the body of each function in a `component`, and each named binding
in a trace. [What the plugin does to your
code](../introduction.md#what-the-plugin-does-to-your-code) shows the
rewrite. `writeWaveform` supplied the context, simulated the design, and
wrote the file.

Next, read [How to keep waveforms from a test suite](../how-to/test-suite.md)
to record waveforms from tests, or [About the design
hierarchy](../explanation/hierarchy.md) to understand how the scopes are
made.
