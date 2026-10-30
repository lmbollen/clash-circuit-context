# Your first waveform

In this tutorial, we make a small design with two counters, simulate it, and
write its waveform to a VCD file. At the end, the VCD has one scope for each
counter, and each scope holds the signals of that counter.

You need a Clash project that can use clash-circuit-context. If you do not
have one, follow [How to add clash-circuit-context to a
project](../how-to/add-to-project.md) first, and then come back here.

## Write the design

Make the file `src/Counters.hs`, with this content:

```haskell,clash group=tutorial
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}

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
  withWaveformSlot "counters" $ \slot ->
    withWaveform slot 8 (sampleN 8 (top (fromList (P.cycle [True, False, True]))))

-- | Print the part of a VCD file that declares its scopes and signals.
declarations :: FilePath -> IO ()
declarations path = do
  vcd <- readFile path
  let fromScopes = P.dropWhile (not . ("$scope" `isPrefixOf`)) (P.lines vcd)
  mapM_ putStrLn (P.takeWhile (not . ("$enddefinitions" `isPrefixOf`)) fromScopes)
```

Now load the module in a Clash session (`cabal repl`, or `clashi`), and run
the simulation:

```haskell,clash group=tutorial
>>> runCounters
[0,0,1,2,3,4,5,6]
```

The output of `top` is the sum of the two counters. The reset holds both
counters at 0 in the first cycle. After that, one of them counts in each
cycle, so the sum goes up by one in each cycle.

The simulation also wrote the file `waveforms/counters.vcd`, and a file
`waveforms/counters.json` beside it. Look at what the VCD declares:

```haskell,clash group=tutorial
>>> declarations "waveforms/counters.vcd"
$scope module top $end
  $var wire 8 ! out $end
  $scope module counter_0 $end
    $var wire 8 $ next $end
    $var wire 8 % count $end
  $upscope $end
  $scope module counter_1 $end
    $var wire 8 " next $end
    $var wire 8 # count $end
  $upscope $end
$upscope $end
```

Notice three things:

- `top` and each `counter` are scopes. This is the hierarchy of the source.
- Each named local binding (`out`, `count`, `next`) is a signal, with the
  name of the binding.
- The two calls of `counter` are two scopes, `counter_0` and `counter_1`. The
  numbers come from the order of the calls in the source.

## Look at the waveform

Open `waveforms/counters.vcd` in a waveform viewer, for example
[Surfer](https://surfer-project.org/) or GTKWave. Expand `top`, then
`counter_0` and `counter_1`. The `count` of `counter_0` goes up in the high
cycles of the input. The `count` of `counter_1` goes up in the low cycles.

## What you made

You made a design with two components, and you recorded all its named
signals without a call to a trace function. The plugin did the work, and
the simulation was an ordinary `sampleN`.

Next, read [How to keep waveforms from a test suite](../how-to/test-suite.md)
to record waveforms from tests, or [About the design
hierarchy](../explanation/hierarchy.md) to understand how the scopes are
made.
