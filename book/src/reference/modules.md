# Modules

The Haddock documentation of each module is the full reference for its
functions. This page lists the modules, and gives the type of each function
that you use most.

| Module | Contains |
|---|---|
| `Clash.CircuitContext` | Everything for a design: re-exports `Clash.CircuitContext.Core`, `Traceable` and `NonTranslatable` |
| `Clash.CircuitContext.Core` | The runtime: the context, components, traces, probes |
| `Clash.CircuitContext.Auto` | The classes `Traceable` and `Probeable`, and what the plugin inserts (`autoTrace`, `autoProbe`) |
| `Clash.CircuitContext.Waveform` | Waveforms from IO code and from a test suite |
| `Clash.CircuitContext.Waveform.Hedgehog` | Waveforms from Hedgehog properties |
| `Clash.CircuitContext.Recording` | Recording by hand, for a design with more than one domain |
| `Clash.CircuitContext.Plugin` | The GHC plugin. Use it as `-fplugin=Clash.CircuitContext.Plugin` |

`Waveform` (for described signals) is not in these modules. It is the class
of clash-shockwaves: import it from `Clash.Shockwaves.Waveform`, and add
`clash-shockwaves` to `build-depends`.

## In a design

From `Clash.CircuitContext`.

### Constraints and annotations

| Name | Use |
|---|---|
| `HasCircuitContext` | the constraint of an instrumented function |
| `HasProbe` | the constraint of a step function whose values are probed |
| `NoCircuitScope` | an annotation, `{-# ANN f NoCircuitScope #-}`: this function deliberately has no `OPAQUE` |

### Components

```haskell
-- Give an expression its own scope, with a name that you choose.
component :: (HasCallStack, HasCircuitContext) => String -> ((HasCircuitContext) => r) -> r

-- Make element i of a vector a component with the name <name>_<i>.
imapComponents ::
  (HasCircuitContext, KnownNat n) =>
  String -> ((HasCircuitContext) => Index n -> a -> b) -> Vec n a -> Vec n b

-- Supply a context that records nothing.
withoutCircuitContext :: ((HasCircuitContext) => r) -> r
```

### Traces

```haskell
-- Record a signal by hand, as bits.
traceSignalC ::
  (HasCallStack, HasCircuitContext, KnownDomain dom, BitPack a, NFDataX a) =>
  String -> Signal dom a -> Signal dom a

-- The same, and describe it in the sidecar.
traceSignalSW ::
  (HasCallStack, HasCircuitContext, KnownDomain dom, BitPack a, NFDataX a, Waveform a) =>
  String -> Signal dom a -> Signal dom a

-- The method of Traceable: record any traceable value by hand (a signal, a
-- record of signals, a vector, a tuple). This is what the plugin applies to
-- a binding.
traceNamed :: (Traceable t, HasCallStack, HasCircuitContext) => String -> t -> t
```

`traceSignalC` and `traceNamed` take their requirements as real
constraints. A type that cannot be recorded is a type error, not a warning.

### Probes

```haskell
-- Record a value in a step function by hand, as bits.
probe :: (HasProbe, BitPack a, NFDataX a) => String -> a -> a

-- The same, and describe it in the sidecar.
probeSW :: (HasProbe, BitPack a, NFDataX a, Waveform a) => String -> a -> a

-- mealy and Moore machines whose step function is probed. Only the function
-- with HasProbe is probed: the output function of a Moore machine is not.
mealyProbed ::
  (HasCallStack, HasCircuitContext, KnownDomain dom, NFDataX s) =>
  Clock dom -> Reset dom -> Enable dom ->
  ((HasProbe) => s -> i -> (s, o)) -> s -> Signal dom i -> Signal dom o

mooreProbed ::
  (HasCallStack, HasCircuitContext, KnownDomain dom, NFDataX s) =>
  Clock dom -> Reset dom -> Enable dom ->
  ((HasProbe) => s -> i -> s) -> (s -> o) -> s -> Signal dom i -> Signal dom o
```

`mealyBProbed` and `mooreBProbed` have the same arguments, with
`Unbundled dom i` and `Unbundled dom o` for the input and the output.

## In a simulation

### `Clash.CircuitContext.Waveform`

```haskell
-- Simulate stop cycles, write waveforms/<name>.vcd and .json, and return the
-- stop samples.
writeWaveform ::
  (KnownDomain dom, NFDataX a) =>
  String -> (Int, Int) -> ((HasCircuitContext) => Signal dom a) -> IO [a]

-- Run the check without recording. If it throws, run it again with
-- recording, write the waveform, and throw the same exception.
withWaveform ::
  (KnownDomain dom, NFDataX a) =>
  String -> (Int, Int) -> ((HasCircuitContext) => Signal dom a) -> ([a] -> IO b) -> IO b
```

The design argument has the type `(HasCircuitContext) => Signal dom a`: the
function supplies the context. You give it the instrumented design, and you
do not need `HasCircuitContext` yourself.

The window `(start, stop)` is in cycles. The simulation runs `stop` cycles,
and the waveform has the cycles `start` to `stop - 1`.

### `Clash.CircuitContext.Waveform.Hedgehog`

```haskell
data Capture = Counterexample | AtSize Size

-- The same as writeWaveform, in a property. Records only as Capture says.
simulateWaveform ::
  (KnownDomain dom, NFDataX a) =>
  Capture -> String -> (Int, Int) -> ((HasCircuitContext) => Signal dom a) -> PropertyT IO [a]
```

### `Clash.CircuitContext.Recording`

```haskell
-- Run the action with recording into the spill file. Returns what the
-- action returned or threw, and the recording.
withRecording :: FilePath -> ((HasCircuitContext) => IO r) -> IO (Either SomeException r, Recording)

-- Write the VCD at the path, and the sidecar beside it. Left: nothing was
-- written, and why.
writeVCD :: (Int, Int) -> Recording -> FilePath -> IO (Either String ())

-- The time at which the simulation stopped, in units of the timescale.
recordingEnd :: Recording -> Int

-- Delete the spill file.
discardRecording :: Recording -> IO ()
```

`withRecording` is also exported by `Clash.CircuitContext`. The window of
`writeVCD` is `[start, stop)` in units of the VCD timescale, not in cycles:
see [Waveform files](waveform-files.md#the-window).
