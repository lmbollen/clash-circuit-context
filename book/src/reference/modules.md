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
| `Clash.CircuitContext.Recording` | Recording by hand, for a design with more than one domain, and spill files |
| `Clash.CircuitContext.Plugin` | The GHC plugin. Use it as `-fplugin=Clash.CircuitContext.Plugin` |

`Waveform` (for described signals) is not in these modules. It is the class
of clash-shockwaves: import it from `Clash.Shockwaves.Waveform`, and add
`clash-shockwaves` to `build-depends`.

## In a design

From `Clash.CircuitContext`.

### Constraints and annotations

| Name | Use |
|---|---|
| `HasCircuitContext` | the constraint of an instrumented function. It asks for the context, for the site of each call, and for the plugin to be on |
| `CircuitContextOnly` | the context, without the call sites: what the functions of the package ask for, and give their arguments. Write it in code that you instrument by hand and compile without the plugin |
| `TurnOnTheCircuitContextPlugin` | a constraint that only the plugin solves. A function defined in a module without the plugin keeps it, and a call of that function is an error that names it |
| `HasProbe` | the constraint of a step function whose values are probed |
| `NoCircuitScope` | an annotation, `{-# ANN f NoCircuitScope #-}`: this function deliberately has no `OPAQUE` |

### Components

```haskell
-- Give an expression its own scope, with a name that you choose.
component :: (HasCallStack, CircuitContextOnly) => String -> ((CircuitContextOnly) => r) -> r

-- Make element i of a vector a component with the name <name>_<i>.
imapComponents ::
  (CircuitContextOnly, KnownNat n) =>
  String -> ((CircuitContextOnly) => Index n -> a -> b) -> Vec n a -> Vec n b

-- Supply a context that records nothing.
withoutCircuitContext :: ((CircuitContextOnly) => r) -> r
```

### Traces

```haskell
-- Record a signal by hand, as bits.
traceSignalC ::
  (HasCallStack, CircuitContextOnly, KnownDomain dom, BitPack a, NFDataX a) =>
  String -> Signal dom a -> Signal dom a

-- The same, and describe it in the sidecar.
traceSignalSW ::
  (HasCallStack, CircuitContextOnly, KnownDomain dom, BitPack a, NFDataX a, Waveform a) =>
  String -> Signal dom a -> Signal dom a

-- The method of Traceable: record any traceable value by hand (a signal, a
-- record of signals, a vector, a tuple). This is what the plugin applies to
-- a binding.
traceNamed :: (Traceable t, HasCallStack, CircuitContextOnly) => String -> t -> t
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
  (HasCallStack, CircuitContextOnly, KnownDomain dom, NFDataX s) =>
  Clock dom -> Reset dom -> Enable dom ->
  ((HasProbe) => s -> i -> (s, o)) -> s -> Signal dom i -> Signal dom o

mooreProbed ::
  (HasCallStack, CircuitContextOnly, KnownDomain dom, NFDataX s) =>
  Clock dom -> Reset dom -> Enable dom ->
  ((HasProbe) => s -> i -> s) -> (s -> o) -> s -> Signal dom i -> Signal dom o
```

`mealyBProbed` and `mooreBProbed` have the same arguments, with
`Unbundled dom i` and `Unbundled dom o` for the input and the output.

## In a simulation

### `Clash.CircuitContext.Waveform`

```haskell
data WaveformOptions = WaveformOptions {format :: Format, spill :: Spill}
data Format = VCD | FST   -- also exported by Clash.CircuitContext.Recording
data Spill = InMemory | OnDisk

-- FST, recorded in memory.
defaultOptions :: WaveformOptions

-- Simulate stop cycles, write waveforms/<name>.fst (or .vcd) and .json, and
-- return the stop samples.
writeWaveform ::
  (KnownDomain dom, NFDataX a) =>
  WaveformOptions -> String -> (Int, Int) -> ((CircuitContextOnly) => Signal dom a) -> IO [a]

-- Run the check without recording. If it throws, run it again with
-- recording, write the waveform, and throw the same exception.
withWaveform ::
  (KnownDomain dom, NFDataX a) =>
  WaveformOptions -> String -> (Int, Int) -> ((CircuitContextOnly) => Signal dom a) ->
  ([a] -> IO b) -> IO b

-- Write the waveform of a run that was stopped, from waveforms/<name>.spill.
-- Returns the path of the waveform, or why there is none.
recoverWaveform :: WaveformOptions -> String -> IO (Either String FilePath)
```

The design argument has the type `(CircuitContextOnly) => Signal dom a`:
the function supplies the context. You give it the instrumented design, and
you do not need `HasCircuitContext` yourself, or the plugin in the module of
the call.

The window `(start, stop)` is in cycles. The simulation runs `stop` cycles,
and the waveform has the cycles `start` to `stop - 1`.

For the options, see [How to keep waveforms from a test
suite](../how-to/test-suite.md#choose-the-file-and-where-the-values-wait).

### `Clash.CircuitContext.Waveform.Hedgehog`

This module also exports the options of `Clash.CircuitContext.Waveform`.

```haskell
data Capture = Counterexample | AtSize Size

-- The same as writeWaveform, in a property. Records only as Capture says.
simulateWaveform ::
  (KnownDomain dom, NFDataX a) =>
  WaveformOptions -> Capture -> String -> (Int, Int) ->
  ((CircuitContextOnly) => Signal dom a) -> PropertyT IO [a]
```

### `Clash.CircuitContext.Recording`

```haskell
-- Run the action with recording: into the spill file at the path, or, with
-- Nothing, in memory. Returns what the action returned or threw, and the
-- recording.
withRecording ::
  Maybe FilePath -> ((CircuitContextOnly) => IO r) -> IO (Either SomeException r, Recording)

-- Write the waveform at the path, and the sidecar beside it. Left: nothing
-- was written, and why.
writeFST :: (Natural, Natural) -> Recording -> FilePath -> IO (Either String ())
writeVCD :: (Natural, Natural) -> Recording -> FilePath -> IO (Either String ())

-- The time from which the recording has cycles, and the time at which the
-- simulation stopped, in units of the timescale.
recordingStart :: Recording -> Natural
recordingEnd :: Recording -> Natural

-- Delete the spill file, if the recording has one.
discardRecording :: Recording -> IO ()

-- Read a spill file, also of a run that was stopped.
recoverRecording :: FilePath -> IO (Either String Recording)

-- Write the waveform of a spill file, in this format, at the second path,
-- over the window, or everything with Nothing. The spill file is kept.
-- Right: the window that the waveform shows.
writeSpill ::
  Format -> Maybe (Natural, Natural) -> FilePath -> FilePath -> IO (Either String (Natural, Natural))
```

`withRecording` is also exported by `Clash.CircuitContext`. The window of
`writeFST`, `writeVCD` and `writeSpill` is `[start, stop)` in units of the
timescale, not in cycles: see [Waveform files](waveform-files.md#the-window).

The package also has two executables, `spill2fst` and `spill2vcd`: see
[Executables](executables.md).
