# Modules

The Haddock documentation of each module is the full reference for its
functions. This page lists the modules, and the functions you use most.

| Module | Contains |
|---|---|
| `Clash.CircuitContext` | Everything for a design: re-exports `Clash.CircuitContext.Core`, `Traceable` and `NonTranslatable` |
| `Clash.CircuitContext.Core` | The runtime: the context, components, traces, probes |
| `Clash.CircuitContext.Auto` | The classes `Traceable` and `Probeable`, and what the plugin inserts (`autoTrace`, `autoProbe`) |
| `Clash.CircuitContext.Waveform` | Waveforms from IO code and from a test suite |
| `Clash.CircuitContext.Waveform.Hedgehog` | Waveforms from Hedgehog properties |
| `Clash.CircuitContext.Recording` | Recording by hand, for a design with more than one domain |
| `Clash.CircuitContext.Plugin` | The GHC plugin. Use it as `-fplugin=Clash.CircuitContext.Plugin` |

## In a design

| Name | Use |
|---|---|
| `HasCircuitContext` | the constraint of an instrumented function |
| `HasProbe` | the constraint of a step function whose values are probed |
| `component` | give an expression its own scope, with a name that you choose |
| `imapComponents` | a vector of instances, with the scopes `name_0`, `name_1`, … |
| `traceSignalC`, `traceSignalSW` | record a signal by hand: as bits, or described |
| `probe`, `probeSW` | record a value in a step function by hand |
| `mealyProbed`, `mooreProbed`, `mealyBProbed`, `mooreBProbed` | mealy and Moore machines whose step function is probed |
| `withoutCircuitContext` | supply a context that records nothing |
| `NoCircuitScope` | an annotation: this function deliberately has no `OPAQUE` |

## In a simulation

| Name | Use |
|---|---|
| `writeWaveform` | simulate a design and always write its waveform |
| `withWaveform` | run the check of a test, and write the waveform only when the check fails |
| `simulateWaveform`, `Capture` | the same for a Hedgehog property: the counterexample, or one case of a size |
| `withRecording`, `writeVCD`, `discardRecording` | record a simulation by hand, and write its files |
