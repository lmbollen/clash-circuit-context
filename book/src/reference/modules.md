# Modules

The Haddock documentation of each module is the full reference for its
functions. This page lists the modules, and the functions you use most.

| Module | Contains |
|---|---|
| `Clash.CircuitContext` | Everything for a design: re-exports `Clash.CircuitContext.Core`, `Traceable` and `NonTranslatable` |
| `Clash.CircuitContext.Core` | The runtime: the context, components, traces, probes, the dump to VCD |
| `Clash.CircuitContext.Auto` | The classes `Traceable` and `Probeable`, and what the plugin inserts (`autoTrace`, `autoProbe`) |
| `Clash.CircuitContext.Waveform` | Waveforms from a test suite: slots, and when to record |
| `Clash.CircuitContext.Waveform.Hedgehog` | Waveforms from Hedgehog properties |
| `Clash.CircuitContext.Shockwaves` | The sidecar for typed waveforms (`dumpVCDSW`) |
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
| `withCircuitContext` | run a simulation with recording, and return its trace and probe maps |
| `dumpVCDC` | the VCD of the maps |
| `dumpVCDSW` | the VCD and its sidecar |
| `withWaveformSlot`, `withWaveform`, `withWaveformLazy` | record a test and write its files |
| `withWaveformWhen`, `waveformsRequested` | record only when you ask |
| `withWaveformOnFailure`, `withWaveformOnFailure'` | keep only the waveform of a failed check |
| `withWaveformOnCounterexample`, `withWaveformCase` | the same for a Hedgehog property |
