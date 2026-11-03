# Waveform files

A waveform is a pair of files: a VCD file and a JSON sidecar. While a
simulation records, a third file holds its values. This page describes
where the files go, their names, and their content.

## Place and names

| File | Path | Life |
|---|---|---|
| VCD | `waveforms/<name>.vcd` | kept |
| Sidecar | `waveforms/<name>.json`, beside the VCD | kept |
| Spill file | `waveforms/<name>.spill` | while the simulation records. Deleted when the waveform is written |

`<name>` is the name that you give to `writeWaveform`, `withWaveform` or
`simulateWaveform`. One name is one pair of files: two tests must not use
the same name. The directory `waveforms` is relative to the working
directory of the process. The working directory of `cabal test` and of
`cabal run` can differ. When a function writes a waveform for a failed test,
the message gives the absolute path.

## How the files are written

- The values of each recorded signal go to the spill file while the
  simulation runs. The VCD is written from the spill file, so a long
  simulation needs about as much memory as a short one. It needs disk space
  instead, about the size of the VCD.
- Each file is written to a temporary file first, and then renamed.
- The sidecar is renamed first. If a `.vcd` file exists, its `.json` file is
  complete. A crash between the two renames can leave a new `.json` file
  beside the `.vcd` file of an earlier run.
- A sidecar entry that fails to render (a `Waveform` instance whose width is
  not `BitSize`) is left out, with a message on stderr. The VCD still has the
  signal.
- If nothing was recorded, no file is written.

## Which functions delete files

| Function | Writes | Deletes |
|---|---|---|
| `writeWaveform` | always | nothing |
| `withWaveform` | when the check fails | the waveform of an earlier failed run, when the check passes |
| `simulateWaveform` | the counterexample, and with `AtSize` one passing case | nothing |

`withWaveform` knows a waveform of a failed run by its `$comment` line (see
below). It does not delete a waveform without that line.

## The VCD

| Item | Content |
|---|---|
| `$comment` | `No comment`, or `clash-circuit-context: the run of a failing test` for the waveform of a failed run |
| `$timescale` | the greatest common divisor of the clock periods of the recorded signals, in ps |
| Time | cycle *c* of a domain with period *p* is at time *c* × *p* / timescale. For one domain, one unit is one cycle |
| `$dumpvars` | the values at the time of `start` |
| Scopes | one `$scope module` for each component instance, nested as the components are |
| Order in a scope | First the signals, in the order of their bindings in the source. Then the child scopes, by name, with numbers compared as numbers (`lane_2` before `lane_10`) |
| Signals | one `$var wire` for each recorded signal, with the name of its binding |
| Sibling instances | `name_0`, `name_1`, … in the order of the call sites of their `component` in the source. Some instances have one call site: each instance of a plugin-made component, and a `map` over a vector. These are in the order in which the simulation first reaches them |
| Elements of a `Vec` | `name_0`, `name_1`, … |
| Fields of a record | `name_field` |
| A name outside each component | in the scope `logic` |
| A cycle that was not evaluated | the value `z` |
| A value that is undefined | the value `x`, with the bits that are defined kept |
| Traces with the same history | one VCD identifier for all of them |

The VCD has no other dependency. Each viewer can open it without the
sidecar.

The window `(start, stop)` of the functions in
`Clash.CircuitContext.Waveform` is in cycles of the domain of the signal.
The window of `writeVCD` in `Clash.CircuitContext.Recording` is in units of
the timescale. The window ends at the end of the simulation at the latest.

## The sidecar

The sidecar is in the format of clash-shockwaves. Its key `signals` has one
entry for each described signal. The key of each entry is the full path of
the signal in the VCD, for example `top.counter_0.count`, or `logic.out` for
a name outside each component. The Surfer plugin of clash-shockwaves reads
the sidecar unchanged.

## Write the files yourself

`Clash.CircuitContext.Recording` records a simulation and writes its files:

| Function | Does |
|---|---|
| `withRecording spill action` | runs `action` with recording into the spill file `spill`, and returns what the action returned or threw, with the `Recording` |
| `writeVCD (start, stop) rec path` | writes the VCD at `path` and the sidecar beside it |
| `recordingEnd rec` | the time at which the simulation stopped, in units of the timescale |
| `discardRecording rec` | deletes the spill file |
| `recordedSignals`, `recordedChanges` | what was recorded, for tests |
