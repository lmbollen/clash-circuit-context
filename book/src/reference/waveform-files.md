# Waveform files

A waveform is a pair of files: a waveform file, in the format FST or VCD,
and a JSON sidecar. While a simulation records, its values wait in memory
or in a spill file. This page describes where the files go, their names,
and their content.

## Place and names

| File | Path | Life |
|---|---|---|
| Waveform | `waveforms/<name>.fst`, or `waveforms/<name>.vcd` with `format = VCD` | kept |
| Sidecar | `waveforms/<name>.json`, beside the waveform | kept |
| Spill file | `waveforms/<name>.spill`, only with `spill = OnDisk` | while the simulation records. Deleted when the waveform is written. Kept when the run stops first |

`<name>` is the name that you give to `writeWaveform`, `withWaveform` or
`simulateWaveform`. One name is one pair of files: two tests must not use
the same name. The directory `waveforms` is relative to the working
directory of the process. The working directory of `cabal test` and of
`cabal run` can differ. When a function writes a waveform for a failed test,
or keeps the spill file of a stopped run, the message gives the absolute
path.

## How the files are written

- A recorder keeps the changes of each signal as compact records. With
  `spill = InMemory` (the default), the records are in memory. With
  `spill = OnDisk`, they go to the spill file while the simulation runs.
- The waveform is written from the records when the simulation ends.
- Each file is written to a temporary file first, and then renamed.
- The sidecar is renamed first. If a waveform file exists, its `.json` file
  is complete. A crash between the two renames can leave a new `.json` file
  beside the waveform file of an earlier run.
- The description of a signal for the sidecar is made when the signal is
  first recorded. A description that fails to render (a `Waveform` instance
  whose width is not `BitSize`) is left out of the sidecar, with a message
  on stderr. The waveform still has the signal.
- If nothing was recorded, no file is written.

## Which functions delete files

| Function | Writes | Deletes |
|---|---|---|
| `writeWaveform` | always | nothing |
| `withWaveform` | when the check fails | the waveform of an earlier failed run, when the check passes |
| `simulateWaveform` | the counterexample, and with `AtSize` one passing case | nothing |
| `recoverWaveform` | the waveform of a spill file | the spill file, after it wrote the waveform |
| `spill2fst`, `spill2vcd` | the waveform of each spill file | with `--delete`, each spill file after its waveform is written |

`withWaveform` knows the waveform of a failed run by its comment: the
`$comment` line of a VCD file, or the comment of an FST file (see below). It
does not delete a waveform without that comment, in either format.

## The waveform

These rows are true for both formats.

| Item | Content |
|---|---|
| Comment | `No comment`, or `clash-circuit-context: the run of a failing test` for the waveform of a failed run |
| Values at `start` | the value of each signal at the time of `start` |
| Scopes | one scope for each component instance, nested as the components are |
| A component with no recorded signal under it | no scope: an empty scope is not written |
| Order in a scope | First the signals, in the order of the position of their recorder in the source file (see below). Then the child scopes, by name, with numbers compared as numbers (`lane_2` before `lane_10`) |
| Signals | one wire for each recorded signal, with the name of its binding |
| Sibling instances | `name_0`, `name_1`, … in the order of the call sites of their `component` in the source. Some instances have one call site: each instance of a plugin-made component, and a `map` over a vector. These are in the order in which the simulation first reaches them |
| Elements of a `Vec` | `name_0`, `name_1`, … |
| Fields of a record | `name_field` |
| A name outside all components | in the scope `logic` |
| A signal that the simulation never evaluated | not in the waveform at all |
| A cycle that was not evaluated | the value `z`, for example the cycles after the last cycle that a check read |
| A value that is undefined | the value `x`, with the bits that are defined kept |
| Traces with the same history | one set of values for all of them: one VCD identifier, or FST aliases |

The waveform file has no other dependency. Each viewer can open it without
the sidecar.

### Time

| Format | Unit | Cycle *c* of a domain with period *p* |
|---|---|---|
| VCD | the `$timescale`: the greatest common divisor of the clock periods of the recorded signals, in ps | at *c* × *p* / timescale. For one domain, one unit is one cycle |
| FST | 1 ps | at *c* × *p* |

The windows of the functions are not in these units. See [The
window](#the-window).

### The order of the signals in a scope

The position of a signal is the position of its recorder in the source
file: for a binding that the plugin records, the position of the binding.

- The signals of one function come in the order of their bindings.
- A helper without `OPAQUE` records into the scope of its caller. Its
  signals have the positions of its own bindings. If the helper is above the
  caller in the file, its signals come first, wherever the call is. If the
  helper is in another file, its signals are in a group of their own,
  before or after the signals of the caller.
- The fields of a record have the position of their binding, and then come
  in the order of their declaration, also inside a nested record.
- The elements of a `Vec` have the position of their binding, and come in
  the order of their index: `outs_2` before `outs_10`.
- Signals without a position come last, by name.

### The window

Each window `(start, stop)` is `[start, stop)`: it has `start`, and it does
not have `stop`.

- The window of the functions in `Clash.CircuitContext.Waveform` is in
  cycles of the domain of the signal.
- The window of `writeFST`, `writeVCD` and `writeSpill` in
  `Clash.CircuitContext.Recording`, and of the executables, is in units of
  the VCD timescale (see [Time](#time)), for both formats. For one domain,
  one unit is one cycle.
- The window ends at the end of the simulation at the latest.

## The sidecar

The sidecar is in the format of clash-shockwaves. Its key `signals` has one
entry for each described signal. The key of each entry is the full path of
the signal in the waveform, for example `top.counter_0.count`, or
`logic.out` for a name outside all components. The Surfer plugin of
clash-shockwaves reads the sidecar unchanged. It finds the sidecar by the
path of the waveform file, with the extension `.json`.

## The spill file

A spill file describes itself, so that the file of a run that was stopped
can be read without the run. It is binary:

- A header: the 8 bytes `CCCSPILL`, a version byte, and the time from which
  the run recorded.
- Then blocks. Each block has a tag, a length, and its content:
  - a signal: its name, kind (trace or probe), clock period, width, and its
    description for the sidecar;
  - a chunk: up to 16 KiB of the value changes of one signal;
  - the entries of a lookup table of the sidecar, made when a value first
    occurs;
  - the end of the run.

A signal comes before its chunks. A block is written whole, also when the
run is cancelled. A run that is killed can leave the last block cut off:
a reader ignores it, and reads the file up to the last whole block.

Read a spill file with `recoverWaveform`, `recoverRecording`, `writeSpill`,
or the executables `spill2fst` and `spill2vcd`. See [How to keep the
waveform of a stopped run](../how-to/recover.md).

## Write the files yourself

`Clash.CircuitContext.Recording` records a simulation and writes its files:

| Function | Does |
|---|---|
| `withRecording spill action` | runs `action` with recording, and returns what the action returned or threw, with the `Recording`. `spill` is `Just path` for a spill file at `path` (its directory is made), or `Nothing` to keep the values in memory |
| `writeFST (start, stop) rec path` | writes the FST file at `path` and the sidecar beside it. Returns `Left` with the reason when it writes nothing |
| `writeVCD (start, stop) rec path` | the same, as a VCD file |
| `recordingStart rec`, `recordingEnd rec` | the time from which the recording has cycles, and the time at which the simulation stopped, in units of the timescale |
| `discardRecording rec` | deletes the spill file, if the recording has one |
| `recoverRecording path` | reads a spill file as a `Recording`, also of a run that was stopped |
| `writeSpill format window spill path` | writes the waveform of a spill file in one step. With the window `Nothing`, all cycles that the run recorded |
| `recordedSignals`, `recordedChanges` | what was recorded, for tests |
