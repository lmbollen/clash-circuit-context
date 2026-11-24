# Waveform files

A waveform is a pair of files: a waveform file, in the format FST or VCD,
and a JSON sidecar. While a simulation records, its values wait in memory
or in a spill file. This page describes where the files go, their names,
and their content.

## Place and names

| File | Path | Life |
|---|---|---|
| Waveform | `<path>.fst`, or `<path>.vcd` with `format = VCD` | kept |
| Waveform of the passing case of `AtSize size` | `<path>_<size>.fst`, or `.vcd` | kept |
| Sidecar | `<path>.json`, beside the waveform | kept |
| Spill file | `<path>.spill`, only with `spill = OnDisk` | while the simulation records. Deleted when the waveform is written. Kept when a recording run stops first after it recorded a cycle, or when the waveform cannot be written (the message gives its path). The first runs of `withWaveform` and `simulateWaveform` do not record |

`<path>` is the path that you give to `writeWaveform`, `withWaveform`,
`simulateWaveform` or `recoverWaveform`, without an extension. For example,
`"waveforms/counter"` gives `waveforms/counter.fst`,
`waveforms/counter.json` and `waveforms/counter.spill`.

- A relative path is relative to the working directory of the process. The
  working directory of `cabal test` and of `cabal run` can differ.
- An absolute path is the same from each working directory.
- The functions make the missing directories of the path.
- The functions refuse a path that cannot be the path of a waveform, before
  the simulation runs: an empty path, a path that names a directory (it
  ends in `/`, `.` or `..`), and a path that ends in `.fst`, `.vcd`,
  `.json` or `.spill`. `writeWaveform`, `withWaveform` and
  `simulateWaveform` throw an error; `recoverWaveform` returns `Left`. The
  error says which path to give.
- A message that names a file gives its absolute path. `writeWaveform`,
  and `simulateWaveform` for the passing case of `AtSize`, write their files
  without a message. `recoverWaveform` returns the absolute path of the
  waveform.
- Two ways to write one path are one path: `waveforms/counter` and
  `./waveforms//counter` are the same.
- One path is one waveform. The FST and the VCD of a path have the same
  sidecar, `<path>.json`, so when a function writes a waveform in one
  format, it deletes the waveform in the other format at that path.
- Two tests must not use the same path. With `spill = OnDisk`, two
  recordings that use one spill file at the same time are an error: the
  second one stops at once, with a message, and the first one goes on.
  `withWaveform` then writes no waveform for the second test, and the test
  fails with the exception of its check. With `spill = InMemory`, the
  functions do not check this: the last waveform written stays.
- A spill file uses RAM when its path is on a file system in RAM (tmpfs),
  for example `/tmp` on many systems. Then `spill = OnDisk` does not save
  memory.

## How the files are written

- A recorder keeps the changes of each signal as compact records. With
  `spill = InMemory` (the default), the records are in memory. With
  `spill = OnDisk`, they go to the spill file while the simulation runs.
- The waveform is written from the records when the simulation ends.
- Each file is written to a temporary file first, and then renamed.
- The sidecar is renamed first. If a waveform file exists, its `.json` file
  is complete. A crash between the two renames can leave a new `.json` file
  beside the waveform file of an earlier run.
- If a file cannot be written or renamed, for example because the disk is
  full, no new file stays. An earlier waveform and an earlier `.json` file
  at the path stay as they were. The functions of
  `Clash.CircuitContext.Waveform` then say why on stderr, and keep the
  spill file; `writeFST` and `writeVCD` return the reason as `Left`.
- The description of a signal for the sidecar is made when the signal is
  first recorded. A description that fails to render (a `Waveform` instance
  whose width is not `BitSize`) is left out of the sidecar, with a message
  on stderr. The waveform still has the signal.
- If nothing was recorded, no file is written. The functions of
  `Clash.CircuitContext.Waveform` then say why on stderr; `writeFST` and
  `writeVCD` return the reason as `Left`.

## Which functions delete files

| Function | Writes | Deletes |
|---|---|---|
| `writeWaveform` | always, also when the simulation throws. Then the waveform ends with the cycle that threw | the waveform in the other format at the path |
| `withWaveform` | when the check fails | the waveform of an earlier failed run, when the check passes. When it writes, the waveform in the other format at the path |
| `simulateWaveform` | the counterexample, and with `AtSize size` one passing case, as `<path>_<size>` | when it writes, the waveform in the other format at the path |
| `recoverWaveform` | the waveform of a spill file | the spill file, after it wrote the waveform, and the waveform in the other format at the path |
| `spill2fst`, `spill2vcd` | the waveform of each spill file | with `--delete`, each spill file after its waveform is written |

`withWaveform` knows the waveform of a failed run by its sidecar (see [The
sidecar](#the-sidecar)). It reads only the sidecar, not the waveform. It
does not delete a waveform whose sidecar does not mark it, in either format.
When it deletes the waveform, it also deletes the sidecar.

## The waveform

These rows are true for both formats.

| Item | Content |
|---|---|
| Comment | `No comment`, or `clash-circuit-context: the run of a failing test` for the waveform of a failed run |
| Values at `start` | the value of each signal at the time of `start` |
| End | the end of the window: the last timestamp of a VCD, the end time of an FST. If the run ended before the end of the window, the end of the run: for example, the last cycle that the check of `withWaveform` read. A viewer shows the last cycle whole |
| Scopes | one scope for each component instance, nested as the components are |
| A component with no recorded signal under it | no scope: an empty scope is not written |
| Order in a scope | First the signals, in the order of the position of their recorder in the source file (see below). Then the child scopes, by name, with numbers compared as numbers (`lane_2` before `lane_10`) |
| Signals | one wire for each recorded signal, with the name of its binding |
| Sibling instances | `name_0`, `name_1`, … (but see [Names that are taken](#names-that-are-taken)) in the order of their calls in the source, numbered among the instances that the run evaluated: a test that evaluates fewer instances numbers them from 0 too. A single evaluated instance has no number: `name`. The instances made by one call, for example by `map` over a vector, are in the order in which the simulation first reaches them; use `imapComponents` to name them by their index. See [About the design hierarchy](../explanation/hierarchy.md#how-instances-are-named) |
| An operator that is a component | its name: `<+>_0`, `<+>_1`, … |
| Elements of a `Vec` | `name_0`, `name_1`, …; a `Vec` in a `Vec`: `name_0_3` |
| Components of a tuple | `name_0`, `name_1`, … |
| Fields of a record | `name_field` |
| The bindings of a local function without a scope, for each call | `v_0`, `v_1`, … in the scope of the caller |
| A name outside all components | in the scope `logic`. This scope keeps the name `logic`, so a component named `logic` beside it is `logic_0`. Without names outside all components, a component named `logic` keeps its name |
| A signal that the simulation never evaluated, for example a binding that nothing uses | not in the waveform at all. When a run ends, the recorder evaluates each trace up to the last cycle of the run (see [The cost of recording](../explanation/cost.md)), so a trace that the run evaluated once is in the waveform |
| A cycle that was not evaluated, before the end of the run | the value `z`, for example a probe in a cycle in which nothing used its value, or a trace that the run evaluated less far than the other signals |
| A probed value that the simulation evaluated after a later cycle | at its own cycle. A value that a mealy machine keeps in its state and uses a few cycles later is evaluated late |
| A value that is undefined, or that throws an exception | the value `x`, with the bits that are defined kept |
| The bits that a constructor does not use, for example in `Nothing` of a `Maybe (BitVector 8)` | the value `x`, as Clash packs them with `BitPack`, also when the value is defined. The sidecar describes the type, so a viewer that reads the sidecar can show the constructor |
| Traces with the same history | one set of values for all of them: one VCD identifier, or FST aliases |

The waveform file has no other dependency. Each viewer can open it without
the sidecar.

### Names that are taken

Each signal and scope in a scope has a name of its own.

- A name that occurs once in a scope stays as it is.
- The duplicates of a name are numbered `name_0`, `name_1`, …, if none of
  these names is in the scope already.
- Otherwise they get two underscores: `name__0`, `name__1`, …, or more
  underscores until no name is taken. For example, a pair `x` gives `x_0`
  and `x_1`, so the two `x` of two calls of a helper beside it are `x__0`
  and `x__1`.
- Two signals with the same name and instance are numbered in the same way,
  in the order in which they were first recorded. Only probe contexts made
  by hand can make them.

The numbers depend only on the names in the scope, so the same design gives
the same names in each run. The VCD, the FST and the sidecar use the same
names.

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
  signals have the position of the call: they come where the helper is
  called, the signals of one call together, in the order of the helper's
  own bindings. Two calls of the helper give signals of the same name, which
  are numbered in the order of the calls: `count_0`, `other_0`, `count_1`,
  `other_1`.
- The fields of a record have the position of their binding, and then come
  in the order of their declaration, also inside a nested record.
- The elements of a `Vec` have the position of their binding, and come in
  the order of their index: `outs_2` before `outs_10`. A `Vec` of records
  comes field by field, in the order of their declaration, and each field
  element by element: `regs_0_valid`, `regs_1_valid`, `regs_0_data`,
  `regs_1_data`.
- The probes of a mealy or Moore machine have the position of its
  `mealyProbed` or `mooreProbed` call, and come in the order of their names.
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
- A window that shows nothing writes no file: one with `stop <= start`, or
  one that starts at or after the end of the simulation. The functions of
  `Clash.CircuitContext.Waveform` throw for `stop < start`. `writeFST`,
  `writeVCD` and `writeSpill` take the window as two `Natural`s, and give
  `Left` with the reason.

## The sidecar

The sidecar is a JSON file in the format of clash-shockwaves. The Surfer
plugin of clash-shockwaves reads the sidecar unchanged. It finds the sidecar
by the path of the waveform file, with the extension `.json`.

| Key | Content |
|---|---|
| `signals` | one entry for each described signal. The key is the full path of the signal in the waveform, for example `top.counter_0.count`, or `logic.out` for a name outside all components. The value is the key of the type of the signal in `types`. A signal that is recorded as bits only has no entry |
| `types` | one entry for each type of a described signal. The key is `<fingerprint>:<type>`. The value tells a viewer how to show the bits of the type: its constructors, fields and widths, from its `Waveform` instance |
| `luts` | the lookup tables of the types whose `Waveform` instance uses one: for each recorded value, its bits and the text to show. Empty when no type uses a lookup table |
| `clash-circuit-context` | only in the sidecar of the waveform of a failed run: see below |

For example, a waveform with one described signal, `accumulate.total` of
the type `Unsigned 8`, has this sidecar (the value in `types` is shortened):

```text
{
  "luts": {},
  "signals": {"accumulate.total": "b78a4dfc493b42551f787193a52a6a98:Unsigned 8"},
  "types": {"b78a4dfc493b42551f787193a52a6a98:Unsigned 8": {"v": …, "w": 8}}
}
```

For the format of the values in `types` and `luts`, see the [file format of
clash-shockwaves](https://github.com/clash-lang/clash-shockwaves/blob/main/docs/reference/FILE_FORMAT.md).

The sidecar of the waveform of a failed run also has the key
`clash-circuit-context`:

```text
"clash-circuit-context": {"origin": "failure", "waveform": "fst"}
```

`waveform` is the extension of that waveform, `fst` or `vcd`. A viewer
ignores this key. A passing `withWaveform` reads it to find the waveform
that it can delete. The sidecar of a waveform that was written on purpose
does not have this key.

## The spill file

A spill file describes itself, so that the file of a run that was stopped
can be read without the run. It is binary: a header (the 8 bytes
`CCCSPILL`, a version byte, and the time from which the run recorded), and
then regions. Each signal writes its value changes into regions of its own.
A region of a signal starts at 4 KiB, and each next region of that signal
is twice as large, up to 16 KiB. So a signal that changes rarely takes 4
KiB of disk. Other regions hold the names, widths and descriptions of the
signals, the lookup tables of the sidecar, and the end of the run.

How the regions are written depends on the platform:

- On Linux, each region is mapped into memory. A value is in the file as
  soon as it is recorded. A process that is killed (`kill -9`, the
  out-of-memory killer, a crash) leaves everything that it recorded. Only a
  crash of the computer can lose the newest values.
- The disk space of a region is reserved before the region is used. When
  the disk is full, the recording stops, a message on stderr says so, and
  the run goes on. The waveform ends where the recording stopped.
- On other platforms, and where a region cannot be mapped (a message on
  stderr then says so), each region is a buffer in memory, written to the
  file when it is full. A cancelled run loses nothing. A killed run loses
  the last region of each signal: up to 16 KiB of its newest values, and
  all values of a signal that never filled its first region.

The values are written twice: to the spill file while the simulation runs,
and to the waveform at the end.

Read a spill file with `recoverWaveform`, `recoverRecording`, `writeSpill`,
or the executables `spill2fst` and `spill2vcd`. See [How to keep the
waveform of a stopped run](../how-to/recover.md).

## Write the files yourself

`Clash.CircuitContext.Recording` records a simulation and writes its files.
Its functions take the full path of each file, with its extension:

| Function | Does |
|---|---|
| `withRecording spill action` | runs `action` with recording, and returns what the action returned or threw, with the `Recording`. A cancellation (an asynchronous exception, such as Ctrl-C or a test's timeout) is not returned: it goes on, after the recording stops. `spill` is `Just path` for a spill file at `path` (its directory is made), or `Nothing` to keep the values in memory. When the action is cancelled after it recorded a cycle, the spill file stays, and a message on stderr gives its absolute path. When it is cancelled before, the spill file is deleted |
| `writeFST (start, stop) rec path` | writes the FST file at `path` and the sidecar beside it. Returns `Left` with the reason when it writes nothing: nothing was recorded, the window shows nothing, or a file cannot be written |
| `writeVCD (start, stop) rec path` | the same, as a VCD file |
| `recordingStart rec`, `recordingEnd rec` | the time from which the recording has cycles, and the time at which the simulation stopped, in units of the timescale |
| `discardRecording rec` | deletes the spill file, if the recording has one |
| `recoverRecording path` | reads a spill file as a `Recording`, also of a run that was stopped |
| `writeSpill format window spill path` | writes the waveform of a spill file in one step. With the window `Nothing`, all cycles that the run recorded. Returns the window that the waveform shows |
| `recordedSignals`, `recordedChanges` | what was recorded, for tests |
