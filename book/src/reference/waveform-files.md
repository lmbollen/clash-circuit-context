# Waveform files

A waveform is a pair of files: a VCD file and a JSON sidecar. This page
describes where they go, their names, and their content.

## Place and names

| Item | Value |
|---|---|
| Directory | `waveforms`, relative to the working directory of the process (`waveformDir`) |
| VCD file | `waveforms/<name>.vcd`, where `<name>` is the name of the slot (`waveformPath name`) |
| Sidecar | `waveforms/<name>.json`, beside the VCD |
| Environment variable | `CCC_WAVEFORMS`: `waveformsRequested` is `True` when it is set to a value other than empty, `0` or `no` |

The working directory of `cabal test` and of `cabal run` can differ. When a
helper writes a waveform for a failed test, the message gives the absolute
path.

## How the files are written

- Each file is written to a temporary file first, and then renamed.
- The sidecar is renamed first. If a `.vcd` file exists, its `.json` file is
  complete. A crash between the two renames can leave a new `.json` file
  beside the `.vcd` file of an earlier run.
- A sidecar entry that fails to render (a `Waveform` instance whose width is
  not `BitSize`) is left out, with a message on stderr. The VCD still has the
  signal.

## The VCD

| Feature | Rule |
|---|---|
| Scopes | one `$scope module` for each component instance, nested as the components are |
| Signals | one `$var wire` for each recorded signal, with the name of its binding |
| Sibling instances | `name_0`, `name_1`, … in the order of their call sites in the source |
| Elements of a `Vec` | `name_0`, `name_1`, … |
| Fields of a record | `name_field` |
| A name outside each component | in the scope `logic` |
| Time | the timescale of `Clash.Signal.Trace.dumpVCD`: for one clock domain, one unit is one clock period |
| A cycle that was not evaluated | the value `z` |
| A value that is undefined | the value `x`, with the bits that are defined kept |
| Signals with the same history | one VCD identifier for all of them |

The VCD has no other dependency. Each viewer can open it without the
sidecar.

## The sidecar

The sidecar is in the format of clash-shockwaves. Its key `signals` has one
entry for each described signal. The key of each entry is the full path of
the signal in the VCD, for example `top.counter_0.count`, or `logic.out` for
a name outside each component. The Surfer plugin of clash-shockwaves reads
the sidecar unchanged.

## Write the files yourself

`Clash.CircuitContext.Core.dumpVCDC` gives the VCD as `Text`.
`Clash.CircuitContext.Shockwaves.dumpVCDSW` gives the VCD and the sidecar.
Both take the maps that `withCircuitContext` returns.
