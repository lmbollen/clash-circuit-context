# About the cost of recording

An instrumented design has three costs to consider: in hardware, in a
simulation that does not record, and in a simulation that records. This
page explains each one.

## In hardware: none

Each recording function has two branches, chosen by `clashSimulation` from
`Clash.Magic`. In a simulation, it records. When Clash generates HDL,
`clashSimulation` is `False`, and the function is the identity:
`component`, `traceSignalC` and `probe` disappear, and `mealyProbed` is an
ordinary `mealy`. The dead-code elimination of Clash then removes all the
machinery. The HDL has no registers and no logic for recording, so you can
instrument a design that you synthesise. The text of the HDL is not the same
as the text without instrumentation: see [How to generate HDL from an
instrumented design](../how-to/hdl.md#compare-with-the-hdl-of-the-design-without-instrumentation).

## In a simulation that does not record: almost none

`withoutCircuitContext` supplies a context that records nothing. Each
recording function checks this first. Then it does not touch a reference,
it does not add a level to the hierarchy, and `mealyProbed` does not count
cycles. A design that carries instrumentation costs almost nothing until a
simulation records it.

## In a simulation that records: time and memory

A recording packs the value of each signal in each cycle, and keeps the
changes, in memory or in a spill file. At the end, the waveform is written
from them. In a
test suite that runs tests in parallel, the peak memory is the sum over the
tests that run at the same time, so the time and memory of recording add up
quickly.

The functions of `Clash.CircuitContext.Waveform` therefore decide before a
run if it records. A run whose waveform will not be kept runs without
recording. The waveform of a passing test is rarely read, so `withWaveform`
does not record while the test passes. When the test fails, it runs the
simulation a second time, with recording. On the test suite of one real
project (24 cores), recording only on failure took the peak memory from
25.2 GB to 8.2 GB, and the time from 6 min 22 s to 1 min 6 s.

The second run is the price. It assumes that the failure repeats. For a
design where it does not, `writeWaveform` records the only run, at the cost
of recording in each run. A Hedgehog property has the same choice:
`simulateWaveform` records only the case that Hedgehog reports, after
shrinking.

There is no switch that turns recording on for all tests. A switch of that
kind recorded each test of a suite at the same time, and the memory of the
whole suite became the problem. To record nothing, simulate the design
under `withoutCircuitContext`.

## Where a recording keeps its history

A recorder packs the value of each cycle into bits as the simulation makes
it, and keeps only the changes, as a VCD does. The changes of each signal
are compact records in blocks of 16 KiB. The option `spill` decides where
the blocks go:

- `InMemory` (the default) keeps them in memory. The memory of a recording
  grows with its history, but the records are small: a few times smaller
  than a VCD file of the same run. Nothing is written to the disk but the
  waveform.
- `OnDisk` writes them to the spill file. The memory of a recording then
  does not grow with its length, and a run that is stopped leaves the
  file. The values are written to the disk twice: to the spill file, and
  to the waveform.

The waveform is written by a merge of the changes of all signals by time,
straight to the file, so the text of a VCD is never in memory.

On the firmware test of one CPU design (237,000 cycles, 162 signals), the
maximum residency was 167 MB in memory and 58 MB with the spill file. The
waveform was 14 MB as FST and 377 MB as VCD, and took the same time to
write.

Signals with the same history share one set of values in the waveform: one
identifier in a VCD file, aliases in an FST file. The same wire often has a
name at each level of the hierarchy that it passes, and each name is a
recorded signal. The recorder finds them by a hash of their changes, and
then compares the changes, so two signals that only have the same hash do
not share their values.

The recorder packs the value of cycle *i* when the simulation evaluates
cycle *i + 1*, not while it makes cycle *i*. To pack a value can need the
value itself, through a combinational loop in the design, and then the
simulation would never finish. The value of a cycle can never need a later
cycle, so one cycle of delay is safe. When the simulation stops, the
recorder evaluates each trace up to the last cycle that any signal reached,
for example the input of a register, which the simulation evaluated one
cycle less far. It does not do this when the run is cancelled. A cycle that
the simulation did not evaluate is `z` in the waveform.
