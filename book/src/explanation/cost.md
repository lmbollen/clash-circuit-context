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
machinery. The HDL is the same as the HDL of the design without
instrumentation, so you can instrument a design that you synthesise.

## In a simulation that does not record: almost none

`withoutCircuitContext` supplies a context that records nothing. Each
recording function checks this first. Then it does not touch a reference,
it does not add a level to the hierarchy, and `mealyProbed` does not count
cycles. A design that carries instrumentation costs almost nothing until a
simulation records it.

## In a simulation that records: time and memory

A recording holds the history of each signal until the dump, and the dump
renders a file. In a test suite that runs tests in parallel, the peak memory
is the sum over the tests that run at the same time.

The helpers of `Clash.CircuitContext.Waveform` therefore decide before a run
if it records. A run that will not be kept runs without recording. A
waveform of a passing test is rarely read, so the default to use is
`withWaveformOnFailure`: no recording while the test passes, and a second run
with recording when it fails. On the test suite of one real project (24
cores), this took the peak memory from 25.2 GB to 8.2 GB, and the time from
6 min 22 s to 1 min 6 s.

The second run is the price. It assumes that the failure repeats. For a
design where it does not, `withWaveformOnFailure'` records the first run and
writes nothing unless the test fails: no second run, but the cost of
recording in each run.

## Why a recording does not grow without limit

A recorder packs the value of each cycle into bits as the simulation makes
it, and keeps only the bits, in runs of equal values (as a VCD does). It
does not keep the values of the design. So the memory of a recording grows
with the number of changes, and a long simulation does not keep its history
alive.

The recorder packs the value of cycle *i* when the simulation evaluates
cycle *i + 1*, not while it makes cycle *i*. To pack a value can need the
value itself, through a combinational loop in the design, and then the
simulation would never finish. The value of a cycle can never need a later
cycle, so one cycle of delay is safe. A cycle that the simulation did not
evaluate is `z` in the VCD.
