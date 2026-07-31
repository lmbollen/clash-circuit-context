# `dogfood/bittide` — tracing a real design, with zero dependency changes

This branch vendors one instrumented checkout, [`bittide-hardware/`](bittide-hardware/),
and answers a single question:

> How much of a real Clash design can `clash-circuit-context` trace when you are
> only allowed to touch **that design** — not any of its dependencies?

Every dependency here is its **pristine upstream pin**. The
[`dogfood/deps`](#see-also) branch answers the follow-up question — what changes
when the dependencies are instrumented too — and the diff between the two
branches is the honest measure of what dependency instrumentation buys you.

## What was changed in `bittide-hardware`

The vendored tree is upstream `bittide-hardware` at commit
[`e80210f8`](https://github.com/bittide/bittide-hardware/commit/e80210f8495af32c3e97f8cc6fbe71530012c6b3)
(`origin/main`) plus the instrumentation, ~97 files. It splits into four kinds of
edit, and only the first is interesting:

1. **`HasCircuitContext` + `OPAQUE` on the peripherals to trace.** The plugin
   turns any `OPAQUE` top-level function carrying `HasCircuitContext` into a
   scope, and traces its named local bindings inside it. This is the actual
   opt-in.
2. **`withoutCircuitContext` at synthesis boundaries.** The `Pnr/` and `Hitl/`
   instances are synthesis entry points; they discharge the constraint so it
   does not escape into generated HDL.
3. **Parenthesising `$` in `circuit` blocks.** `f x $ g -< bus` becomes
   `(f x g) -< bus`. Mechanical, and needed because the tracing renamer runs on
   circuit-notation's desugared output.
4. **Test-side waveform plumbing**: `Tests.Waveform` (in both `bittide` and
   `bittide-instances`), plus `clash-circuit-context` in the two `.cabal` files
   and `-fplugin=Clash.CircuitContext.Plugin` in their `common clash` stanzas.

   That helper is the reason `Clash.CircuitContext.Waveform` now exists: two
   copies of the same lifecycle in one repository is what finding F7 is about,
   and the plugin ships it today. This branch deliberately keeps its own copy —
   the measurements below were taken with it, and rewriting the plumbing would
   break the correspondence between this branch and its own numbers.
   [`dogfood/shockwaves`](#see-also) is where the migrated form lives: slots
   instead of a global flush, and capture-on-failure instead of recording every
   run. If you are starting a project, start from the shipped module.

Enabling the plugin repo-wide is safe: it is a no-op for any module without a
`HasCircuitContext`/`HasProbe` signature. Opt-in is **by signature, not by
module**.

### Turning it off again

All three opt-out mechanisms appear in this tree:

| Granularity | How | Where you can see it here |
| --- | --- | --- |
| One binding | Prefix the binder with `_` | Scratch bindings and `circuit` ports a designer doesn't want in the waveform |
| One function | Omit `HasCircuitContext` (and/or `OPAQUE`) | Every peripheral that was left un-instrumented |
| A call and everything under it | `withoutCircuitContext` | The `Pnr/` and `Hitl/` synthesis entry points — edit kind 2 above |

```haskell
-- what bittide's synthesis instances do, in effect:
topEntity = withoutCircuitContext (myInstrumentedCircuit clk rst)
```

It is HDL-transparent: the wrapped call generates identical hardware to calling
an uninstrumented function, so discharging the constraint at a synthesis boundary
costs nothing.

The asymmetry is the real cost. Opting *out* is a one-line edit; opting *in* is
viral, because `HasCircuitContext` propagates to callers — `timeWb` needed 9
edits (7 synthesis call sites plus 2 library intermediaries), and peripherals
instantiated inside `processingElement` are worse still. See finding F1 in
[`docs/dogfooding-bittide.md`](../docs/dogfooding-bittide.md).

Wiring `clash-circuit-context` in is the whole of the build-level change — see
the top of [`bittide-hardware/cabal.project`](bittide-hardware/cabal.project):

```cabal
packages:
  ...
  ../../clash-circuit-context.cabal
```

Everything below that in the same file is untouched upstream, including the
`source-repository-package` pins for `clash-protocols`, `clash-protocols-memmap`
and `clash-cores`. That is the defining property of this branch.

## Generating the waveforms

### One-time setup after cloning

`bittide-hardware` locates its own project root at **compile time** by shelling
out to `git rev-parse --show-toplevel` — in a TH splice that reads clock-config
CSVs (`Bittide.ClockControl.ParseRegisters`), in `bittide-instances`' test main
when it looks up firmware binaries, and in the Rust build scripts under
`firmware-support/`. Vendored as a plain subdirectory it has no `.git` of its
own, so that call would return *this* repository's root and the build would fail
looking for `<repo-root>/bittide/data/clock_configs/…`.

Give the vendored tree its own repository. An empty `git init` is enough —
nothing needs to be committed:

```bash
cd deps/bittide-hardware
git init -q
```

This nested repository is deliberately not part of this branch, and it does not
disturb the outer one: the vendored files are already tracked here, and git keeps
tracking them normally when a subdirectory acquires its own `.git`.

### Building and running

The toolchain is GHC 9.10.3 + `clang` + `cargo` (the firmware self-tests build
RISC-V ELFs). All of it comes from the flake, and **none of it is on the ambient
`PATH`** — enter the devshell first:

```bash
cd deps/bittide-hardware
nix develop          # or `direnv allow`, the tree ships an .envrc
```

The `bittide-instances` self-tests each run a RISC-V core against real
firmware, so the firmware ELFs have to exist before the Haskell tests can load
them. Build `bittide-instances` first (the firmware's `build.rs` generates its
HAL from the memory map), then the binaries:

```bash
cabal build bittide-instances
(cd firmware-binaries && cargo build --release)
```

`firmware-binaries/.cargo/config.toml` already pins the target and the output
directory, so no flags are needed; the ELFs land in
`_build/cargo/firmware-binaries/riscv32imc-unknown-none-elf/release/`, which is
where `peConfigFromElf` looks. Skipping this step is not a subtle failure — a
dozen tests abort with *"does not point to an extant file"*.

Then run the two suites that carry instrumentation:

```bash
cabal build bittide:unittests bittide-instances:unittests
cabal test  bittide:unittests
cabal test  bittide-instances:unittests
```

VCDs land in `waveforms/` **relative to each package directory** — so
`bittide/waveforms/` and `bittide-instances/waveforms/`. They are gitignored;
regenerate rather than commit them.

One file per test, holding that test's **last** run. A hedgehog property runs
many cases, and rendering a VCD per case would dominate the suite; instead each
run overwrites the pending waveform for its name and a single `flushWaveforms`
(wired via `finally` in both `unittests.hs` mains, so it fires even on the exit
exception `defaultMain` throws) writes each file once at the end. Last run is
also the *useful* run: on success hedgehog grows the size parameter, so the last
case is the largest; on failure it shrinks, so the last case is the minimal
counterexample.

### The 27 waveforms

**`bittide:unittests` — 13**, from `bittide/tests/`. Plain Clash designs sampled
with `sampleN`/`simulateN` (`withWaveform`) or small `clash-protocols` circuits
(`withWaveformC`):

| Waveform | Source |
| --- | --- |
| `prop_happy`, `case_trackerWaveform` | `Tests/Transceiver/Prbs.hs` |
| `prop_handshake`, `prop_noHandshake` | `Tests/Handshake.hs` |
| `case_xilinxElasticBufferEq`, `…MaxBound`, `…MinBound` | `Tests/ElasticBuffer.hs` |
| `byteAddressableBlockRamAsBlockRam`, `readWriteByteAddressableBlockram` | `Tests/DoubleBufferedRam.hs` |
| `case_asciiDebugMuxWaveform` | `Tests/Df.hs` |
| `case_axi4StreamPacketFifoWaveform` | `Tests/Axi4.hs` |
| `bench_correctness`, `bench_recording` | `Tests/Bench.hs` — tracing-overhead benchmark |

`Tests/Counter.hs` holds three more call sites (`case_zeroSameDomain`,
`case_zeroSrcRst`, `case_zeroDstRst`), but they do **not** run: `Tests.Counter` is
listed in `bittide.cabal`'s `other-modules`, so it compiles, yet `UnitTests.hs`
never imports it into the tasty tree. That is pre-existing upstream, not something
the instrumentation introduced. Reach them via the REPL recipe below.

**`bittide-instances:unittests` — 14**, from `bittide-instances/tests/`. Each is
a RISC-V firmware self-test `sampleC`-ing a `Circuit () (Df dom (BitVector 8))`
UART DUT built around a VexRiscv CPU. All use `withWaveformLive`, so each
captures the cycles its assertion actually forced rather than a fixed window:

| Waveform | Source |
| --- | --- |
| `registerwb_sim`, `registerwb_c_sim` | `Wishbone/RegisterWb.hs` |
| `nested_interconnect_sim` | `Wishbone/NestedInterconnect.hs` |
| `watchdog_self_test` | `Wishbone/Watchdog.hs` |
| `time_self_test`, `time_c_test` | `Wishbone/Time.hs` |
| `capture_ugn_self_test` | `Wishbone/CaptureUgn.hs` |
| `clock_control_wb_self_test` | `Tests/ClockControlWb.hs` |
| `addressable_bytes_wb_test` | `Wishbone/AddressableBytesWb.hs` |
| `axi_stream_self_test` | `Wishbone/Axi.hs` |
| `dna_port_self_test` | `Wishbone/DnaPortE2.hs` |
| `elastic_buffer_wb_test` | `Df/ElasticBufferWb.hs` |
| `wb_to_df_test` | `Df/WbToDf.hs` |
| `ring_buffer_test` | `Wishbone/RingBuffer.hs` |

### Generating one waveform without running the whole suite

The firmware suites are slow (a late-printing DUT can be ~70 s and several GB on
its own). To produce a single VCD, call its DUT from a REPL. Every waveform in
the table above was generated this way during development — e.g. for
`registerwb_sim`, put this in `regwb.ghci`:

```haskell
import Bittide.Instances.Tests.RegisterWb (dutWithVcdAndPeConfig, peConfigSim)
import Protocols.MemoryMap (unMemmap)
import Tests.Waveform (withWaveformC, flushWaveforms)
import VexRiscv (DumpVcd (NoDumpVcd))
pc <- peConfigSim
_ <- withWaveformC "registerwb_sim" 2 150000 "uartTx" id (unMemmap (dutWithVcdAndPeConfig NoDumpVcd pc))
flushWaveforms
putStrLn "DONE"
```

and run it against the test suite (which is where `Tests.Waveform` lives):

```bash
cabal repl bittide-instances:unittests < regwb.ghci
```

The `withWaveformC` arguments are: VCD base name, reset cycles, cycles to
sample, traced wire name, a projection of the circuit's forward output, and the
fully driven circuit. `flushWaveforms` must run for anything to reach disk — the
render is deferred by design (see above).

Designs sampled with `sampleN`/`simulateN` rather than `sampleC` use
`withWaveform` instead, and long-running firmware tests that should stop at their
own exit condition use `withWaveformLive`, which captures the cycles the consumer
actually forced instead of a fixed window.

### Reading the result

The VCD hierarchy mirrors the design: scope per instrumented component, wire per
traced binding, e.g. `dutWithMm → captureUgns → {6 signals}`. Any hierarchical
VCD viewer works; [Surfer](https://surfer-project.org/) and GTKWave were both
used during development.

Values are recorded with three-state fidelity, which matters when reading these
files: a cycle that was **never sampled** renders `z`, an **evaluated but
undefined** value renders `x`, and a partially-defined value keeps its defined
bits (`b0x…`) rather than being silently zeroed.

## What you get, and what you don't

With dependencies left pristine, tracing covers bittide's **own** `circuit`
blocks and instrumented peripherals. The gap this branch exists to demonstrate:

* `HasCircuitContext` is **necessary but not sufficient**. Only combinators that
  go through circuit-notation's `circuit`/`-<` desugaring get traced. A
  combinator built directly from the `Circuit` constructor (`Df.fifo` is the
  clean example) contributes **zero** wires no matter what constraints it
  carries — its internals are `where`-bound inside a function the renamer never
  sees.
* Register **contents** are invisible. `registerWb` and friends live in
  `clash-protocols-memmap`; their state is a `where`-binding in a low-level
  `Circuit go`, so no amount of bittide-side instrumentation reaches it. On
  `dogfood/deps` a one-expression tap inside `registerWbDf` adds 44 content
  wires to `registerwb_sim` alone.
* The constraint is **viral**, and the cost scales with how shared a component
  is. `timeWb` has 7 direct synthesis call sites plus 2 library intermediaries —
  9 `withoutCircuitContext` edits for one peripheral. Peripherals instantiated
  inside `processingElement` (`wbStorage`, `singleMasterInterconnectC`,
  `watchDogWb`) are worse: every DUT and every synthesis instance uses it.

The full findings list, with the F-numbers referenced in code comments, is in
[`docs/dogfooding-bittide.md`](../docs/dogfooding-bittide.md) on `main`.

## What this unlocks

The waveforms are the demonstration, not the point. What this branch is
evidence for:

**Waveforms stop being something you set up.** 27 of them fall out of a suite
whose tests were written to assert, not to trace. The instrumentation is two
annotations on a function — no `traceSignal` calls, no name strings, no
per-signal work — so the marginal cost of the 28th waveform is one
`HasCircuitContext` in a signature. A design where that is true is a design
where "can I see this signal?" stops being a question worth asking.

**It can stay in the design.** Every recording combinator is gated on
`Clash.Magic.clashSimulation`, so an instrumented function synthesizes to the
same netlist as the un-instrumented one — verified here on the real
`xilinxElasticBuffer` (dcFifo + CDC), which generates clean Verilog with zero
tracing leakage. Instrumentation you have to strip before taping out is
instrumentation you stop using; this one has no such deadline.

**A failing CI job can hand you the waveform, and a green one pays nothing.**
This branch is the *before* picture: it records every run unconditionally, and
`bittide-instances:unittests` takes **10m18s and peaks at 7.7 GB** for 32
tests, almost all of it spent rendering waveforms nobody asked for. The same
32 tests on `dogfood/shockwaves`, which records only what will be kept, take
**69 s** — and still write the failing test's waveform when one goes red. For
a hedgehog property what lands is the *shrunk counterexample*: the minimal
case, in the cycles that produced it.

That difference is why `Clash.CircuitContext.Waveform` exists on `main`, and
it is the one result from this whole exercise most worth taking to another
project.

**A firmware bug becomes a hardware waveform.** The `bittide-instances`
waveforms each cover a RISC-V core executing real firmware against real
peripherals. When a self-test prints the wrong byte, the VCD shows the bus
transaction that produced it — the CPU side and the peripheral side on one
time axis, which is exactly the boundary these bugs live on.

**And the honest limit, which is why `dogfood/deps` exists.**
`HasCircuitContext` is necessary but not sufficient: only combinators
desugared through circuit-notation are reachable, so a `Circuit`-constructor
combinator contributes nothing however it is annotated, and a dependency's
internal state (a register's *contents*) is invisible from the design side.
Instrument the dependency and it appears. That is the whole argument of the
next branch.

## See also

* **`main`** — the plugin and runtime themselves, plus `docs/`.
* **`dogfood/deps`** — this tree plus instrumented `clash-protocols`,
  `clash-protocols-memmap`, `clash-cores` and `circuit-notation`. Compare
  `bittide-hardware/cabal.project` between the branches to see the dependency
  substitution, and
  [`docs/dep-instrumentation-assessment.md`](../docs/dep-instrumentation-assessment.md)
  for the measured per-dependency wire counts.

## Note on the vendored checkout

`bittide-hardware/` is committed as plain files, not a submodule. The
instrumentation is working-tree state on top of upstream `main`, never pushed
anywhere, so there is no commit a submodule could point at — and the point of
this branch is that you can read the instrumentation in the diff rather than
having to reconstruct it.

The upstream base is `e80210f8`, so to see the instrumentation on its own, clone
upstream at that commit and diff against the vendored tree:

```bash
git clone https://github.com/bittide/bittide-hardware /tmp/bh-upstream
git -C /tmp/bh-upstream checkout -q e80210f8
diff -ru --exclude=.git /tmp/bh-upstream deps/bittide-hardware
```

Only tracked sources are vendored (~630 files, ~6.7 MB); build output is excluded
by bittide's own `.gitignore`, which this repository honours because the tree is
committed as ordinary files. Two local-only files are deliberately left out:
`cabal.project.local` (its contents were folded into the tracked `cabal.project`
so this tree builds with no extra setup) and `devenv.sh` (a dumped nix
environment full of absolute `/nix/store` paths — use `nix develop` instead).
