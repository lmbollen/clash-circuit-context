# `dogfood/deps` — tracing a real design, dependencies instrumented too

This branch is [`dogfood/bittide`](#see-also) plus the instrumented dependencies
that bittide builds against. It answers the follow-up question:

> How much *more* of a real Clash design becomes visible once you are also
> allowed to touch its dependencies?

Read the two branches as a pair. `dogfood/bittide` instruments bittide alone and
leaves every dependency on its pristine upstream pin; this branch swaps four of
them for local instrumented checkouts. The diff between the branches is the
honest measure of what dependency instrumentation buys.

## The five vendored checkouts

| Checkout | Upstream pin | Instrumentation |
| --- | --- | --- |
| `bittide-hardware/` | `e80210f8` (`origin/main`) | Unchanged from `dogfood/bittide` except `cabal.project` (see below) |
| `clash-protocols/` | `e03dbd89` | `.cabal` only: `ImplicitParams`, the plugin, a `clash-circuit-context` dep |
| `clash-protocols-memmap/` | `20bacc70` | `.cabal` + `HasCircuitContext` on `deviceWb`/register combinators + a contents tap |
| `clash-cores/` | `5d084eff` | `.cabal` + `HasCircuitContext` on `etherboneC` |
| `circuit-notation/` | `8da93a3` on branch `trace-ports` | A committed patch, not working-tree state — see the caveat below |

The only change to `bittide-hardware` relative to `dogfood/bittide` is its
`cabal.project`: the `source-repository-package` pins for `clash-protocols`,
`clash-protocols-memmap` and `clash-cores` are commented out (kept for
provenance, since each checkout sits at exactly that commit) and replaced by
local paths under `packages`. Diff that one file between the branches to see the
whole substitution.

## What the dependency instrumentation actually buys

**The headline: register contents.** `registerWb` and its whole family live in
`clash-protocols-memmap`, and their stored state is a `where`-binding inside a
low-level `Circuit go` — invisible to any amount of bittide-side work. A
one-expression tap inside `registerWbDf` (the shared worker behind
`registerWb`/`registerWb_`/`registerWbVec*`/`registerWithOffset*`/`*I`, so one
edit covers all of them) yields **45 `<device>_<register>_content` wires in
`registerwb_sim`** — `ManyTypes_x2_content`, `ManyTypes_sum0_content`, … — at
widths matching the register types and carrying real values. The watchdog test
goes from 168 to **175** wires, gaining the CPU's live register contents by name
(`Timer_scratchpad_content`, `Uart_data_content`, …) with its byte-exact
assertion intact.

Instrumenting the memmap register/device layer accounts for **+54 wires** in the
firmware DUTs overall (25 → 168 in the watchdog trace), which is the single
largest win from dependency instrumentation.

Getting that tap right is the interesting part, and it is a lesson about the
runtime rather than about memmap:

```haskell
wbS2M2Traced
  | clashSimulation =
      (\c s -> c `seq` s)
        <$> traceSignalC [I.i|#{deviceName}_#{registerName}_content|] packedOut
        <*> wbS2M2
  | otherwise = wbS2M2
```

`traceSignalC` records in lockstep with the tapped signal being **sampled**, so
the tap has to sit on a path that is always sampled. The obvious spot — the
register's value output `aOut` — is wrong: wrappers like `registerWb_`
(`_ignored <- registerWbDf …`) discard it. Those registers *would* still show up,
but only because `dumpVCDC` drains the packed tail at dump time — the O(cycles)
space leak the lockstep recorder exists to avoid. Threading it through the bus
response `wbS2M2`, which the interconnect consumes every cycle for every register
whether or not anyone reads it, records all variants the efficient way. The
`clashSimulation` guard keeps synthesis seeing a plain `wbS2M2`, so there is
**zero synthesis impact**.

**And the anti-headline: `HasCircuitContext` is necessary but not sufficient.**
Adding the constraint to a dependency buys nothing on its own. Only combinators
that go through circuit-notation's `circuit`/`-<` desugaring get traced; one
built directly from the `Circuit` constructor contributes **zero** wires however
it is annotated, because its internals are `where`-bound inside a function the
renamer never sees. Concretely: instrumenting `clash-protocols-memmap` added
54 wires, while `Df.fifo` in `clash-protocols` added **0**. That is why
`clash-protocols` and `clash-cores` here carry `.cabal`-level changes and almost
no source changes — there was nothing for the plugin to reach.

Per-dependency measurements are in
[`docs/dep-instrumentation-assessment.md`](../docs/dep-instrumentation-assessment.md);
the full findings list, with the F-numbers referenced in code comments, is in
[`docs/dogfooding-bittide.md`](../docs/dogfooding-bittide.md).

### Caveat: `circuit-notation` is not wired into bittide

The vendored `circuit-notation` carries a `trace-ports` patch (`8da93a3`) that
makes *all* circuit ports visible to renamer plugins, not just the intermediate
`<-` ports. On this branch it is exercised only by this repository's own
`notation-smoke` suite — **not** by the bittide waveforms. Two things block that:

* it is version `0.3.0.0`, while `clash-protocols` requires
  `circuit-notation >=0.2 && <0.3`, so bittide resolves to the Hackage version;
* `trace-ports` is opt-in per package via
  `-fplugin-opt=CircuitNotation:trace-ports`, which no bittide `.cabal` sets.

Relaxing that bound and enabling the option is the natural next step for this
branch, and it is not done here — so none of the wire counts above include any
trace-ports effect.

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

Then run the two suites that carry instrumentation:

```bash
cabal build bittide:unittests bittide-instances:unittests
cabal test  bittide:unittests
cabal test  bittide-instances:unittests
```

Expect a long first build: unlike `dogfood/bittide`, this branch builds five
dependency libraries from source rather than reusing cached upstream pins.

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

## This repository's own test suite on this branch

Because `deps/circuit-notation` is vendored here, this branch also carries the
`notation-smoke` suite, which runs the real circuit-notation desugarer against
the plugin instead of hand-mimicking its output. It is absent from `main`, whose
`cabal.project` is just `packages: .` so a plain clone builds. From the
repository root:

```bash
./check.sh              # all four suites + golden VCD diffs
```

## See also

* **`main`** — the plugin and runtime themselves, plus `docs/`. No vendored
  dependencies; builds from a plain clone.
* **`dogfood/bittide`** — bittide instrumented, every dependency pristine. The
  baseline this branch is measured against.

## Note on the vendored checkouts

All five are committed as plain files, not submodules. Four carry
never-pushed working-tree instrumentation, so there is no commit a submodule
could point at — and the diff is the artifact worth reading. To recover any one
checkout's instrumentation on its own, clone its upstream at the pin from the
table above and diff:

```bash
git clone https://github.com/QBayLogic/clash-protocols-memmap /tmp/memmap
git -C /tmp/memmap checkout -q 20bacc70
diff -ru --exclude=.git /tmp/memmap deps/clash-protocols-memmap
```

`circuit-notation` is the exception: its change is the committed `8da93a3` on a
local `trace-ports` branch, so `git show 8da93a3` in a checkout of that branch
gives the patch directly.

Only tracked sources are vendored; build output is excluded by each checkout's
own `.gitignore`, which this repository honours because the trees are committed
as ordinary files. Two local-only bittide files are deliberately left out:
`cabal.project.local` (its contents were folded into the tracked `cabal.project`
so the tree builds with no extra setup) and `devenv.sh` (a dumped nix environment
full of absolute `/nix/store` paths — use `nix develop` instead).
