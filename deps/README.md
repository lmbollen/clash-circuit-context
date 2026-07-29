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
| `circuit-notation/` | `8da93a3` on branch `trace-ports` | A committed patch (not working-tree state): `trace-ports` mode, [below](#trace-ports-component-boundary-buses) |

`bittide-hardware` differs from `dogfood/bittide` in exactly four files, and
none of them is design code:

* `cabal.project` — the `source-repository-package` pins for `clash-protocols`,
  `clash-protocols-memmap` and `clash-cores` are commented out (kept for
  provenance, since each checkout sits at exactly that commit) and replaced by
  local paths under `packages`, along with `../circuit-notation`;
* `cabal.project.freeze` — the `circuit-notation` pin moves from `0.2.0.0` to
  `0.3.0.0`;
* `bittide/bittide.cabal` and `bittide-instances/bittide-instances.cabal` — each
  gains `-fplugin-opt=Protocols.Plugin:trace-ports`.

```bash
git diff dogfood/bittide dogfood/deps -- deps/bittide-hardware
```

shows the whole substitution.

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

### Turning it off again

Instrumenting a dependency does not commit its users to recording anything.
Tracing is opt-in **by signature, not by module**, and there are three ways back
out — all three are used in this tree:

| Granularity | How | Where you can see it here |
| --- | --- | --- |
| A whole function | Omit `HasCircuitContext` from its signature | Every un-instrumented peripheral; `clash-protocols` gained the plugin but almost no constraints |
| One binding | Prefix its name with `_` | Ports and scratch bindings a designer doesn't want in the waveform |
| A call and everything under it | `withoutCircuitContext` | The `Pnr/` and `Hitl/` synthesis entry points, which must not carry the constraint into generated HDL |

```haskell
-- bittide's synthesis instances, in effect:
topEntity = withoutCircuitContext (myInstrumentedCircuit clk rst)
```

It is HDL-transparent: the wrapped call generates identical hardware to calling
an uninstrumented function. So a dependency can ship instrumented combinators
without imposing anything on users who never supply a context — which is what
makes instrumenting `clash-protocols-memmap` viable at all.

What is *not* cheap is the reverse direction. `HasCircuitContext` is viral, so
opting a widely-shared component *in* costs 9 edits for one peripheral
(`timeWb`: 7 synthesis call sites plus 2 library intermediaries), and peripherals
inside `processingElement` are worse. Opting out is easy; opting in spreads. See
finding F1 in [`docs/dogfooding-bittide.md`](../docs/dogfooding-bittide.md).

### `trace-ports`: component boundary buses

The vendored `circuit-notation` carries a `trace-ports` patch (`8da93a3`) that
makes a `circuit` block's **interface** ports visible to renamer plugins, not
just its intermediate `<-` ports. Without it, you see a component's internal
wiring but not the buses crossing its boundary.

It is enabled here, which took three changes beyond vendoring the checkout:

1. `clash-protocols` (both packages) required `circuit-notation >=0.2 && <0.3`;
   raised to `<0.4`.
2. bittide's `cabal.project.freeze` pinned `circuit-notation ==0.2.0.0`; bumped
   to `0.3.0.0`.
3. `circuit-notation` 0.3 added three `ExternalNames` fields for its *value
   circuits* feature (`signalTagName`, `fwdTagName`, `dSignalTagName`).
   `Protocols.Plugin` is circuit-notation's plugin with renamed constructors
   (`CN.mkPlugin`), so it has to supply them. `Protocols` has no value-port
   markers, so they are bound to a loud `error` rather than silently aliased to
   `Tagged` — a construct we do not support should fail, not mis-desugar.

Then `-fplugin-opt=Protocols.Plugin:trace-ports` in the three packages that also
run the tracing plugin (`bittide`, `bittide-instances`,
`clash-protocols-memmap`). Note the option is namespaced by the plugin **module
GHC was given** — `Protocols.Plugin`, not `CircuitNotation`.

#### Measured effect

Ten of the fourteen firmware waveforms have a like-for-like baseline (both runs
produced by `cabal test`; the other four baselines came from `withWaveformC`
REPL runs, which add a test-side wire of their own and so are not comparable).
Across those ten:

**1817 → 1961 wires (+144, +7.9 %): 146 added, 2 removed.**

| Waveform | Before | After | Δ |
| --- | --- | --- | --- |
| `addressable_bytes_wb_test` | 143 | 157 | +14 |
| `capture_ugn_self_test` | 151 | 165 | +14 |
| `clock_control_wb_self_test` | 181 | 195 | +14 |
| `dna_port_self_test` | 138 | 152 | +14 |
| `registerwb_c_sim` | 266 | 280 | +14 |
| `time_self_test`, `time_c_test` | 157 | 171 | +14 |
| `watchdog_self_test` | 175 | 189 | +14 |
| `elastic_buffer_wb_test` | 255 | 272 | +17 |
| `axi_stream_self_test` | 194 | 209 | +17 −2 |

The uniform +14 is the CPU's shared periphery, identical in every DUT because
every DUT wraps the same `processingElement`. For `registerwb_sim`:

| Added | What it is |
| --- | --- |
| `manyTypesWb.{wb0_Fwd, wb_Fwd_0, wb_Fwd_1}` | Wishbone buses at the register device's ports (72 bits each) |
| `wbStorage_{0,1}.{wb0_Fwd, wb_Fwd, wbMm_Fwd._1}` | both storage instances' bus ports |
| `uartInterfaceWb.{bus_Fwd._1, wb0_Fwd, wb_Fwd}` | the UART peripheral's bus ports |
| `processingElement.jtagIn_Fwd` | JTAG into the CPU |
| `uartTx_Bwd` | the DUT's own top-level backpressure |

So trace-ports is a modest but qualitatively different gain: not more internal
signals, but the **interface** each instrumented component presents — you can
now see the bus at a peripheral's port, not only its internal wiring. It costs
nothing where there is no notation to desugar: the `xilinxElasticBuffer`
waveforms are 13 wires before and after, being plain Clash.

Compiling all of `bittide` + `bittide-instances` against 0.3 with the flag on
produced **no** desugaring regressions (307 modules clean), which was the main
risk: 0.3 also contains a value-ports refactor touching arity and as-pattern
handling.

#### The two lost wires — not caused by trace-ports

`axi_stream_self_test` lost `dut.brReadAddr` (10 b) and `dut.otpA` (1 b) relative
to its baseline. **Neither `trace-ports` nor `circuit-notation` 0.3 is
responsible**, and the reason is decidable statically: both are `where`-bindings
inside `Protocols.Df.fifo`, and `clash-protocols` today contains **zero**
occurrences of `HasCircuitContext`. The plugin instruments only functions whose
signature carries it, so there is nothing in that package for any version of the
desugarer to affect — those wires cannot be produced by the current source at
all.

They come from an earlier experiment. `Df.fifo` was temporarily given
`HasCircuitContext` (the "cycle 2" low-level instrumentation in
[`docs/dep-instrumentation-assessment.md`](../docs/dep-instrumentation-assessment.md)),
the axi baseline was captured at 09:49 while it was in place, and the experiment
was reverted afterwards. Because `fifo` has no `OPAQUE` pragma, its bindings
registered under the **caller's** scope rather than a `fifo` sub-scope — which is
exactly the flat `dut.brReadAddr` shape the baseline shows.

Worth noting against the assessment doc, which reports that instrumenting
`Df.fifo` yielded "**0 wires**": it actually yielded these two. Of the four
binders in `(brReadAddr, brWrite, otpA, otpB) = unbundle $ mealy …`, two are of
traceable type and two are not. The doc's conclusion still holds in substance —
none of the interesting internals appeared, and 2 wires for a repo-wide viral
constraint is a bad trade — but the number was 2, not 0.

So the trace-ports change is **purely additive**: 146 wires added, 0 lost.

### Does every binder get a named `Fwd` and `Bwd`?

Nearly, and the exceptions are inherent rather than missing work. The binder
forms actually used in bittide's `circuit` blocks, by frequency:

| Form | Count | `Fwd` | `Bwd` |
| --- | --- | --- | --- |
| `x <- …` | 218 | `x_Fwd` | `x_Bwd` |
| `(a, b) <- …` | 136 | per leaf | per leaf |
| `[a, b] <- …` (vec) | 43 | per leaf, indexed | per leaf |
| `_x <- …` | 31 | — (deliberate opt-out) | — |
| `Fwd x <- …` | 27 | plain `x` | **none — none exists** |
| lambda interface ports | — | `p_Fwd` (needs `trace-ports`) | `p_Bwd` |
| a port used twice (`RefMulticast`) | — | `p_Fwd` | **none — fan-in** |

Two forms cannot have both halves, for reasons in the notation's semantics:

* **`Fwd x <- …`** means *"give me only the forward signal"*; circuit-notation
  supplies `trivialBwd` for the backward channel, so there is no backward signal
  to name. Its forward half *is* traced, just as plain `x` rather than `x_Fwd` —
  and it has to be, because the binder escapes into ordinary Haskell. In
  `Bittide.Wishbone`, `Fwd regIn <- unsafeFromDf -< …` is followed by
  `rxEmpty = fmap isNothing regIn` in a plain `let`; renaming the binder to
  `regIn_Fwd` would break that code. The inconsistent name is the price of the
  escape hatch, not an oversight.
* **A port used more than once** has a backward channel that is a fan-in of its
  consumers, not one signal, so `bindWithSuffixNamed` binds a wildcard for it.
  Naming it would mean naming each leg separately.

What *was* genuinely missing had nothing to do with naming: a composite whose
`Fwd`/`Bwd` was a tuple containing one untraceable component fell back **whole**,
silently taking the traceable components with it. A memmap device port is
`(mm, wb)`, so its `Bwd` is `(MemoryMap, Signal dom WishboneS2M)` — the memory
map is not a signal, and the bus response disappeared along with it. Fixing that
in the runtime (fields are now tolerated individually) is what closed the gap:

| `registerwb_sim` | trace-ports only | + per-leaf tolerance |
| --- | --- | --- |
| wires | 281 | **586** |
| port bases | 97 | **244** |
| ports with *both* halves | 75 (77 %) | **233 (95.5 %)** |
| `Fwd`-only | 15 | **4** |

The four remaining `Fwd`-only ports (`jtag`, `jtagIn`, `rxFifoIn`, `uartRx`) have
backward channels that are genuinely not traceable signals. Recovered wires carry
real values — `uartInterfaceWb.bus_Bwd._1` (36 b `WishboneS2M`) shows
`bxxx…xxxx0000` while idle (data undefined, control bits defined), `b000…001000`
on ack, and `bxxx…x1000` for a write ack where read data is legitimately
undefined. Assertions still pass byte-exact.

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
