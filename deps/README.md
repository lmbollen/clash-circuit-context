# `dogfood/shockwaves` — hierarchy *and* ADT information

This branch is `dogfood/deps` plus [`clash-shockwaves`](https://github.com/clash-lang/clash-shockwaves),
combined so one simulation yields both halves of a good waveform:

> Automatic tracing and hierarchy from this plugin, **and** the ADT structure
> (constructors, fields, bit ranges) that makes those bits readable.

See [Typed waveforms](#typed-waveforms-the-shockwaves-half) for that part; the
dependency-instrumentation story it builds on is unchanged and described below.

Read the two branches as a pair. `dogfood/bittide` instruments bittide alone and
leaves every dependency on its pristine upstream pin; this branch swaps four of
them for local instrumented checkouts. The diff between the branches is the
honest measure of what dependency instrumentation buys.

## The six vendored checkouts

| Checkout | Upstream pin | Instrumentation |
| --- | --- | --- |
| `bittide-hardware/` | `e80210f8` (`origin/main`) | Unchanged from `dogfood/bittide` except `cabal.project` (see below) |
| `clash-protocols/` | `e03dbd89` | `.cabal` only: `ImplicitParams`, the plugin, a `clash-circuit-context` dep |
| `clash-protocols-memmap/` | `20bacc70` | `.cabal` + `HasCircuitContext` on `deviceWb`/register combinators + a contents tap |
| `clash-cores/` | `5d084eff` | `.cabal` + `HasCircuitContext` on `etherboneC` |
| `circuit-notation/` | `8da93a3` on branch `trace-ports` | A committed patch (not working-tree state): `trace-ports` mode, [below](#trace-ports-component-boundary-buses) |
| `clash-shockwaves/` | `2bdb720` (`origin/main`) | Unmodified — consumed as a library, [below](#typed-waveforms-the-shockwaves-half) |

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

### Binders the notation invents are never traced

A `circuit` block's final statement, unless it is literally `idC -< x`, is
desugared as if the designer had written `final:stmt <- c -< x; idC -<
final:stmt` — a binder that exists in no form in the source. Before it was
excluded, this one artefact was 979 of 4,823 declarations (20 %) across the
bittide waveforms — measured by toggling only this gate on today's build —
every one duplicating a signal already present under its real name (its
composite ports even fan out into per-leaf sub-scopes, which is exactly the
`final:stmt_*` clutter that showed up under `manyTypesWb` in Surfer).

The exclusion is a naming **contract** between the two plugins, not a filter
over the output: no source-language variable identifier can contain a `:`, so
circuit-notation puts one in the name of every binder it invents (`final:stmt`,
the `lam:`-prefixed trace-ports lambda binders, the `val:in`/`val:out`/
`circuit:logic` value-circuit plumbing — see `Note [Synthesised binder names]`
in its source), and this plugin's renamer refuses to wrap any colon-carrying
binder (`wantedBinder`). The signal is never instrumented, registered or
sampled — and the mark is unforgeable, so no user-named port can ever be
skipped by it.

Two obvious alternatives fail, instructively: marking the *span* as generated
breaks the notation itself (`genLocName` derives collision-free names from
spans; blanking one collapsed every `final:stmt` in a module to a single
self-referential binding, which hung the compiler on
`Bittide.Instances.MemoryMaps`) and degrades error locations; a leading
underscore collides with `completeUnderscores`, which binds `_`-ports to `def`
— including the block's own master, so the circuit's output would silently
become a default value.

## Typed waveforms: the shockwaves half

A hierarchical VCD tells you a signal is `manyTypesWb.wb_Fwd` and 72 bits wide.
It cannot tell you those bits are a `WishboneM2S 27 4` whose `addr` occupies one
field and whose `cycleTypeIdentifier` another. `clash-shockwaves` knows exactly
that, from the payload type — and the two libraries turn out to be complements
rather than competitors:

| | this plugin | `clash-shockwaves` |
| --- | --- | --- |
| knows | *where* a signal is | *what* a signal is |
| output | nested `$scope` per component instance | ADT structure as a JSON sidecar |
| state | per-simulation `IORef`s | one process-global `unsafePerformIO` `IORef` |
| hierarchy | yes | no — a single `$scope module logic` |

So each side keeps what it is good at. Values, hierarchy and isolation stay with
this recorder; the ADT description rides along and is emitted in shockwaves'
own schema, which its
[Surfer plugin](https://github.com/clash-lang/clash-shockwaves) reads unmodified.

### How it is joined

The payload type is only known at registration, so that is where the descriptor
is captured: `traceSignalC` requires `Waveform a` and stores
`(typeName @a, tRef @a)` in the trace entry. `Clash.CircuitContext.Shockwaves`
then emits `{signals, types, luts}` keyed by the paths `disambiguate` produces —
which **are** the paths the VCD declares, sibling `_0`/`_1` suffixes included. No
name matching, and nothing to keep in sync.

`tRef` rather than `translator` matters: the reference form registers the type
*itself*, where a bare translator only pulls in the types it references. With the
wrong one, a record's own entry is missing from `types` while its members are
present.

### The cost, measured — and the trap in it

Requiring `Waveform` of every traced payload means tracing and typed waveforms
always arrive together. But the requirement is decided by the same oracle as
every other tracing constraint, so a payload *without* an instance does not fail
to compile — **it silently stops tracing**. That is the correct behaviour for an
instrumentation library (it must never break a build), and it is a trap for
exactly this reason: the failure mode is a wire that is simply absent, and no
diagnostic anywhere says so.

It caught us. Deriving `Waveform` on the types that obviously needed it left
**366 wires (10 %) silently missing** across the 29 waveforms — `Ack` handshakes
inside `xilinxElasticBuffer`, every `Status`/`Meta` wire in `handshake`, the CPU's
`cpuOut`/`rvIn`, `RamOp` ports, whole `activeSubordinate` families. Nothing
failed; the VCDs just quietly said less. It surfaced only from a **coverage diff
against a build where nothing could be dropped**, not from reading the waveforms
we had — a present-signal audit cannot see an absent signal.

The fix is to derive the instance everywhere rather than tolerate the gap:

| where | what |
| --- | --- |
| `clash-protocols` | `Ack`, and all 16 `Experimental.Axi4` request/response types |
| `bittide` | 10 modules (`Handshake`, `ClockControl` + `Config`/`Si539xSpi`/`Callisto`, `Wishbone`, `Axi4`, `SharedTypes`, `Ethernet.Mac`, …) |
| `bittide-instances` | `RegisterWb`'s 13 register payloads, `TimeWb`, `WbToDf`, `Common`, 3 HITL modules |
| `bittide-extra` | `Wishbone.Extra` |
| orphans, by necessity | `clash-vexriscv`'s `CpuIn`/`CpuOut`/`JtagIn`/`JtagOut` (in `ProcessingElement`) and clash-prelude's `RamOp` (in `Df.Extra`) — both are external pins, instanced next to the component whose ports carry them |
| polymorphic signatures | 3: `handshake`, `wbToDf`, and `bittide-extra`'s `withWaveformC` now carry `Waveform a` alongside `BitPack a` |

One upstream constraint was also relaxed: `clash-shockwaves` required
`1 <= n` for `Waveform (Index n)`, while `BitPack (Index n)` needs only
`KnownNat n`. The stricter context made the instance unusable in any component
polymorphic over `n` without that bound — common in interconnects — so it now
matches `BitPack`.

After the sweep the diff is exact: **4,823 declarations with everything
traceable, minus 979 notation artifacts, equals the 3,844 the branch emits.**
Every real wire is accounted for.

The residual lesson is a maintenance one. A new `BitPack` payload that forgets
`Waveform` will silently lose its wires the same way, and only a coverage diff
will notice. Deriving both together is the habit that prevents it.

### What you get

`registerwb_sim`, one firmware DUT, regenerated on this branch:

| | |
| --- | --- |
| VCD wires | 567 |
| signals described in the sidecar | **543** (96 %) |
| distinct types described | **27** |

The 27 include `WishboneM2S 27 4`, `WishboneS2M 4`, `BurstTypeExtension`,
`Maybe (BitVector 32)`, `Vec 44 (WishboneM2S 27 4)`. The `WishboneM2S`
translator carries every field name — `addr`, `writeData`, `busSelect`,
`busCycle`, `strobe`, `writeEnable`, `cycleTypeIdentifier`,
`burstTypeExtension` — so a viewer decodes the 72-bit blob into named fields
instead of showing hex.

The 24 undescribed wires are **probes**: `probe` is a different recording path
from `traceSignalC` and carries no descriptor yet. That is the obvious next step.

`luts` comes out empty, which is correct rather than broken: the generic
`Waveform` default does not use the lookup-table approach — only
`deriving (Waveform) via WaveformForLut` does.

### A waveform is now a pair of files

Every VCD needs its `.json` beside it, and that is `clash-shockwaves`' own
convention — its `dumpVCD` returns `(contents, meta)` and its docs write the two
separately. Matching it is precisely why its Surfer plugin reads our output
unmodified; embedding the descriptor in a VCD `$comment` block (which standard
parsers ignore) would work mechanically but would mean patching their plugin.

Three things keep the coupling cheap:

* **The VCD is still self-contained as a waveform.** Without the sidecar you get
  the full hierarchy and values, just untyped bits. `dumpVCDC` remains available
  for exactly that.
* **The sidecar is rounding error.** 82 KB against a 190 MB VCD for
  `registerwb_sim`, 45 KB against 29 MB for `watchdog_self_test` — about 0.04 %.
  It is per-dump, not per-signal.
* **One call produces both**, so they cannot drift apart in content, and
  `writeWaveformSlot` writes both through temp + rename with the **sidecar
  landing first**. That gives readers the invariant they need: if the `.vcd` exists, its
  `.json` exists and is complete. Writing the sidecar non-atomically (the first
  version of this) would let an interrupted run leave a whole VCD beside a
  truncated sidecar, silently disagreeing.

### The viewer side is a three-link version chain

Vendoring the Haskell library pins the JSON **wire format**, and two more things
have to line up before a viewer can read it. Both failure modes showed up in
practice, and neither says anything is wrong with the emitted sidecar:

```
SHOCKWAVES: Could not parse metadata file: unknown variant `X`,
            expected one of `I`, `In`, `C`, `Concat`, `L`, `Lit`, `S`, `Slice`
```

The Haskell `BitPart` has **13** constructors; `"X"` is `BPHasUndefined` ("1 if
there are undefined bits"). A plugin that accepts only those four predates
`BPHasUndefined`, `BPReverse`, `BPInvert`, `BPAnd`/`Or`/`Xor`, `BPOneHot`/`NHot`
and `BPIf` — i.e. it was built from an older `clash-shockwaves` than the one
vendored here. The vendored
[`surfer-shockwaves/src/data.rs`](clash-shockwaves/surfer-shockwaves/src/data.rs)
has all 13, so **build the plugin from this checkout**, not from a separate one.

```
Failed to load plugin … incompatible import type for
  `extism:host/user::translators_config_dir`
```

That is the next link: the plugin imports host functions from
`surfer-translation-types`, which it pins by tag, and that tag must match the
**installed Surfer**. The vendored plugin pinned `v0.6.0` while Surfer here is
`0.7.0`; the pin is now `v0.7.0`. Build and install with:

```bash
cd deps/clash-shockwaves/surfer-shockwaves
cargo build --target wasm32-unknown-unknown   # needs the wasm32 target
./compile.sh linux                            # copies into ~/.local/share/surfer/translators/
```

Bumping that tag drags a third constraint with it. `surfer-translation-types`
takes `ecolor` from surfer's *workspace* — 0.33 at Surfer 0.6.0, **0.34.1** at
0.7.0 — while the plugin pins `egui` itself. Leave them apart and two `ecolor`
crates coexist, so the `Color32` the plugin builds is not the `Color32`
`ValueKind::Custom` expects:

```
error[E0308]: mismatched types
  expected `ecolor::color32::Color32`, found `Color32`
  note: there are multiple different versions of crate `ecolor` in the dependency graph
```

The fix is alignment, not conversion: the plugin's `egui` must track the tag
(`0.34` for Surfer 0.7.0). `Color32::from_hex`, the only egui API the plugin
uses, is unchanged across the bump.

And one link is an actual bug rather than a version pin. The plugin declared

```rust
pub fn translators_config_dir(_user_data: ()) -> Json<Option<String>>;
```

against libsurfer's `host_fn!(translators_config_dir() -> …)`, which takes **no**
parameters — so it imported a 1-parameter function. extism 1.13 tolerated that;
extism **1.21** (Surfer 0.7.0) validates import types and refuses to load:

```
Failed to load plugin … incompatible import type for
  `extism:host/user::translators_config_dir`
```

The other two host functions (`read_file`, `file_exists`) take one real argument
on both sides, which is why the error names only this one. Dropping the spurious
`()` fixes it. Worth upstreaming.

That one is worth knowing about because bumping `extism-pdk` cannot fix it —
1.4.1 is the newest published version, and it is already what the plugin uses.

So the chain is: **vendored Haskell commit → plugin built from that same commit →
`surfer-translation-types` tag matching your Surfer → the plugin's `egui`
matching that tag's `ecolor`**, plus a correct host-function arity. Break any
link and the symptom is a parse error, a load error, or a compile error — never a
silently wrong waveform, which is the reassuring part.

Reading the installed binary is the quickest way to pin the targets down:

```bash
strings -a "$(which surfer)" | grep -oE 'extism-1\.[0-9]+\.[0-9]+|e(gui|color)-[0-9.]+' | sort -u
```

Here that reported `extism-1.21.0` and `egui/ecolor-0.35.0` — the latter showing
the installed Surfer is *newer* than tag `v0.7.0` (which pins 0.34.1). That
mismatch is harmless: values cross the wasm boundary as JSON, so the plugin's
`egui` only has to be self-consistent, not equal to the host's.

### Toolchain note

`clash-shockwaves` uses `TypeAbstractions`, so this branch needs **GHC ≥ 9.8**;
`tested-with` drops to `9.10.3` and `check.sh` must run inside the devshell (or
with `-w ghc-9.10.3`). That is the one hard constraint the integration adds.

The `shockwaves-smoke` suite in this repository demonstrates the combination on a
small design with no manual tracing calls at all — a sum type inside a record,
two component instances — asserting the hierarchy in the VCD *and* the
constructors, field names and per-path type keys in the sidecar.

## Generating the waveforms

### When a waveform is written

A green test run writes **no** waveforms, and that is deliberate: recording,
rendering and writing a VCD nobody reads is the dominant cost when tests run in
parallel (measured on 24 cores: 25.2 GB peak and 6m22s with unconditional
recording, versus 8.2 GB and 1m06s without). Two things produce a file:

* **A failing test**, automatically. `withWaveformOnFailure` runs the
  simulation with recording OFF; if the assertion throws it re-runs that
  simulation with recording ON, writes the VCD and its sidecar, and rethrows.
  Passing tests pay nothing at all. If the re-run does not reproduce the
  failure — possible because `clash-vexriscv` resolves undefined CPU inputs
  randomly per run — it says so and writes nothing rather than handing you a
  passing run's waveform labelled as the failure; such a test can switch to
  `withWaveformOnFailure'`, which records as it goes and renders only on
  failure.

* **`CCC_WAVEFORMS=1`**, for artifacts. Captures that exist to be looked at
  rather than to diagnose a failure are gated on this variable:

  ```bash
  CCC_WAVEFORMS=1 cabal test bittide:unittests
  CCC_WAVEFORMS=1 cabal test bittide-instances:unittests
  ```

  A hedgehog property still leaves only ONE waveform: the decision is made in a
  generator before simulating (`recordLargestCase`, in
  `Clash.CircuitContext.Waveform.Hedgehog`), so it picks the largest — most
  thorough — case and the other 99 never record. To get the waveform of a
  counterexample hedgehog already found, replay it with `recheckWithWaveform`
  using the size and seed the failure printed.

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

VCDs land in `waveforms/` **relative to the working directory** — so
`bittide/waveforms/` and `bittide-instances/waveforms/` under `cabal test`,
but wherever you invoked it from under `cabal run`. A capture triggered by a
failure therefore prints its absolute path: for a property it appears in the
failure report beside the counterexample, otherwise on stderr. They are
gitignored; regenerate rather than commit them.

**A green run writes none of them.** Recording is not a filter applied at the
end: a run that will not be kept never enters a recording context, so it costs
no maps, no render and no file. That is what makes the suite affordable in
parallel — 25.2 GB and 6m22s when every run recorded, 8.2 GB and 1m06s now, on
24 cores. Two things do produce a file:

* **a failing test**, always. `withWaveformOnFailure` (IO) and
  `withWaveformCase` (hedgehog properties) re-run the case that failed with
  recording on and write its waveform. For a property that means the waveform
  of the **shrunk counterexample** — the minimal case hedgehog reports, not the
  first one that happened to fail.
* **`CCC_WAVEFORMS=1`**, for the artifacts. Tests whose waveform is documentation
  rather than diagnosis capture only under that flag; a property picks which case
  to keep with `recordLargestCase` (or `recordCaseOfSize` for a smaller, readable
  one), which fires exactly once however often shrinking re-runs its generators.

Two designs cannot use the re-run: the VexRiscv firmware sims resolve undefined
CPU inputs randomly (`unsafeMakeDefinedRandom`), so a second run may differ.
`withWaveformOnFailure'` records as it goes for those, rendering only on
failure; where a re-run does not reproduce, the harness says so on stderr rather
than writing a passing run's waveform under a failure's name.

### The 29 waveforms

**`bittide:unittests` — 15**, from `bittide/tests/`. Plain Clash designs sampled
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
| `prop_alignDealignLsb`, `prop_alignDealignMsb` | `Tests/Transceiver/WordAlign.hs` |
| `bench_correctness`, `bench_recording` | `Tests/Bench.hs` — tracing-overhead benchmark |

`Tests/Counter.hs` holds three more call sites (`case_zeroSameDomain`,
`case_zeroSrcRst`, `case_zeroDstRst`), but they do **not** run: `Tests.Counter` is
listed in `bittide.cabal`'s `other-modules`, so it compiles, yet `UnitTests.hs`
never imports it into the tasty tree. That is pre-existing upstream, not something
the instrumentation introduced. Reach them via the REPL recipe below.

**`bittide-instances:unittests` — 14**, from `bittide-instances/tests/`. Each is
a RISC-V firmware self-test `sampleC`-ing a `Circuit () (Df dom (BitVector 8))`
UART DUT built around a VexRiscv CPU. Each captures the cycles its assertion
actually forced rather than a fixed window:

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
import Protocols.Waveform (withWaveformC)
import Clash.CircuitContext.Waveform (withWaveformSlot)
import VexRiscv (DumpVcd (NoDumpVcd))
pc <- peConfigSim
_ <- withWaveformSlot "registerwb_sim" $ \wf -> withWaveformC wf 2 150000 "uartTx" id (unMemmap (dutWithVcdAndPeConfig NoDumpVcd pc))
putStrLn "DONE"
```

and run it against the test suite (`Protocols.Waveform` lives in
`bittide-extra`, the rest in `clash-circuit-context`):

```bash
cabal repl bittide-instances:unittests < regwb.ghci
```

The `withWaveformC` arguments are: the slot, reset cycles, cycles to sample,
traced wire name, a projection of the circuit's forward output, and the fully
driven circuit. The slot is what reaches disk: `withWaveformSlot` creates one,
runs the action, and writes whatever it captured — even if the action threw.

Designs sampled with `sampleN`/`simulateN` rather than `sampleC` use
`withWaveform` instead, and long-running firmware tests that should stop at
their own exit condition use `withWaveformLazy`, which captures the cycles the
consumer actually forced instead of a fixed window.

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
