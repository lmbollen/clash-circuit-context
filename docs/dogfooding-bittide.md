# Developer-experience findings: dogfooding `clash-circuit-context` on `bittide-hardware`

This is a running record of what we learned by instrumenting a real Clash
project (`bittide-hardware`) with the tracing runtime and the auto-instrumentation
plugin: what works, where a developer hits friction, and the concrete package /
plugin changes that address it. It is meant to drive the roadmap — each item is
tied to a real example, not a hypothetical.

## Repo-wide framing + probing: pros, cons, and the limits of the approach (2026-07-21)

Goal of this pass: instrument `bittide-hardware` the way a *mature* project would
— every reusable component a scope, internal combinational and state-machine
signals visible — and record where the approach helps and where it hits a wall.
The trigger was a real red flag: CPU-based simulations showed **no interconnect
component** — the whole processing element flattened into one pile of ~30
un-scoped wires under the DUT.

**What fixed the red flag.** A component only gets its own VCD scope if it is
`{-# OPAQUE #-}` (that is ccc's definition of a component). Almost none of
bittide's `HasCircuitContext` functions were OPAQUE, so they flattened. Marking
the structural components OPAQUE (`processingElement`, `rvCircuit`,
`singleMasterInterconnect(C)`, `arbiter(Mm)`, `uartInterfaceWb`, `fifoWithMeta`,
+ every other component repo-wide) produced the hierarchy a developer expects:

```
dutWithPeConfig
  processingElement
    arbiter            rvCircuit (CPU — empty, see limits)
    singleMasterInterconnectC  › singleMasterInterconnect
  receiveRingBuffer   timeWb   transmitRingBuffer
  uartInterfaceWb  › fifoWithMeta_0  fifoWithMeta_1
```

Signal counts, firmware DUTs, over this whole effort: dna 2→**35**,
capture_ugn 7→**40**, registerwb 2→**35**, ring_buffer 7→**46**; scopes 1→**11–13**.

**Pros — what genuinely works well.**
- OPAQUE + `HasCircuitContext` gives a nested hierarchy that mirrors the HDL
  module structure for free — no manual `$scope` bookkeeping.
- `probeFmap` reaches a whole class of signals nothing else could: the internals
  of a *combinational* function lifted over signals (`route`'s `toMaster` /
  `oneHotOrZeroSelected` / `addrIndex` inside `singleMasterInterconnect`).
- `mealyBProbed` / `mooreBProbed` cover the dominant *bundled* mealy idiom;
  `fifoWithMeta`'s `fifoEmpty`/`fifoFull`/`readSuccess`/`readAddr` now show,
  and the two FIFO instances auto-disambiguate as `_0`/`_1`.
- Fully HDL-transparent: everything gates on `clashSimulation`; suite stays
  90/90 and the synthesis instances still compile.

**Cons / limits — the findings that matter for the roadmap.**
1. **Viral `HasCircuitContext` is the dominant cost.** Every component *and every
   transitive caller* needs the constraint (or a `withoutCircuitContext`
   boundary). This pass touched ~50 files almost entirely for constraint
   plumbing. This is the single strongest argument for non-viral tracing
   (roadmap #0).
2. **OPAQUE-for-framing is a silent footgun.** Forget it on one component and its
   internals silently flatten into the parent — exactly the reported red flag.
   The plugin should *warn* when a `HasCircuitContext` component lacks OPAQUE
   (F6). Also: OPAQUE is a real Clash directive (blocks inlining/specialization;
   each becomes its own HDL module). Simulation + `cabal build` are green, but
   that does **not** exercise Clash HDL generation — the synthesis impact of
   OPAQUE-ing hot polymorphic cores like `processingElement` is unverified here
   and is a genuine open risk.
3. **The per-site `HasProbe` signature is the throughput bottleneck.** Probing a
   mealy step or a combinational function requires a *written* `HasProbe`
   signature so the plugin switches to probe mode; an unsignatured `go` needs its
   full type hand-written. Protocol-typed steps (`Df`'s `Data`, Axi4 records) are
   especially painful and error-prone. This — not any constraint — is why
   "instrument everything" (≈30 mealy + ≈305 combinational sites) is a multi-day
   sweep. It is the clearest lever for a future plugin improvement (infer/inject
   the signature, or a partial-signature form).
3a. **Partial signatures do NOT rescue the bottleneck** (tested on Axi4's
   `wbToAxi4MemoryMapped`). The obvious shortcut — `go :: HasProbe => _` with
   `PartialTypeSignatures`, letting the plugin read only the constraint spine and
   GHC infer the rest — *fails*: a `where`-bound step closes over the enclosing
   function's `forall` type variables (`nBytes`, `addrW`, record-field
   constraints), and the wildcard `_` introduces *fresh* variables under
   `MonoLocalBinds`, disconnecting them (`Could not deduce KnownNat nBytes1`,
   `HasField "raDone" r4`). So the signature genuinely must be written out by
   hand, tied to the outer scope — the bottleneck is fundamental, not cosmetic.
   (The plugin injecting the `HasProbe` constraint onto the *inferred* type,
   rather than requiring a source signature, is the real fix.)
3b. **Most mealy/moore steps have no probeable internals.** A large fraction of
   step functions are bare pattern-match equations returning tuples
   (`goCapture`, `timeout`'s `goMealy`, `tracker`, the ElasticBuffer/AutoCenter
   moores) — no named `where`/`let` bindings, so `probe` has nothing to record;
   only the machine's I/O (already at the boundary) is observable. Probing only
   pays off where the design *names* intermediate values (`fifoWithMeta`,
   `arbiter`, Axi4's state machines). "Convert every mealy" is the wrong target;
   "convert every mealy whose step names intermediates" is far smaller.
4. **Combinational probing needs a call-site rewrite, not just a constraint.**
   `f <$> a <*> b` must become `probeFmap hasClock (uncurry f) (bundle (a,b))`
   — bundle the inputs, thread a clock, add a lambda. That breaks the "add a
   constraint, the plugin does the rest" ideal and argues for a plugin that
   rewrites `<$>`/`<*>` chains directly.
5. **The CPU is a hard blackbox.** `rvCircuit`'s scope is empty: VexRiscv is an
   FFI/Verilator model, so no CPU-internal state is reachable in a CPU sim. This
   caps how much a firmware DUT waveform can ever show from the core itself.
6. **The clash-protocols boundary.** The wishbone register/device machinery
   (`deviceWbI`, `registerWbI`, the memory-map plumbing) lives in
   clash-protocols, not bittide. A large share of the "missing" bus-level
   signals are inside code the design cannot instrument — instrumentation stops
   at the dependency edge.
7. **"Probe everything" is the wrong rule.** Most of the 305 `<$>`/`<*>` sites are
   `fmap pack` / `liftA2 (+)` with no named internals; wrapping them adds a
   counter and an empty scope for nothing. The right rule is "probe named
   functions that have internal bindings" — which is far fewer than 305.
8. **Closed polymorphic locals can't be probed** (F9): auto-tracing them breaks
   GHC generalization, so they are deliberately skipped.
9. **Recording timing is a one-cell-wide design point.** Recording a traced
   signal eagerly while *producing* its cons cell deadlocks on clash-protocols
   value knots (blackhole re-entry: `<<loop>>`/silent hang, and value-level
   re-entries were caught and silently recorded as `x` — corrupt waveforms);
   recording lazily at dump time retains the raw history (the space leak). The
   only safe point is one cell later — record cycle *i* when cell *i+1* is
   forced — which is what ccc's tap now does.
10a. **Two-run capture was the suite killer.** The original capture design
   ("accept the extra run") simulated every firmware DUT twice — a lazy
   assertion run that exits early plus a strict full-window re-run just for
   the waveform. With CPU boots of 10⁵–10⁶ cycles that made every test
   150–650 s and stacked GBs across tasty's parallel workers. The fix is
   architectural: `withWaveformLive` records DURING the assertion run and
   dumps `recordedCycles` — one simulation, bounded by the assertion's early
   exit. Measured: watchdog 155 s → 3.5 s, ring 654 s → 22 s, dna 401 s → 77 s.
10b. **Per-cycle probe qualification was the hidden constant.** Resolving the
   hierarchy + building the qualified-name string + inserting into a global
   string-keyed map happened per probe per cycle; a per-instance cache
   (resolve once) removes it. Beware: the cache allocation must be tied to
   the instance identity or full laziness shares one cache across all
   instances (the goldens caught the merged probes).
10b2. **Hedgehog defaults are a trap for CPU-simulating properties.** A
   `property $ …` runs 100 cases by default; WbToDf's each did an ELF load, a
   strict 100 k-cycle capture and a model-driven CPU sim — the slowest test in
   the suite by far. `withTests 1` plus a live, output-count-bounded capture
   brought it to 0.6 s.
10b4. **Record only what you keep.** Even leak-free, recording every run is
   wasteful: a 100-case property rendered 100 VCDs to keep one, and a passing
   firmware test rendered 270 MB nobody reads. Under a parallel runner that is
   the dominant cost. The capture decision now happens BEFORE the simulation
   wherever possible (`withWaveformWhen`; the losing cases run under
   `noCircuitContext`, where tracing is identity), and a failing test gets its
   waveform by re-running with recording on (`withWaveformOnFailure`).
   Measured on all 24 cores: peak 25.2 GB -> 8.2 GB and wall 6m22s -> 1m06s
   for bittide-instances, with zero VCDs written on a green run. The wall-clock
   win is the surprise — per-cycle packing and RLE encoding cost real time, not
   just memory.

10b3. **A serializing lock was the wrong fix — the leak was.** Tasty running N
   CPU tests concurrently held N partially evaluated sims (GBs each), so the
   harness serialized heavyweight sims on a global lock (`heavySimLock`). That
   treated the symptom: what made concurrency unaffordable was per-test
   retention (a lazy sidecar pinning every run's history, and a process-global
   store holding every rendered VCD to the end of the suite). With capture
   bounded per test, the lock was removed and the suite runs on all cores
   again. Serializing to control memory hides the retention bug that makes
   serializing feel necessary.
10c. **What remains is the simulation itself.** Control-measured: dna's
   UN-instrumented assertion run alone costs ~60 s and ~3.3 GB max residency
   (ghci) — sampleC + the VexRiscv FFI over 540 k cycles. Recording now adds
   ~28%% time and no residency. Further suite gains must come from the
   simulation infrastructure (or capping tasty parallelism for these tests),
   not from the recorder.
10. **RLE meets its floor on CPU signals.** Counters, PCs and busses change
   nearly every cycle, so change-compression ≈ O(cycles) for them (42k–55k runs
   per 100k cycles measured on the ring DUT); a 10⁶-cycle full-window capture
   of a CPU DUT is intrinsically GB-scale — in the VCD too. Right-size capture
   windows; a streaming-to-disk recording mode is the roadmap item that removes
   the in-memory floor entirely.

Net: the framing/hierarchy story is a clear win and the red flag is fixed; the
probing *mechanism* is proven for all three idioms (signal, bundled-mealy,
combinational); the *scaling* of probing is gated by the viral constraint and the
hand-written-signature bottleneck, which together define the top of the roadmap.

### Executing the full sweep: what converted, and the demonstrated wall

Pushing past ROI triage, the previously-deferred modules were actually attempted:

- **Counter — fully instrumented.** Both CDC mealy machines
  (`domainDiffCounter`'s `go`, `extendSuccCounter`'s `go`, both already
  signatured) converted; `HasCircuitContext` threaded through the 3-deep
  primitive chain (`extendSuccCounter` → `synchronizedSuccCounter` →
  `domainDiffCounter` → `domainDiffCountersWbC`), the one test caller discharged.
- **SPI — the rich machine instrumented.** `si539xSpiDriver`'s `go` (signatured,
  the state machine with real internals) converted; `HasCircuitContext` threaded
  through `si539xSpiDriver`/`si539xSpi`/`si539xSpiDriverC` and the 4 synthesis/HITL
  callers (`Pnr/Si539xSpi`, `Hitl/{Transceivers,LinkConfiguration}`, the test)
  discharged.
- **Axi4 — the signatured machine instrumented.** `axiRxHandler`'s `goMealy`
  (signatured) converted via `mealyBProbed`; `HasCircuitContext` threaded through
  `axiRxHandler` → `wbAxisRxBufferCircuit#` → `wbAxisRxBufferCircuit`, the property
  test + `Pnr/Ethernet` discharged, and the context left *flowing* into the
  simulated Axi DUT so the probes land in its waveform.

- **The wall (demonstrated, not assumed).** The remaining Axi4 machines
  (`wbToAxi4MemoryMapped`, the stream converters, `axiStreamPacketFifo`, …) and the
  two unsignatured SPI machines are blocked by the signature bottleneck, and the
  natural escape was *tested and shown to fail*: on `wbToAxi4MemoryMapped`, giving
  the step a partial `go :: HasProbe => _` under `PartialTypeSignatures` produces
  **unsolvable** fresh unification variables (`Could not deduce KnownNat nBytes1`,
  `HasField "raDone" r4`) because the step closes over the enclosing `forall`'s
  `nBytes`/`addrW` — GHC cannot even *report* a concrete type to copy. Writing the
  signature by hand would mean spelling out clash-protocols' internal
  `Fwd`/`Bwd`/channel type-family applications, which are not a stable, hand-authorable
  surface. So these are left at component-framing level not by ROI choice but
  because the source-signature requirement is a hard wall here — which is the
  precise, concrete case for the roadmap fix: **the plugin must inject the
  `HasProbe` constraint onto the compiler-inferred type, so no source signature is
  needed at all.**

## What a developer gets, and how little they write

For an instrumented component the developer writes **three markers**, and the
plugin generates everything else:

```haskell
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}   -- (1) enable
...
myComponent :: (HasCircuitContext, ...) => ... -> Signal dom o   -- (2) opt in
myComponent clk rst inp = out where ...
{-# OPAQUE myComponent #-}                                        -- (3) boundary
```

From those markers the plugin (renamer pass) injects `component "myComponent"`
around the body and `autoTrace "x"` on every local `let`/`where` binding, and
the typechecker oracle makes those injections always compile (falling back to a
no-op when a signal is not `BitPack`). The developer never writes `component`,
`traceSignalC`, or `Traceable` instances by hand. Verified end to end: the
Handshake test's `dut → handshakesWithDelays → handshake_{0,1}` VCD hierarchy,
with every FSM signal, is 100% plugin-generated.

**Confirmed working on real code:** hierarchical VCD scopes from `OPAQUE`
components; per-signal tracing of local bindings; tuple/record pattern bindings
(`(a,b) = unbundle …`); generic `Traceable` records nesting field-wise; multiple
instances of one component auto-disambiguated (`handshake_0`/`handshake_1`);
`mealyProbed` probing mealy-step internals; HDL-transparency (instrumented
designs still synthesize — `bittide-instances` builds, Verilog is clean).

## Friction found, and status

### F1 — `HasCircuitContext` is viral; cutting it needed a hand-written shim per component ✅ FIXED
`HasCircuitContext` propagates to every transitive caller. To keep a
synthesis-facing caller context-free, each instrumented component grew a
`fooNC = let ?circuitContext = noCircuitContext in foo` shim that **duplicated
foo's entire type signature**. bittide accumulated five of these
(`generatorNC`, `checkerNC`, `trackerNC`, `handshakeNC`, `xilinxElasticBufferNC`)
— ~70 lines of pure boilerplate.

**Fix (shipped):** added `withoutCircuitContext :: (HasCircuitContext => r) -> r`
to the package. A synthesis caller now writes
`withoutCircuitContext (foo a b c)` inline — no shim, no signature copy. All five
bittide shims were deleted; `bittide-instances` still builds and the tests still
produce identical VCDs.

**Usage guideline (2026-07-22):** `withoutCircuitContext` binds
`?circuitContext = noCircuitContext` lexically over its *whole* argument, so a
context-free function needs it **once at the result**, not per sub-application.
A `circuit`-notation block wraps as
`f = withLittleEndian $ withoutCircuitContext $ circuit $ \… -> do …` and every
inner `processingElement`/`uartInterfaceWb`/… inside the block (even under a
nested `withClockResetEnable`/`fmapC`) resolves to that one context — no need to
wrap each `-<` sub-application. Swept the repo's collapsible multi-wrap functions
(`processingElement`/UART/`timeWb` synthesis instances, GenericDemo `core`
had 7 in one block): ~30 redundant `withoutCircuitContext` call sites removed,
one per component boundary retained. (Separately *bound* context-free helpers —
`where foo = withoutCircuitContext …` used in several places — are already
one-per-boundary and were left as is.)

**Where `withoutCircuitContext` belongs (2026-07-22):** only at a boundary that
*samples* a circuit and wants no hierarchy — a synthesis top (Pnr/Hitl instance)
or a sampling test. A **library component** that is itself part of the traced
hierarchy must NOT self-discharge; it should carry `HasCircuitContext` and let
its caller decide, so its sub-components nest in the waveform. Fixed
`Bittide.Transceiver`: `transceiverPrbsN`/`transceiverPrbs`/`transceiverPrbsWith`
were discharging `Prbs.generator`/`checker`/`tracker` internally with
`withoutCircuitContext`, which hard-coded them *out* of every waveform even when
the enclosing `transceiverPrbsNC` (OPAQUE + `HasCircuitContext`) was being traced.
Threaded the constraint through the three functions and removed the three inner
wraps; the discharge now happens once at each real boundary — the two HITL
synthesis instances (`Hitl/Transceivers`, `Hitl/LinkConfiguration`) and the
`Tests.Transceiver` sim harness. Now, captured under a recording context, the
Prbs machines nest under `transceiverPrbsNC` (per-channel instances via
StableName); at a synthesis/sampling boundary they reduce away as before.

### F2 — Test-harness signals are not auto-traced
The plugin only traverses top-level functions whose signature carries
`HasCircuitContext`. Signals that live in a Hedgehog property's `do`-block or a
test lambda (e.g. the PRBS pipeline wiring, a RAM's driven inputs) are invisible
to it, so we traced them with explicit `traceSignalC` calls. This is inherent to
the opt-in-by-signature design.

*Options:* (a) accept explicit `traceSignalC` in tests (a handful of lines); or
(b) lift the harness into an instrumented component so the plugin covers it.
Neither is a package change; documenting the pattern is probably enough.

### F3 — `traceSignalC` resolves its context lexically, which surprises
`traceSignalC` reads `?circuitContext` at its **definition site**, not the call
site. Putting it inside a `topEntity` defined in an outer `let` and then calling
that `topEntity` under `withWaveform` does **not** pick up the context — the
`topEntity` must be defined *inside* the `withWaveform` argument (or take
`HasCircuitContext`). This bit us in `Tests.DoubleBufferedRam`.

*Proposal:* a short "gotchas" section in the README, since it is a direct
consequence of implicit parameters and cannot be designed away.

### F4 — No `mooreProbed` ✅ FIXED (bundled/`mealyB` variants still open)
Only `mealyProbed` existed. `Bittide.Transceiver.Prbs.generator` is written with
`moore`, so its step could not be probed without rewriting it as a mealy.

**Fix (shipped):** added `mooreProbed` to the package, mirroring `mealyProbed`
(companion counter for cycle identity; the *transition* function is `HasProbe`,
the output projection is left unprobed). Dogfooded: `generator` moved `moore →
mooreProbed`, and its `prbs` shift-vector now shows per cycle in the `generator`
VCD scope. `mealyBProbed`/bundled variants remain open but low-demand.

### F5 — `HasProbe` is viral into per-element helpers (footgun) 🔜 PROPOSED
Marking a mealy step `HasProbe` puts the *whole* lexical subtree in probe mode,
including helpers called N times per cycle (`mapAccumL`/folds). Their local
bindings then get auto-probed with colliding `(name, cycle)` keys (lossy) and a
real perf hit — Prbs `checker` was 57.8s→19.0s just by `_`-prefixing two such
bindings. The opt-out works but is silent/surprising.

*Proposal:* plugin should not descend into multi-argument local functions in
probe mode (or detect multi-write-per-cycle and warn).

### F6 — The `OPAQUE` marker is easy to forget, and its absence is silent ✅ FIXED
A component needs both `HasCircuitContext` *and* `{-# OPAQUE #-}`. With the
constraint but no `OPAQUE`, the plugin still traces the local bindings but does
**not** create a `$scope` for the function — the signals silently flatten into
the parent. `OPAQUE` is also required for correct per-instance identity (heap
identity relies on the call not being floated/shared).

*Fix (shipped):* `-fplugin-opt=Clash.CircuitContext.Plugin:diagnostics` reports
it, along with the other near-misses whose symptom is a missing scope or wire:
`OPAQUE` without a type signature, a signature carrying both `HasProbe` and
`HasCircuitContext`, and (from the oracle half) every binding whose payload
type was declined, with the requirement it got stuck on. Off by default —
the silent fallback is what makes package-wide enablement safe, so the
reporting is a build flag, not a warning. Auto-injecting `OPAQUE` was not
done: it risks a silent instance-identity bug if the injection is a no-op at
that stage.

Two neighbouring silent failures went with it, both found from the Helios
dogfooding rather than bittide's:

* a `HasCircuitContext` behind the designer's own constraint synonym
  (`type SwitchCtx dom = (HiddenClockResetEnable dom, HasCircuitContext)`) was
  invisible to the renamer, which runs before synonyms are expanded — the
  function kept tracing but silently lost its `$scope`. Both kinds of synonym
  are now followed: a module-local one from the group being compiled, an
  imported one from its interface.
* enabling the plugin twice (package-wide *and* an `OPTIONS_GHC` pragma) ran
  the renamer twice and nested every component wrap in itself
  (`switch.switch`). The pass is now idempotent, which also means hand-written
  `component "f"` in a binder the plugin would wrap is left alone instead of
  doubled.

Pinned by the `plugin-diagnostics` suite, which enables the plugin twice on
purpose and greps its own build log for the diagnostics.

### F7 — Waveform lifecycle helpers are re-implemented per project ✅ FIXED
`withWaveform`/`withWaveformC`, the one-file-per-name discipline, "keep the last
run", and the `clash-protocols` `toSignals` bridge all live in a bittide-side
`Tests.Waveform` helper. Every project that wants VCDs from tests will
re-derive them, and the subtleties (force once to avoid double-instantiation;
`dumpVCDC` must not throw on an empty run; last-run-wins via a deferred flush at
teardown) are easy to get wrong.

*Shipped* as `Clash.CircuitContext.Waveform` (slots, capture, writing) plus
`Clash.CircuitContext.Waveform.Hedgehog` (which case of a property leaves a
waveform). No separate package was needed: only the `toSignals` bridge is
`clash-protocols`-shaped, and that one edge stays project-side —
`clash-protocols` depends on this package, so depending back would be a cycle.
bittide keeps its `withWaveformC`/`driveFwd` in `bittide-extra`.

### F8 — Auto-tracing a partial binding crashed the whole VCD dump ✅ FIXED
Instrumenting real peripherals (the `bittide-instances` firmware DUTs) surfaced
this immediately. `Bittide.CaptureUgn.captureUgns` has a `where` binding
`rawLinkIns = fromJust <<$>> linkIns` — defined only after a link comes up; the
design never forces it earlier. Auto-instrumentation traces *every* named
binding and the VCD dumper samples each every cycle, so it forced `rawLinkIns`
during the pre-link-up window and the simulation died with the design's own
`Maybe.fromJust: Nothing` — a failure that never happens in normal (lazy,
conditional) simulation. Two compounding causes: the trace path sampled with
`sample` (forces each element to NF *before* any guard), and the probe path
guarded with `isX`, which catches `XException` but not a plain `ErrorCall`.

**Fix (shipped):** trace samples are taken with `sample_lazy`, and every
sample — trace and probe alike — is forced through a shared `packMaskValue`
that catches ANY exception and records an all-undefined (`x`) entry. A tracer
must never change whether the simulation crashes; a binding the design leaves
undefined at a cycle now simply shows `x`. Verified: `captureUgns` dumps a
6-signal VCD scope (`triggers`, `remoteCounters`, `rawLinkIns`, …) with
`rawLinkIns` as `x` until the link is up, and ccc's own golden VCDs are
unchanged.

### F9 — Auto-tracing destroyed the polymorphism of closed local helpers ✅ FIXED
Instrumenting `clockControlWb` and `timeWb` failed to compile
(`Couldn't match type 'Bool' with 'SpeedChange'` / `'TimeCmd' with 'Unsigned 64'`
inside `taggedCircuit`). The original diagnosis blamed forward-referenced
do-block `let`s in the Protocols `circuit` DSL — **wrong**, as a minimal repro
with forward references compiled fine. The real mechanism, found by re-adding
the constraint to the real `timeWb` and reading the error closely:

Both peripherals share a **closed polymorphic where-helper used at several
types** — `noWrite = pure Nothing :: Signal dom (Maybe a)`, passed to registers
of different payload types. GHC generalizes closed local bindings even under
`MonoLocalBinds` (GHC2024). Wrapping the binding in `autoTrace` injects the
constraint `AutoTrace (CanTrace t) t`, whose `CanTrace` **type-family
application is not quantifiable in an inferred type**; GHC drops it from
quantification and monomorphizes the binder at its first use — the second use
at a different payload type is then a type error. The Protocols DSL was
incidental: circuit blocks just make such helpers common, and `taggedCircuit`
makes the error unreadable.

*Fix (ccc renamer, `closedBind`):* skip auto-tracing local bindings whose
free-variable set (the renamer's own `fun_ext`/`pat_ext` NameSet) contains no
local names besides the binder itself. Closed bindings are exactly the ones
GHC generalizes — and they are constants by construction, so their trace is a
flat line; skipping them costs almost nothing. Open bindings (everything that
references a port, argument or sibling — all the signals worth watching) are
monomorphic anyway and keep tracing. Regression-tested in ccc's auto-instrumentation golden suite
(`polyHelper`/`quiet`: compiles, `quiet` untraced, its two typed views traced).

*Known trade-off:* a GHC-closed binding that is *actually* monomorphic (a
self-contained `cnt = register 0 (cnt + 1)` referencing no argument — its
hidden-clock constraint makes GHC monomorphize it) is also skipped; trace it
explicitly with `traceSignalC` if it matters.

*Result:* `timeWb` and `clockControlWb` instrument cleanly; the Time DUT
waveform now shows `count`, `cmdWaitAck`, `cmpResult`, `cmpResultWrite`,
`scratchWrite` (with `noWrite`/`freq`/register configs correctly skipped).

### Repo-wide `HasCircuitContext` propagation (2026-07-21)
`HasCircuitContext` now sits on every component function in `bittide/src` and
the `bittide-instances` test DUTs — `processingElement`, `rvCircuit`, all
Wishbone circuits (`uartInterfaceWb`, `watchDogWb`, `singleMasterInterconnect(')`,
`arbiter(Mm)`, `fifoWithMeta`, `dfWishboneMaster`), `wbStorage(')`,
`asciiDebugMux`, `wbToDf`, `axiMemoryMappedToWb`, `timeWb`, `clockControlWb`,
`bootPe`, `callistoSwClockControlC`, `freeze`, `si539xSpi{C,Wb,DriverC}`,
`domainDiffCountersWbC`, `sync*C`, `ethMac1GFifoC`, `macStatusInterfaceWb`,
`programmableMux`, `shutter`, `timeout`, `autoCenter`, `wireDemoPe`,
`transceiverPrbsN{C,Wb}`, plus the previously instrumented peripherals.
Skipped by policy: stateless adapters (`andAck`, `dupWb`,
`extendAddressWidthWb`, `uartBytes`, `uartDf`, `axiUserMapC`, `jtagChain`),
synthesis-only debug (`ilaWb`), and `*Worker` shims. Boundaries: every
Pnr/Hitl synthesis instance and every memory-map extractor discharges with
`withoutCircuitContext`; test assertion paths (`sampleC`) discharge too, while
waveform paths keep the context.

*Effect on waveforms* (2 kcycle repl captures): dna 2→16 vars,
capture_ugn 7→20, registerwb 2→16, ring_buffer 7→27, time (new) 21 — the
processingElement internals (`cpuOut`, `m2s`/`s2m`, UART FIFO state) now
appear in every firmware DUT trace. Note they appear *flat* under the DUT
scope: `processingElement` is not OPAQUE, so its bindings trace at the
caller's scope (see F6) — adding OPAQUE to key shared cores is the natural
next refinement. Validated: bittide suite 90/90 (90.9 s), both packages +
suites green, ccc check.sh ALL CHECKS PASSED.

### F10 — Repo-wide plugin enablement is a safe no-op for uninstrumented modules ✅ VALIDATED
Flipped both packages from 13 per-module `OPTIONS_GHC -fplugin` pragmas to a
single `-fplugin=Clash.CircuitContext.Plugin` in each cabal `clash` common
stanza (next to `Protocols.Plugin`). Result, measured A/B on seven waveforms
(4 firmware DUT VCDs via repl + `case_trackerWaveform`,
`case_xilinxElasticBufferMinBound`, `prop_handshake`): **bit-for-bit identical**
(instance DUT VCDs identical including values, modulo `$date`). Full tree
builds green (74 s wall for both packages + suites), bittide suite 90/90.

Why: opt-in is by *signature*, not by module — the renamer only touches
top-level binders whose written signature spine carries
`HasCircuitContext`/`HasProbe`, and the constraint synonym in **argument
position** (`withWaveformC`'s `(HasCircuitContext => Circuit () b)`,
`withoutCircuitContext` call sites) never triggers it. F9 cannot fire in
unconstrained modules for the same reason. *Recommendation:* enable the plugin
package-globally as the default workflow — the per-module pragma is pure
boilerplate, and with the global flag a newly constrained function is
instrumented with zero further ceremony. (One caveat worth documenting in the
ccc README: the plugin listed at both package and module level runs twice —
remove module pragmas when going global.)

### F11 — A hedgehog failure is invisible to an IO-level capture ✅ FIXED
Capture-on-failure was built in IO: run with recording off, `try` the consumer,
and on an exception re-run with recording on. That covers HUnit and the firmware
tests, and covers **nothing** in a hedgehog property. `===`, `assert` and
`failure` do not throw — they put a `Failure` into `PropertyT`'s `ExceptT`
layer, which no `try` in IO can observe. Verified the hard way: breaking
`prop_alignDealignMsb` produced a perfectly good printed counterexample and
zero waveform files.

The fix is `Clash.CircuitContext.Waveform.Hedgehog.withWaveformCase`, which runs
the property monad itself (`runTestT`/`mkTestT`) to catch the failure as a
value, re-runs that case under a recording context, and re-raises the failure
unchanged so hedgehog still shrinks and still reports exactly what it would
have. Two properties fall out of it that are better than the IO version:

* **the waveform is of the SHRUNK counterexample.** Shrinking re-runs the
  property on ever-smaller inputs and each failing case overwrites the slot, so
  what survives is the minimal case hedgehog prints. Measured on the broken
  `prop_alignDealignMsb`: a 2-cycle, 4-wire, 384-byte VCD showing 8-bit `input`
  going 0→1 while `aligned` never follows — the whole bug, and nothing else.
* **only failing cases pay.** A passing property records nothing at all, so the
  cost is one recording per step down the shrink path.

Two constraints to document, both from the re-run: the consumer must contain no
`forAll` (the second run would draw different values — the seeds differ by
position in the bind chain), and it must be assertions and forcing rather than
one-shot IO.

This required `withCircuitContextWindowM` in `Core`: a recording context for an
action in any `MonadIO`, with no `try`, precisely because the monads that need
it report failure as a value rather than by throwing.

The same shape covers the artifact case, so it is one combinator, not two:
`withWaveformCase keep` records as it goes when `keep` (no re-run, no
reproducibility needed) and falls back to capture-on-failure otherwise. A case
either passes or fails, so it is never simulated twice.

*Also added:* `recordCaseOfSize fired n`, since hedgehog's `Size` is the only
knob on how big a generated case is and therefore on how big its waveform is.
Demonstrated on one property, one run, both waveforms: **size 10 → 2 cycles,
16-bit words, 470 B; size 90 → 32 cycles, 64-bit words, 6.2 kB.** Small enough
to read by eye, or large enough to be representative — the caller picks.

## Dogfooding `bittide-instances` (firmware DUTs)

The `bittide-instances` unit tests are RISC-V firmware self-tests: each
`sampleC`s a `Circuit () (Df dom (BitVector 8))` UART DUT built around a
VexRiscv CPU. Instrumenting them "as a designer would" (add `HasCircuitContext`
+ `OPAQUE` to a peripheral, enable the plugin, let it trace) produced correct
hierarchical VCDs through the `clash-protocols` `circuit` DSL — e.g.
`dutWithMm → readDnaPortE2Wb → maybeDna` and `dutWithMm → captureUgns → {6
signals}`. Findings specific to this larger surface:

* **Viral reach into `processingElement`.** Three of the eleven target
  peripherals (`wbStorage`, `singleMasterInterconnectC`, `watchDogWb`) are
  instantiated *inside* `processingElement`, which every DUT and every
  synthesis instance uses. Giving them `HasCircuitContext` either goes
  repo-wide or forces `withoutCircuitContext` inside `processingElement`
  itself — at which point the CPU-internal instances are no longer traced. A
  peripheral shared with the CPU core cannot be cheaply opted into tracing.
  Reinforces F1: the viral constraint does not scale to widely-shared
  components; a non-viral opt-in (trace "from the outside") would help.
* **Two of those three (`wbStorage`, `watchDogWb`) have no `where`/`let`
  Signals anyway** — pure `circuit`-DSL `<-` wiring or a bare `mealy` — so
  instrumenting them yields an empty scope for maximal viral cost. For those
  tests the UART output is captured at the test boundary with `withWaveformC`
  (no design change), which is enough to "instrument the test".
* **`timeWb` fan-out.** The single richest shared peripheral has 7 direct
  synthesis call sites plus 2 library intermediaries (`bootPe`,
  `callistoSwClockControlC`) — nine `withoutCircuitContext` edits for one
  peripheral. Purely mechanical, but it is the concrete cost of F1 at scale.
* **Strict re-run cost.** `withWaveformC` re-runs the DUT strictly (accepted:
  we take the perf hit rather than change `sampleC`). Firmware UART output can
  be very late (DnaPortE2's first byte is at cycle ~540k; Axi's at 342), so
  the window is sized per test and a full-UART capture of a late printer costs
  ~70 s + ~7.5 GB for one run.

**Final coverage (all 12 firmware tests instrumented).** Rich peripheral
instrumentation (component scope + auto-traced internal signals), 5 tests:
DnaPortE2 (`readDnaPortE2Wb`), CaptureUgn (`captureUgns`, 6 signals),
RegisterWb (`manyTypesWb`), ElasticBufferWb (`xilinxElasticBufferWb` + its inner
buffer), RingBuffer (`transmit`+`receiveRingBuffer` as sibling scopes).
UART-only capture (top-level output trace, no design change), 7 tests: Axi,
Watchdog, NestedInterconnect, WbToDf, AddressableBytes (weak/empty peripheral),
ClockControlWb, Time (rich peripheral blocked by F9). *Superseded 2026-07-21:*
after the F9 fix and the repo-wide propagation (see above), ALL 12 DUTs carry
`HasCircuitContext` and trace rich — including the processingElement
internals — the UART-only tier no longer exists. Validated: the whole
`bittide` + `bittide-instances` tree compiles on GHC 9.10.3 (both libraries and
both test-suites, including the `withoutCircuitContext`-patched Pnr/Hitl
synthesis instances); VCDs repl-confirmed for DnaPortE2, CaptureUgn, RegisterWb,
RingBuffer. The tasty suite is not runnable in this environment (its cargo step
needs `clang`), so per-test VCD generation is via `cabal repl` + compile.

## Waveform coverage + value-correctness audit (2026-07-22)

Systematic audit of every generated VCD (`bittide-hardware/waveforms/*.vcd`, 14
testbenches): (1) does each capture the signals a designer would want, and (2)
are the recorded values actually correct?

**Method.** A single streaming pass over each VCD classifies every declared
signal as all-`x` / no-data / constant / toggling (`scratchpad/vcdaudit.py`),
and a UART decoder reconstructs the CPU's `uartInterfaceWb.txFifoIn` byte stream
straight from the waveform (`scratchpad/uartdecode.py`) to compare against the
string each test asserts on.

**Value correctness — strong.** Zero all-`x` and zero no-data signals across all
14 waveforms: the tail-shift recording law (see the recorder note above) holds
in practice — no silent `x`-corruption anywhere. Every constant signal is a
plausible idle-state wire (`txFull=0`, `rxEmpty=1`, `busRead=0`, a disabled
peripheral's ack). Decisively, the UART byte stream decoded *directly from the
VCD* matches the asserted test output for every CPU test — RegisterWb
(`RESULT: OK`), AddressableBytes (full transcript ending `RESULT: OK`),
ClockControlWb (structured `nLinks/linkMask/...`), Axi (`…: None … Done`),
CaptureUgn (the u64 counter tuple), DnaPortE2 (the DNA digits), Time (every
subtest `: None`). The recorder is numerically faithful, not merely structurally
present.

**Fidelity follow-up (2026-07-22) — never a false zero; `z` ≠ `x`.** Prompted by
"we don't want to falsely claim values are zero, and we may want to distinguish
not-evaluated from evaluated-undefined". Confirmed the pipeline never emitted a
false `0` (the renderer maps any mask-set bit to `x` regardless of the value
bits), and then made the two undefined-ish states distinct in the VCD: a bit the
design *evaluated to undefined* renders `x`, a cycle that was *never sampled*
(never-forced probe cycle, or history dropped by the trailing window) renders
`z`. Implemented by canonicalising evaluated samples (`packMaskValue` clears
value bits under the mask → undefined is always `(1,0)`), reserving `(1,1)` for
the gap-fill, and rendering `(1,0)→x` / `(1,1)→z`. Defined bits are always kept,
so a partial value is `b0xx…x`, never a fabricated `0`. This directly improves
coverage-gap-3 above: window-dropped history now reads as `z` ("not retained"),
no longer masquerading as evaluated-undefined `x`.

**Coverage gap 1 — RESOLVED via `traceEdgeC` (ad-hoc Circuit tracing).** An
identity `Circuit p p` (`Bittide.CircuitContext.Trace.traceEdgeC`) spliced onto a
protocol edge taps its forward and backward port signals with `traceSignalC`,
giving an empty circuit-notation component a VCD scope `<name>` with `fwd`/`bwd`
wires — WITHOUT threading `HasCircuitContext` into it or needing any `Signal`
binding inside. Applied to both CPU memories in `processingElement`
(`DataMemory`, `InstructionMemory`): every CPU waveform now shows the memory bus
traffic (verified on watchdog — `InstructionMemory.fwd` walks the fetch address,
272 distinct values, zero all-x, and the CPU still executes correctly, so the tap
is semantically identity). Works for any single-channel protocol whose `Fwd`/`Bwd`
are one `BitPack` `Signal` each (Wishbone, `Df`, `CSignal`, Axi4-Stream); a
product edge (`BitboneMm = (ToConstBwd Mm, Wishbone)`) is destructured to its
`Signal` port first. HDL-transparent via `traceSignalC`'s `clashSimulation` gate.
This is the "trace from the outside" idea (roadmap item 4) realised as a
design-side combinator — the general in-plugin version remains the bigger lever.

**Coverage gap 1 (original diagnosis) — empty components are invisible.** `wbStorage`
(the CPU instruction + data memories, two per `processingElement`) is `OPAQUE` +
`HasCircuitContext` and IS wrapped as a `component` by the plugin — yet it
appears in *no* VCD. Root cause (confirmed against the source): VCD `$scope`s are
materialized **solely** from the dotted paths of registered trace/probe leaves
(`Core.hs` `insertVar`/`renderVCDHier`); `component` only pushes a `HierSeg` onto
`ccHier`. `wbStorage`'s body binds only `clash-protocols` port values (Wishbone /
Df / ReqResp), never a `Signal dom a`, and none of its sub-calls (`deviceWbI`,
`addressableBytesWb`, the block-RAM prims) carry `HasCircuitContext`. So its
subtree registers zero leaves and the component leaves no footprint at all — not
pruned, just never emitted. This is the same class as the combinational-function
gap `probeFmap` addressed, but for a whole memory. Consequence: memory traffic is
still observable at the bus level (`rvCircuit.cpuOut`, the interconnect
`toMaster`/`s2m`, the `arbiter`), but the decoded per-memory access view is
absent from every CPU waveform. Fix options for the roadmap: (a) emit empty
component scopes so the hierarchy node at least shows, and/or (b) probe
`wbStorage`'s internal read/write address+data so memory activity is captured.

**Coverage gap 2 — a WHNF consumer captures almost nothing (fixed).** The
single-run live-capture contract is "the waveform covers exactly what the
consumer forces". Watchdog's consumer was `evaluate (L.head (lines s))`, which
forces only WHNF — the first `Char` — advancing the simulation by a single UART
byte; the `assertEqual` that forces the whole line ran *after* `withWaveformLive`
returned and the recording context had frozen. Result: the VCD held one UART byte
(`T`) instead of the asserted `Timeout took 50 microseconds`. Fixed by forcing
the line's full spine (`evaluate (L.length firstLine)`) inside the consumer;
regenerated VCD now records all 29 bytes and the sim runs to cycle ~18.4k (was
~16.8k). Every other test already forces its full output (a parse or a
spine-forcing `length`), so this was the only offender — but it is a sharp edge
of the live-capture design worth a helper or a doc warning: **whatever the
consumer leaves as a thunk is not in the waveform.**

**Coverage gap 3 — the trailing window drops early history (by design).**
`time_self_test` runs ~268k cycles against a 100k window, so its waveform is
missing the early `Start time self test` UART header (the assertion still passes:
it parses the full lazy stream inside the consumer; only the *waveform* is
windowed). Correct behavior for a bounded logic-analyzer window, but a designer
wanting the whole transcript must raise the window (memory ∝ window for
every-cycle signals). Reinforces the streaming-to-disk roadmap item.

## Circuit/circuit-notation ports are first-class (2026-07-23)

The audit's coverage gap 1 (`wbStorage` invisible; protocol wiring binds no
`Signal`s) is now closed at the ROOT, replacing the `traceEdgeC` stopgap
(module deleted, `processingElement` taps removed):

* **Discovery:** circuit-notation desugars every `x <- comp -< a` into an
  ordinary recursive-`let` PatBind of `Tagged port (Fwd/Bwd port)` values
  carrying the port's REAL source span — ccc's renamer was already wrapping
  them; they fell back to identity for lack of `Traceable` instances (and a
  span gate bug: the generated binder NAMES carry no span; `patBinders` now
  falls back to the pattern's span).
* **Fix (ccc):** `Traceable` instances for `Tagged p t` (newtype coercion,
  knot-safe), `()` (no constructor match), tuples 2..12 (generic default,
  `nm._0`/`nm._1`); oracle extension: `KnownNat` arithmetic (built-ins +
  ghc-typelits-extra families) is known iff its operands are — unlocking
  ports inside bus-width-polymorphic components (`WishboneM2S (30 - CLog 2
  (n+1)) 4`).
* **Fix (circuit-notation, local clone `../circuit-notation`, branch
  `trace-ports`, prepared for upstream):** `varP` puts the span on the inner
  name too, and an opt-in `-fplugin-opt=…:trace-ports` re-binds the
  lambda-bound INTERFACE ports through source-located `let` indirections —
  interface ports then trace exactly like `<-` ports. Test-driven by ccc's
  `notation` golden suite against the REAL desugarer (flag on and
  off), including a forward-reference feedback knot (the laziness proof:
  ccc's taps add no strictness — Tagged is a coercion, units aren't matched,
  tuples are demand-equivalent to the let-selector).
* **Measured on watchdog:** 44 → 114 wires (every bus edge Fwd+Bwd at every
  level, per-slave interconnect M2S, `wbStorage_{0,1}` read/write traffic),
  0 all-x, UART decode still byte-exact, sim wall unchanged (~15 s in ghci),
  VCD 3.4 M → 18 M (wide busses × window — the knob is the window size).
  WbToDf: 34 → 92 wires, passes in 5 s. Whole tree builds.
* **Remaining graceful gaps:** composite ports with an untraceable
  component fall back whole (`wb0 :: RegisterWb` carries register metadata;
  a bittide-side `Traceable` identity orphan for `Mm`/metadata types would
  recover them); payloads without `BitPack` (some `reqresp_Fwd` sides);
  interface ports in bittide trace only once the upstream `trace-ports`
  lands and the pin moves (bittide's pinned CN 0.2.0.0 works for `<-` ports
  via ccc's span fallback). Note: circuit-notation gives `_`-ports
  per-occurrence hole semantics, so the "underscore opt-out" for `-<` ports
  exists only for genuinely unused holes — there is currently NO per-port
  opt-out for used ports (a future ccc knob if tap cost ever bites).

## Roadmap (priority order)

1. **`withoutCircuitContext`** — ✅ shipped (F1).
2. **`mooreProbed`** — ✅ shipped (F4); `mealyBProbed`/bundled variants still open.
3. **Exception-tolerant tracing** — ✅ shipped (F8); `sample_lazy` + a shared
   `packMaskValue` so a partial/undefined traced binding shows `x` instead of
   crashing the dump.
3a. **Closed-binding skip** — ✅ shipped (F9); `closedBind` in the renamer keeps
   GHC's generalization of closed polymorphic locals intact by not wrapping
   them; unblocked `timeWb`/`clockControlWb` and the repo-wide propagation.
4. **Non-viral tracing opt-in** — NEW, from `bittide-instances`: a way to enable
   tracing of a component "from the outside" (at the instantiation site) without
   threading `HasCircuitContext` through its signature. Would make shared-core
   peripherals (things inside `processingElement`) traceable without repo-wide
   `withoutCircuitContext` churn. Biggest open lever.
5. **Plugin warning for `HasCircuitContext` without `OPAQUE`** (F6) — ✅
   shipped, as the opt-in `diagnostics` option, together with constraint-synonym
   recognition and renamer idempotence.
6. **Probe-mode: stop descending into multi-arg local helpers** (F5) — removes a
   correctness+perf footgun.
7. **`Clash.CircuitContext.Waveform` test helpers** (F7) — ✅ shipped, with
   `…Waveform.Hedgehog` for the property-monad half (F11). The duplicate
   `Tests.Waveform` in both bittide suites is gone.
8. **README "gotchas"** — lexical context resolution (F3), opt-in model, viral
   constraints and how `withoutCircuitContext` cuts them.
9. **Empty-component visibility** (audit gap 1) — ✅ RESOLVED 2026-07-23 at
   the root: circuit-notation port binders now auto-trace (Tagged/unit/tuple
   `Traceable` instances + patBinders span fallback + oracle KnownNat
   arithmetic), so protocol-wiring components like `wbStorage` register
   leaves and materialize their scopes. Follow-ups: identity `Traceable`
   orphans for memory-map metadata types (recovers composite `RegisterWb`
   ports), upstream the circuit-notation `trace-ports` patch (interface
   ports).
10. **`withWaveformLive` force helper / warning** (audit gap 2) — the consumer
   bounds what is recorded, and a WHNF-only consumer (`evaluate (head …)`)
   silently captures almost nothing. Ship an NFData-forcing helper
   (`consumeNF`) or document the "force what you want to see" contract loudly;
   Watchdog hit exactly this and recorded one UART byte.
