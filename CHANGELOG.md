# Changelog

## 0.1.0.0 — unreleased

Initial version.

* Runtime (`Clash.CircuitContext.Core`): scoped simulation context on
  unmodified clash-prelude — `component` hierarchy, per-simulation trace
  maps (`traceSignalC`), probes inside mealy/moore step functions
  (`mealyProbed`/`mooreProbed`/`probe`), hierarchical VCD dumping
  (`dumpVCDC`).
  Instance identity is heap identity (StableName), resolved to
  design-deterministic `_0`/`_1` sibling names ordered by instantiation
  call site; `imapComponents` names replicated instances by structural
  position.
* GHC plugin (`Clash.CircuitContext.Plugin`): automatic instrumentation.
  `OPAQUE` + `HasCircuitContext` toplevel functions become components
  (guarded equations are case-encoded when they cannot fall through);
  named local bindings in `HasCircuitContext` functions are auto-traced,
  in `HasProbe` functions auto-probed; `_`-prefix opts a binding out.
  Local **pattern** bindings (tuples, records, …) are handled too: each
  binder is traced via an injected sibling binding. Traceability is
  decided by a typechecker-plugin oracle (`CanTrace`/`CanProbe`), so
  untraceable types fall back to identity without errors.
* Plugin options (`-fplugin-opt=Clash.CircuitContext.Plugin:<opt>`).
  `diagnostics` reports, on stderr, every decision that silently costs a
  wire or a scope: bindings whose payload type the oracle declined (with
  the requirement it got stuck on), `HasCircuitContext` without `OPAQUE`,
  `OPAQUE` without a type signature, and signatures carrying both
  `HasProbe` and `HasCircuitContext`. Off by default — the silent fallback
  is what makes package-wide enablement safe. An unrecognised option is
  reported rather than ignored.
* The renamer half sees through **constraint synonyms**: a signature
  written against `type Ctx dom = (HiddenClockResetEnable dom,
  HasCircuitContext)` now makes its `OPAQUE` function a component, whether
  the synonym is declared in the module or imported. It used to keep
  tracing but silently lose its `$scope`, because the renamer runs before
  synonyms are expanded.
* The renamer half is **idempotent**. Enabling the plugin twice
  (package-wide plus an `OPTIONS_GHC` pragma) used to nest every component
  wrap in itself (`switch.switch`); it is now a no-op, and a hand-written
  `component "f"` on a binder the plugin would wrap is left alone rather
  than doubled.
* Probes carry the ADT description of their payload type too, so an FSM
  state — which is state, never a wire, and therefore visible only as a
  probe — renders as constructors rather than as raw bits. Decided
  separately from probeability (`CanDescribe`/`Describable` beside
  `CanProbe`/`Probeable`), so a payload `Waveform` cannot describe still
  probes, as before: `probe` is unchanged, `probeSW` is the describing
  variant, and the plugin picks per type.
* Fixed: the ADT sidecar keyed its descriptors off the traces alone, while
  the VCD names come from the union of traces and probes. Sibling numbering
  is a function of the whole key set, so wherever a probe shared a name with
  a trace the sidecar described a path the VCD did not declare (and left the
  one it did undescribed).
* `Traceable` derives generically for records of traceable parts (signal
  bundles): derive `Generic` and write an empty instance. Fields trace as
  `name.field` sub-scopes in the VCD hierarchy; nested records nest. The
  umbrella module re-exports `Traceable`.
* Tracing a `Signal dom a` requires `(KnownDomain dom, BitPack a,
  NFDataX a)` — no `Typeable`. Registration stores an empty `TypeRep`
  encoding (unused by `dumpVCDC`), so size-polymorphic payloads
  (`Unsigned n`, `BitVector n`) trace inside polymorphic components.
* Supported GHCs: 9.6, 9.10; clash-prelude `>=1.9 && <1.12` (the trace
  internals used are identical across these versions).
* Instrumentation is transparent to HDL generation. All recording
  combinators are gated on `Clash.Magic.clashSimulation`, so Clash reduces
  them to their plain forms and eliminates the tracing machinery:
  `traceSignalC`/`probe`/`component` become identity and `mealyProbed`
  becomes a plain `mealy` (no companion counter, hence no extra hardware).
  An instrumented design synthesizes to the same netlist as the
  un-instrumented one; discharge `HasCircuitContext` at a boundary with
  `withoutCircuitContext (f a b c)` (or the raw `noCircuitContext`).
  `withoutCircuitContext :: (HasCircuitContext => r) -> r` lets a
  synthesis-facing caller invoke an instrumented function without acquiring
  the (viral) constraint, replacing hand-written
  `fooNC = let ?circuitContext = noCircuitContext in foo` shims that had to
  duplicate the callee's whole type signature.
* Simulation is cheap when not recording: under `noCircuitContext` the
  combinators short-circuit before any `IORef`/hierarchy/counter work.
* `dumpVCDC` returns `Left` (never throws) when nothing was traced or
  probed — e.g. a run of zero cycles, or one where no traced signal was
  forced. Previously it propagated stock `dumpVCD1#`'s `error "no traces
  found"`, which could crash an otherwise-passing simulation that had
  instrumentation left in.
* Tracing is robust to partial and undefined signals. Auto-instrumentation
  traces every named local binding, including ones the design only evaluates
  under a condition (e.g. `fromJust`/`head` on a signal that is `Nothing`/empty
  until a link comes up). Sampling such a binding every cycle previously
  crashed the whole VCD dump with the design's own `error` (a plain
  `ErrorCall`, which the old `isX`-based probe guard did not catch, and which
  the trace path did not guard at all — it also eagerly forced samples via
  `sample`). Now trace samples are taken with `sample_lazy` and every
  sample — trace or probe — is forced through `packMaskValue`, which maps ANY
  exception (`XException` undefined bits, or an `ErrorCall` from a partial
  binding) to an all-undefined entry rendered as `x`. A tracer never decides
  whether the simulation crashes; a signal the design leaves undefined at a
  cycle simply shows `x` in the waveform.
* Live, single-run capture support: `withCircuitContextWindow` bounds
  recording to a trailing capture window (logic-analyzer style — full-history
  recording of a program counter is intrinsically unbounded; RLE cannot
  compress a value that changes every cycle), implemented as a two-chunk gap
  buffer with O(1) amortized pruning; window-dropped history renders `x`.
  `recordedCycles` reports how far a simulation actually ran, so a dump after
  an early-exiting consumer (an assertion that found its answer) covers
  exactly the simulated cycles instead of draining a fixed window. Together
  these enable the intended usage: ONE lazy simulation shared by assertion
  and waveform, instead of a second strict full-window re-run per test —
  measured on a real CPU-DUT suite: 155 s → 3.5 s and 654 s → 22 s per test,
  with recorder memory below the simulation's own footprint.
* Probe recording no longer re-derives its identity every cycle. `simProbe`
  used to resolve the hierarchy (StableName per segment), build the
  qualified-name string, and insert into a global string-keyed map — per
  probe, per cycle: ruinous at 10⁶ cycles. Probes now resolve once (a
  per-instance cache carried in the probe context) and a cycle costs one
  small-map lookup plus one `IORef` update. The cache allocation is tied to
  the instance's identity object — a closed `unsafePerformIO (newIORef …)`
  gets floated to the top level and silently shared across instances
  (caught by the golden tests as merged probes).
* The trace tap records cycle /i/ when cell /i+1/ is forced, never while
  producing cell /i/. Packing a value mid-production deadlocks on
  combinational feedback: in a value-level knot (clash-protocols ties
  @m2s@/@s2m@ through Circuit fixpoints), fully evaluating value /i/ can
  demand — through the other half of the knot — spine cell /i/ of the same
  signal, the very cell under construction: a blackhole re-entry that shows
  as `<<loop>>` single-threaded and as a silent hang under the threaded RTS
  (a test suite that "does not terminate"). Worse, when the re-entry happened
  at value level it was caught by the tracer's exception guard and silently
  recorded the rest of the signal as `x` — corrupt waveforms that also
  compressed deceptively well. Deferring by one cell is safe by causality: a
  value can never demand its own future spine, and by tail-force time no
  evaluation is suspended inside the handed-over cell. All recorded values
  verified real (no all-x signals) on the instrumented CPU DUTs after the
  change.
* The recorder no longer leaks space — by design, not by strictness patches.
  Recording a real design (dozens of signals, 10⁵–10⁶ cycle windows) used to
  grow memory linearly with simulation length twice over: traces stored a lazy
  `map packMaskValue (sample_lazy sig)` whose retained HEAD pinned every
  cycle's raw design value while the simulation advanced the shared signal
  (measured: 1.2 GB live / 3.5 GB heap for one 100 k-cycle CPU-DUT capture),
  and probes stored one map entry per cycle (initially behind a lazy
  `IntMap.union` thunk chain that also pinned design values). Both stores are
  now RUN-LENGTH ENCODED (`Runs`; memory ∝ value changes, the same
  compression a VCD applies on output) and recorded in LOCKSTEP with the
  simulation: `traceSignalC` returns a tapped signal that packs each cycle's
  value as the design forces it and keeps only the un-consumed packed tail,
  so raw history is released immediately; probes append strictly to their run
  history. `withCircuitContext` now returns this compressed `TraceData`
  (replacing the stored-lazy-list `TraceMap`) and `ProbeMap` holds `Runs`;
  `dumpVCDC` expands runs lazily into the dense form the VCD builder needs —
  cycles the design never forced are still drained on demand and probe gaps
  still render `x`, so dumps are bit-identical (goldens unchanged). Same
  100 k-cycle capture after: 295 MB live — below the simulation's own
  no-recording footprint.
* New probe combinators for the two Clash idioms `mealyProbed` could not reach:
  `mealyBProbed`/`mooreBProbed` (bundled-I/O mealy/moore — the `mealyB`/`mooreB`
  form, the most common in real designs) and `probeFmap`, an `fmap` that binds a
  per-cycle `?probe` so the internals of a COMBINATIONAL function applied over
  signals (`route <$> masterS <*> slavesS`) are probed — the named `where`
  bindings inside such a function are neither `Signal`s (so `traceSignalC`
  cannot reach them) nor inside a mealy step (so `probe` had no cycle context).
  All three reduce to their plain forms during HDL generation and when not
  recording. Covered by the auto-smoke design (`probeFmapDemo`, `mealyBDemo`).
* The waveform now distinguishes NOT-EVALUATED from EVALUATED-UNDEFINED, and
  never renders a false `0` for either. A recorded sample carries a per-bit
  `(mask, value)`: a defined bit renders `0`/`1`; a bit the design evaluated to
  undefined renders `x`. `packMaskValue` now CANONICALISES an evaluated sample
  (clears every value bit the mask marks undefined) so an undefined bit is
  always `(mask=1, value=0)` — Clash's `pack` does not promise a particular
  value under the mask, and without this a masked value bit left set would
  misrender. That frees the `(mask=1, value=1)` encoding, which the run-length
  gap-fill (`expandRunsX`) now uses for a cycle that was NEVER sampled — a
  never-forced probe cycle, or history dropped by a trailing capture window —
  rendered `z`. So `x` means "we looked and it was undefined" and `z` means "we
  never looked"; defined bits are always preserved, so a partially-defined value
  shows e.g. `b0xx…x`, never a fabricated `0`. The hierarchical renderer owns
  the value-change emission (`renderVC`), so this needed no change to the stock
  `dumpVCD1#`. Auto-smoke goldens updated; `check.sh` now asserts both invariants
  (a `z` gap and a `b…x` partial-undefined must appear).
* `Circuit`\/circuit-notation designs are now first-class: every `-<` port
  binder in a `circuit` block auto-traces, without call-site changes. The
  notation's parse-stage plugin desugars `x <- comp -< a` into ordinary
  pattern bindings of `Tagged port (Fwd port)`\/`Tagged port (Bwd port)`
  values that the renamer half already wraps; three new `Traceable` instance
  groups make them trace: `Tagged p t` (newtype delegation — a coercion, so
  no strictness is added to the notation's lazy forward-reference knot),
  `()` (identity WITHOUT matching the constructor, for the same reason), and
  2..12-tuples via the generic default (positional `nm._0`\/`nm._1`
  sub-scopes for composite protocol ports). Wires keep the generated
  `<port>_Fwd`\/`<port>_Bwd` names. New dep: `tagged` (transitively pinned by
  clash-prelude already). Supporting fixes:
  - `patBinders` falls back to the enclosing PATTERN's source span when a
    binder name carries none: circuit-notation locates generated patterns
    but not the names inside them, and the good-span gate (which skips
    compiler-generated bindings) was silently dropping every port binder.
  - The oracle now decides `KnownNat` ARITHMETIC: an application of a
    type-level arithmetic family (GHC's built-ins + ghc-typelits-extra's) is
    known iff its operands are — mirroring what ghc-typelits-knownnat/-extra
    solve in the instrumented module. Without this, ports inside
    bus-width-polymorphic components (`Signal dom (WishboneM2S (30 - CLog 2
    (n + 1)) 4)`) silently fell back.
  Limitations (all graceful — a port simply doesn't trace): a composite port
  with one untraceable component (e.g. compile-time memory-map metadata)
  falls back whole; multicast `Bwd` sides bind wildcards; the circuit's
  lambda-bound INTERFACE ports need circuit-notation's new opt-in
  `trace-ports` mode (patch on the local `circuit-notation` checkout's
  `trace-ports` branch, prepared for upstreaming: the lambda binds fresh
  names and each interface port half is re-bound through a source-located
  `let` indirection). Covered by the `notation-smoke` golden suite, which
  runs the REAL circuit-notation desugarer (with and without `trace-ports`)
  against this plugin — a register-feedback circuit with a forward
  reference is the laziness proof. Dogfooded on bittide CPU DUTs: the
  watchdog waveform went from 44 to 114 wires (memory busses, interconnect
  per-slave M2S, `wbStorage` read/write traffic) with zero all-x signals
  and unchanged simulation cost; VCD size grows with the extra wide bus
  wires (bounded by the capture window).
* The plugin no longer auto-traces local bindings with a CLOSED right-hand
  side (no free local variables besides the binder itself; decided from the
  renamer's own free-variable sets). Closed bindings are the ones GHC
  generalizes even under `MonoLocalBinds` — polymorphic helpers like
  `noWrite = pure Nothing` used at several types. Wrapping such a binding in
  `autoTrace` injected an `AutoTrace (CanTrace t) t` wanted whose type-family
  application is not quantifiable in an inferred type, so GHC monomorphized
  the binder at its first use and the second use became a baffling type error
  (found on bittide's `timeWb`/`clockControlWb`, misdiagnosed at first as a
  Protocols-DSL conflict). Closed bindings are constants by construction, so
  the lost trace is a flat line; a genuinely monomorphic closed binding (a
  self-contained `cnt = register 0 (cnt + 1)`) can still be traced explicitly
  with `traceSignalC`. Regression-tested in the auto-smoke design
  (`polyHelper`).
* Code-quality alignment with `clash-lang/clash-shockwaves`: clash-lang-style
  module headers (copyright/license/maintainer) on every module; a shared
  `common` stanza with `-Wall -Wcompat -haddock`; the library builds with
  `-fexpose-all-unfoldings -fno-worker-wrapper` (Clash normalizes through the
  runtime combinators in user designs; OPAQUE workers are exempt by
  definition, preserving instance identity); `fourmolu` formatting
  (`fourmolu.yaml`, `format.sh`, CPP-in-declaration regions kept verbatim via
  `FOURMOLU_DISABLE`); `packMaskValue` uses `unsafeDupablePerformIO` (the
  guard is pure, so duplicate-suppression bought nothing per sample); the
  tree is `-Wall -Wcompat`-clean on GHC 9.6.7 and 9.10.3.
* Waveforms from a test suite: `Clash.CircuitContext.Waveform` ships the
  lifecycle every instrumented project otherwise re-derives — a `WaveformSlot`
  owning one test's pending waveform, `withWaveform`/`withWaveformLazy` by how
  the design is sampled, and atomic writes. Deliberately not a global
  registry, which can only be drained once every test has finished and so must
  retain every rendered VCD until then (measured: 26 GB resident across a
  suite).
* Capture only what will be kept. `withWaveformWhen` (and
  `waveformsRequested`, reading `CCC_WAVEFORMS`) decide BEFORE simulating: a
  run that will not be kept executes under `noCircuitContext`, where nothing
  registers, accumulates, renders or is written. `withWaveformOnFailure` runs
  with recording off and re-runs only if the consumer throws;
  `withWaveformOnFailure'` records as it goes for a design whose re-run may
  not reproduce, and still renders only on failure. Under a parallel runner
  peak memory is the sum over concurrent tests, so this is the dominant cost:
  measured on a 24-core suite, 25.2 GB and 6m22s became 8.2 GB and 1m06s,
  with no waveform at all on a green run.
* `Clash.CircuitContext.Waveform.Hedgehog` for properties. A hedgehog failure
  is a value in `PropertyT`'s error layer, not a thrown exception, so no `try`
  in IO can observe one — a property would fail, print a counterexample and
  leave no waveform. `withWaveformCase` runs the property monad itself, re-runs
  the failing case with recording on, and re-raises the failure unchanged, so
  what survives is the waveform of the SHRUNK counterexample (each failing
  case overwrites the slot as hedgehog shrinks). `recordThisCase` /
  `recordCaseOfSize` / `recordLargestCase` choose which passing case to keep,
  deciding inside a generator — the only place hedgehog exposes `Size`.
* `withCircuitContextWindowE` returns what was recorded even when the action
  threw (the plain version reads the recorder refs after the action, so an
  exception escaped with the recording); `withCircuitContextWindowM` runs the
  action in any monad over IO, for monads that report failure as a value.
* Signals with an identical recorded history now share one VCD identifier,
  with the copies re-attached as extra `$var` declarations. Cuts VCD size
  roughly 3x on real designs, where a bus and its aliases carry the same bits.
* A VCD scope is a COMPONENT. Generically-derived composite traces used to
  name fields `binder.field`, and the renderer splits every `.` into a scope,
  so data structure was indistinguishable from design hierarchy — 850 of 1064
  scopes across a real suite's waveforms were structural. Fields are now
  siblings (`binder_field`, `binder_0`).
* The plugin never traces a binder a code generator invented. Names
  containing `:` are skipped, which is unforgeable because no source-language
  variable identifier can contain one; `circuit-notation`'s `trace-ports`
  patch marks its synthesised binders accordingly. Removed 979 of 4823
  declarations (20%) across a real suite, each a duplicate of a signal
  already present under its own name.
* `recordedCycles` no longer stops one cycle short. A recorder commits cycle
  `i` only when cell `i+1` is forced — the one-cell delay `registerTrace`
  takes deliberately, because packing a value while producing its own cell
  blackholes on a combinational knot — so the last forced cycle was never in
  the runs; it sits packed in the continuation, which the dump drains. Every
  lazily-captured waveform was therefore one cycle short, which is invisible
  on a 600k-cycle trace and fatal on a 3-cycle counterexample.
* The test suite is structured in two levels, both run by `check.sh`.
  `tests/Example/` is the runnable worked example of all of the above —
  `Example.SingleRun` captures one run's waveform in the unit-test shapes,
  `Example.Hedgehog` instruments hedgehog properties the way a downstream
  suite would — so the documentation cannot drift from the API.
  `tests/Test/` pins features: recorder behaviour under generated stimuli
  (`Test.Recorder`), the capture-cost contract (`Test.Capture`), the plugin
  and oracle golden suites, and `Test.ExampleOutput`, which tasty sequences
  after the Example level to decode the waveforms it wrote and verify they
  show the runs they claim to.
