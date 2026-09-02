> ### You are on `dogfood/shockwaves`
>
> This branch is the plugin — everything below — **plus six vendored
> checkouts**: everything `dogfood/deps` has, and
> [`clash-shockwaves`](https://github.com/clash-lang/clash-shockwaves) on top.
>
> The two halves of a good waveform come from different places, and this branch
> is the demonstration that one simulation can produce both: **hierarchy and
> automatic tracing** from this plugin, and the **ADT structure** — constructors,
> fields, bit ranges — that makes those bits readable, from shockwaves. A
> `$var wire 34` becomes a decoded `WishboneM2S` in a typed viewer, and neither
> library had to learn the other's job. Probes are described too, wherever the
> probed type allows it (`probeSW`, and the `CanDescribe` half of the oracle) —
> an FSM phase is state, never a wire, so a probe is the only way it is seen at
> all, and it is the signal whose constructors a reader most wants named.
>
> Read **[`deps/README.md`](deps/README.md)** for what that costs, what it
> buys, and the version chain between the sidecar format and the Surfer plugin
> that reads it.

# clash-circuit-context

Scoped simulation tracing for [Clash](https://clash-lang.org/) designs: a
small runtime plus a GHC plugin that instruments your design automatically.
Simulate as usual, get a **hierarchical VCD** of every named signal in your
design — on **unmodified `clash-prelude`**, with no changes to how you
simulate.

Instrumentation is two annotations per function:

* an `{-# OPAQUE f #-}` pragma, and
* a `HasCircuitContext` constraint.

The plugin then wraps the function in a named *component* (a hierarchy
level), auto-traces every named local binding of traceable type under its
binder name, and auto-probes bindings inside `HasProbe` functions (e.g.
mealy step functions). Remove the plugin (or use `noCircuitContext`) and
every combinator collapses to identity.

## Adding it to your project

Not on Hackage yet, so pin it from git. In your `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/lmbollen/clash-circuit-context.git
  tag: <commit-sha>   -- a branch name works, but pins nothing

-- clash-prelude 1.11 is not on Hackage yet either; this package is developed
-- against the commit below. Drop this stanza if you are on a released 1.9/1.10.
source-repository-package
  type: git
  location: https://github.com/clash-lang/clash-compiler.git
  tag: e16599387a68d6361134b4fe7e63bf4ad3ae8408
  subdir: clash-prelude
```

In the `.cabal` file of the package whose designs you want traced:

```cabal
build-depends: clash-circuit-context
ghc-options:   -fplugin=Clash.CircuitContext.Plugin
```

Enabling the plugin package-wide is safe and is the recommended default: it
only touches top-level binders whose *written signature* carries
`HasCircuitContext` or `HasProbe`, so every other module compiles to exactly
the same code. Measured on a real repository, flipping 13 per-module pragmas
to one package-wide flag left seven waveforms bit-for-bit identical. You can
still enable it per module with
`{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}` — but do not do
both, or the plugin runs twice.

Requires GHC 9.6 or 9.10 and `clash-prelude >= 1.9 && < 1.12`. Nothing else,
and in particular no patched `clash-prelude`.

Then instrument a function with the two annotations above, and see
[Waveforms from your test suite](#waveforms-from-your-test-suite) for getting
a VCD out of a test.

## Example

```haskell
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}
module My.Design where

import Clash.Explicit.Prelude
import Clash.CircuitContext

acc :: HasCircuitContext => Signal System Int -> Signal System Int
acc inp = total
 where
  total = mealyProbed clockGen resetGen enableGen step 0 inp
  step :: HasProbe => Int -> Int -> (Int, Int)
  step s i = (next, s)
   where
    next = s + i
{-# OPAQUE acc #-}

top :: HasCircuitContext => Signal System Int -> Signal System Int
top inp = out
 where
  out = acc inp + acc (inp + 1)
{-# OPAQUE top #-}
```

No `traceSignal`, no manual names — the code reads like ordinary Clash.
Run a simulation under `withCircuitContext` and dump:

```haskell
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  (_samples, traces, probes) <- withCircuitContext $ do
    let xs = sampleN 100 (top (fromList [1, 1 ..]))
    _ <- evaluate (deepseqX xs xs)
    pure xs
  vcd <- dumpVCDC (0, 100) traces probes
  either fail (TIO.writeFile "trace.vcd") vcd
```

The VCD scope tree mirrors the design hierarchy; the two `acc` instances
are disambiguated deterministically by instantiation call site:

```
top
├── out
├── acc_0
│   ├── total
│   └── next, prev   (per-cycle probes from the step function)
└── acc_1
    ├── total
    └── next, prev
```

## What the plugin does

Enable it per module (`{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}`)
or project-wide (`ghc-options: -fplugin=Clash.CircuitContext.Plugin`). Doing
both is harmless: the renamer pass then runs twice, and it is idempotent.

1. **Component wrap** — a toplevel function with an `OPAQUE` pragma *and* a
   written `HasCircuitContext` constraint has its body wrapped in
   `component "<functionName>"`. Its `where`-bindings are moved into a
   `let` under the wrap (required: `where`-bindings would otherwise be
   elaborated against the *caller's* context and escape the new scope).
2. **Auto-trace** — inside any function whose signature carries
   `HasCircuitContext` — the synonym, a raw `?circuitContext`, or your own
   constraint synonym that expands to either (`type Ctx dom =
   (HiddenClockResetEnable dom, HasCircuitContext)` works, declared in the
   module or imported) — every named traceable binding becomes
   `autoTrace "<binder>" …`. This covers
   zero-argument `let`/`where` bindings *and* the binders of local
   **pattern** bindings — `(a, b) = unbundle …`, `Out{x, y} = f …` — the
   usual Clash idiom for multi-output circuits: each pattern binder `x` is
   traced via an injected `x = autoTrace "x" x'` sibling, so the rest of
   the code is untouched. Untraceable types (no `BitPack`, unknown domain,
   missing evidence for a polymorphic type) fall back to identity — by
   design, decided by a typechecker-plugin oracle (`CanTrace`), so
   instrumented code never fails to compile because something cannot be
   traced. Every such fallback is reported as a warning; see
   [When something isn't traced](#when-something-isnt-traced).
3. **Auto-probe** — same rule for `HasProbe` signatures: bindings become
   `autoProbe "<binder>" …`, recording one value per simulated cycle from
   inside e.g. a mealy step function.
4. **Innermost signature wins** — a local signature switches the mode for
   its subtree, so a `HasProbe` step function nested in a
   `HasCircuitContext` component probes rather than traces.
5. **Guarded equations** — guarded bodies are case-encoded inside the
   component wrap whenever their guards cannot fall through to a later
   equation (a final `otherwise`/unguarded alternative, or the last
   equation of the function). A *non-final* equation whose guards can fall
   through is skipped with a compile-time warning, because wrapping it
   would turn the fall-through into a crash.

### Semantics worth knowing

* A `HasCircuitContext` function *without* an `OPAQUE` pragma is not a
  component: its auto-traced bindings register under the **caller's**
  scope, because the implicit-parameter dictionary flows from the call
  site. This is often exactly what you want for small helpers — which is
  why it is a warning you can turn off
  (`-Wno-x-circuit-context-uninstrumented`) rather than an error.
* Traceable today: `Signal dom a` with `(KnownDomain dom, BitPack a,
  NFDataX a)`, and `Vec n` thereof (traced element-wise as `name_0`,
  `name_1`, …). No `Typeable` is required — that field of a trace only
  feeds clash's replay machinery, which these maps don't use, and
  requiring it would exclude size-polymorphic payloads (`Unsigned n` under
  a `KnownNat n` given) from tracing inside polymorphic components. Extend
  by adding `Traceable` instances — and for records of traceable parts
  (signal bundles), derive `Generic` and write an **empty instance**:

  ```haskell
  data Bus dom = Bus
    { busAddr :: Signal dom (Unsigned 8)
    , busStrobe :: Signal dom Bool
    } deriving (Generic)

  instance KnownDomain dom => Traceable (Bus dom)
  ```

  A binding of this type auto-traces field-wise, the record becoming a
  sub-scope in the VCD (`bus.busAddr`, `bus.busStrobe`); nested records
  nest. Fields are tolerated **individually**: an untraceable field falls
  back to identity on its own and its traceable siblings still record, so a
  composite is never all-or-nothing. That matters most for protocol ports,
  whose `Fwd`/`Bwd` halves routinely mix signals with non-signal payloads —
  a memory-map-carrying bus port has
  `Bwd (mm, wb) = (MemoryMap, Signal dom WishboneS2M)`, and demanding
  `Traceable` of both would drop the bus response too. Keep such instance
  contexts to ordinary class constraints, as above — the oracle recognizes
  an instance by recursing on its written context.
* Multiple instances of the same component under one parent are
  disambiguated **design-deterministically**: siblings are ordered by
  instantiation call site (never by evaluation order) and named
  `name_0`, `name_1`, …. Instances born at the *same* call site (e.g.
  `fmap acc`-style replication) cannot be design-ordered — name those by
  structural position with `imapComponents`.

### When something isn't traced

The fallback to identity is what makes project-wide enablement safe, and it
is also what makes a missing wire look exactly like a wire nobody asked for.
So the fallback stays and the silence does not: every decision that costs a
wire or a scope is a **real GHC warning**, in a custom category you tune with
the flags you already use.

```
tests/Test/PluginDiagnostics.hs:114:3: warning: [-Wx-circuit-context-undecided]
    not traced: Signal dom (Awkward n)
    the oracle could not decide: 1 <= n
    This may be a limit of the approximation rather than a fact about the type.
    Name the binding with traceSignalC, which takes Traceable as a real
    constraint, to find out which.
    |
114 |   held = inp
    |   ^^^^^^^^^^
```

Four categories, because they differ in how likely each is to be a defect:

| Flag | Fires when |
| --- | --- |
| `-Wx-circuit-context` | The plugin could not honour something the source unambiguously asked for. Nothing benign lands here. |
| `-Wx-circuit-context-uninstrumented` | A signature asks for instrumentation somewhere the pass does not reach or does not scope: `HasCircuitContext` without `OPAQUE`, `OPAQUE` without a signature, both constraints at once, a class or instance method body. |
| `-Wx-circuit-context-undecided` | The oracle could not decide, and fell back to not tracing. **The one that matters**: this is the approximation admitting it may be wrong, not a fact about your types. |
| `-Wx-circuit-context-untraced` | The oracle found no instance. A real answer, and the verbose one — it fires for every `Int` and `Bool` that was never going to trace. |

Which means "strict mode" is not a plugin option, it is `-Werror`:

```
-Wno-x-circuit-context-untraced     -- I know, stop telling me
-Werror=x-circuit-context-undecided -- a guess must not cost me a wire
-Werror=x-circuit-context-untraced  -- a curated design: any lost wire fails CI
```

A blanket `-Werror` promotes these like any other warning — GHC's behaviour,
not this plugin's, but worth knowing before a pin bump: a project already
building with `-Werror` sees previously silent fallbacks become build
failures on the first build after enabling them.

For the one shape that is *deliberate* rather than a near-miss — a test
harness or simulation driver that carries `HasCircuitContext` with no
`OPAQUE`, so the design tree roots at the waveform top instead of under a
driver's scope — say so on the binder instead of silencing the category:

```haskell
{-# ANN runSystem NoCircuitScope #-}
runSystem :: HasCircuitContext => …   -- deliberately no OPAQUE
```

That keeps `-Wx-circuit-context-uninstrumented` live, and promotable to
`-Werror`, for every binder you did *not* vouch for. A mistyped constructor
is a compile error, which is the point: a marker that silently failed to
register would reintroduce the silence these warnings exist to remove.

Nothing here ever fails a build on its own, so promoting is a decision you
make per project. The fallback itself is unchanged either way: instrumented
code never fails to compile because something cannot be traced, and
`traceSignalC` remains the way to demand tracing at compile time, since it
takes `Traceable` as a real constraint.

Custom warning categories are a GHC 9.8 feature. On 9.6 these are still real
warnings — a blanket `-Werror` promotes them — but they cannot be silenced or
promoted individually.

### Opting out

Instrumentation is opt-in **by signature, not by module**, so enabling the
plugin project-wide is safe — a module with no `HasCircuitContext`/`HasProbe`
signature is untouched. To opt back out, at four granularities:

| Granularity | How |
| --- | --- |
| One binding | Prefix the binder with `_`: `_hidden = acc inp` is skipped. Works for `circuit` ports too (`_dbg`). |
| One function | Omit the `OPAQUE` pragma (no component scope) or the constraint (no instrumentation at all). |
| One module | Don't enable the plugin on it. |
| A call, and everything under it | `withoutCircuitContext` — discharges the constraint without duplicating the callee's signature just to drop it. |

```haskell
-- A synthesis entry point calling an instrumented circuit:
topEntity = withoutCircuitContext (myInstrumentedCircuit clk rst)
```

`withoutCircuitContext` is exactly `let ?circuitContext = noCircuitContext in …`.
Because instrumentation is HDL-transparent, the wrapped call is identical — in
simulation *and* synthesis — to calling an uninstrumented function.

What is *not* cheap is the opposite direction. `HasCircuitContext` propagates to
callers, so instrumenting a widely-shared component means either threading the
constraint repo-wide or writing `withoutCircuitContext` at every boundary — in
the bittide dogfooding, 9 edits for one peripheral (`timeWb`: 7 synthesis call
sites plus 2 library intermediaries). Opting out is cheap; opting *in* spreads.
A non-viral opt-in — tracing a component "from the outside" — is the main open
design question, tracked as finding F1 in
[`docs/dogfooding-bittide.md`](docs/dogfooding-bittide.md).

## Waveforms from your test suite

`withCircuitContext` + `dumpVCDC` is the raw API. In a real test suite you
also want to decide *which* run survives, write the file atomically, and —
above all — not pay for waveforms nobody will look at.
`Clash.CircuitContext.Waveform` is that lifecycle, so a project does not have
to grow its own (two suites in the proof-of-concept repository had):

```haskell
import Clash.CircuitContext.Waveform

case_myDesign :: Assertion
case_myDesign = withWaveformSlot "my_design" $ \wf -> do
  out <- withWaveform wf 1000 (sampleN 1000 (top (fromList [1 ..])))
  expected @=? out
```

That writes `waveforms/my_design.vcd`. A *slot* is one test's pending
waveform, created and owned by that test — deliberately not a global registry,
which would have to retain every rendered VCD until the whole suite finished.

### Capture only what you will keep

Recording is not free, and under a parallel runner peak memory is the sum over
concurrently running tests. So decide *before* simulating rather than
discarding afterwards — a run that will not be kept executes under
`noCircuitContext`, where `traceSignalC` is identity and nothing is recorded,
rendered or written:

```haskell
-- Only when asked for (CCC_WAVEFORMS=1), e.g. to regenerate documentation.
keep <- waveformsRequested
out  <- withWaveformWhen keep wf 1000 (sampleN 1000 (top inp))

-- Or: nothing while the test passes, the failing run's waveform when it does
-- not. `consume` runs twice, so it must be assertions and forcing, not
-- one-shot IO.
withWaveformOnFailure wf 1000 (sampleN 1000 (top inp)) $ \out ->
  expected @=? out
```

On a real 24-core suite that took unconditional recording from 25.2 GB and
6m22s to **8.2 GB and 1m06s**, with no waveform at all on a green run and the
failing test's waveform when one goes red. `withWaveformOnFailure'` covers a
design whose re-run may not reproduce (a CPU model resolving undefined inputs
randomly): it records as it goes but still renders only on failure.

For a design whose consumer decides how far to simulate — firmware reading a
UART stream until it prints its result — use `withWaveformLazy`, which
captures exactly the cycles the consumer forced instead of a fixed window.

### Hedgehog properties

A hedgehog failure is a value in `PropertyT`'s error layer, not a thrown
exception, so no `try` in IO can see one — a property fails, prints a perfect
counterexample, and leaves no waveform.
`Clash.CircuitContext.Waveform.Hedgehog` closes that:

```haskell
import Clash.CircuitContext.Waveform.Hedgehog

prop_roundtrip :: IORef Bool -> WaveformSlot -> Property
prop_roundtrip fired wf = property $ do
  input <- forAll genInput
  keep  <- recordLargestCase fired          -- which PASSING case to keep
  withWaveformCase keep wf nCycles (sampleN nCycles (dut input)) $ \out ->
    out === model input
```

If the property fails, the waveform you get is of the **shrunk
counterexample** — shrinking re-runs the property on smaller inputs and each
failing case overwrites the slot, so what survives is the minimal case
hedgehog reports, and its absolute path is printed in the failure report.
If it passes, `recordLargestCase` keeps the biggest case and fires exactly
once however often shrinking re-runs the generators; `recordCaseOfSize` picks
a smaller, readable one instead (hedgehog's `Size` is also the knob on how big
the waveform is).

A worked version of all of this is the Example level of the test suite,
which `check.sh` builds and runs:
[`tests/Example/SingleRun.hs`](tests/Example/SingleRun.hs) captures one run's
waveform in the unit-test shapes, and
[`tests/Example/Hedgehog.hs`](tests/Example/Hedgehog.hs) instruments hedgehog
properties exactly as a downstream suite would. Neither is documentation on
trust: sequenced by tasty,
[`tests/Test/ExampleOutput.hs`](tests/Test/ExampleOutput.hs) decodes the
waveforms the examples wrote and verifies they show the run they claim to
(the failing one down to its shrunk, single-cycle counterexample).

## Manual API

Everything the plugin injects is ordinary user API from
`Clash.CircuitContext`, usable directly where you want explicit control:
`component`, `imapComponents`, `traceSignalC`, `probe`, `mealyProbed`,
`withCircuitContext`, `dumpVCDC`, and the `autoTrace`/`autoProbe` pair
(these two still need the plugin enabled: its typechecker half reduces the
`CanTrace`/`CanProbe` type families).

## How it works

* The context is one implicit parameter, `?circuitContext`. `component`
  pushes a hierarchy segment; trace/probe registrations qualify their keys
  with the path at creation time. All state lives in per-simulation
  `IORef`s created by `withCircuitContext` — there is no global mutable
  state, and independent simulations don't interfere.
* Instance identity is **heap identity**: each `component`/`mealyProbed`
  application allocates a fresh hierarchy cell, resolved to a small ordinal
  at registration time via its `StableName`.
* Probing inside step functions works by pairing the mealy machine with a
  companion cycle counter; writes are keyed by `(name, cycle)` and hence
  idempotent — safe under lazy re-evaluation, and an expression that is
  never forced simply records nothing.
* The plugin is a renamer-stage rewrite (names are resolved, injection is
  by exact `Name` — user code needs no extra imports) plus a
  typechecker-plugin oracle that decides `CanTrace`/`CanProbe` by
  approximating instance solvability, keeping all evidence construction in
  GHC's ordinary instance machinery.

## HDL generation & performance

Instrumentation is **transparent to Clash**. Every recording combinator is
gated on `Clash.Magic.clashSimulation`, so during HDL generation it reduces
to its plain form and Clash's dead-code elimination drops all the tracing
machinery:

* `traceSignalC`/`probe`/`component` become the identity;
* `mealyProbed` becomes a plain `mealy` — **no** companion counter register,
  so no extra hardware in the netlist.

You can therefore instrument a design in place — add `HasCircuitContext`,
`component`, `mealyProbed`, `probe` — and still synthesize it: the generated
HDL is byte-for-byte what the un-instrumented design would produce (the
`OPAQUE`/`unsafePerformIO` workers live behind `clashSimulation` and are
never reached by the compiler). The only thing a caller must do is discharge
the `HasCircuitContext` constraint, e.g. `let ?circuitContext =
noCircuitContext in …` at the synthesis entry point.

The same gate keeps **simulation cheap when you are not recording**: under
`noCircuitContext` (i.e. not inside `withCircuitContext`) the combinators
short-circuit before touching any `IORef` — `component` skips the hierarchy
push, `mealyProbed` runs as a plain `mealy` with no cycle counter, and
`probe`/`traceSignalC` are the identity. A design that merely *carries*
instrumentation pays essentially nothing until a `withCircuitContext` run
actually collects a waveform.

## Limitations

* GHC 9.6 and 9.10 are supported (`tested-with: 9.6.7, 9.10.3`), with
  `clash-prelude >= 1.9 && < 1.12` — the `Clash.Signal.Trace` internals
  used are identical across these versions. The `cabal.project` here pins
  the 1.11 upstream commit this package is developed against (1.11 is not
  yet on Hackage).
* Instance-method and class-default bodies are not instrumented: the
  renamer pass rewrites value bindings only. A signature there carrying the
  constraint is reported (`-Wx-circuit-context-uninstrumented`) rather than
  quietly ignored.
* A constraint synonym is followed, but a constraint-kinded *type family*
  is not — nothing at renamer stage can reduce one. Superclasses need no
  handling: GHC rejects an implicit parameter in a superclass context, so a
  synonym is the only way to alias `HasCircuitContext`.
* Constraint plumbing is at the type level: `HasCircuitContext` propagates
  to a function's callers. Supply the context once, at a boundary — a test
  does it with `withCircuitContext`; a synthesis entry point (or any caller
  you don't want to thread the constraint through) does it with
  `withoutCircuitContext` (see [Opting out](#opting-out)). This is a
  compile-time concern only; it does not affect the generated hardware
  (see below).
* The traceability oracle is a conservative approximation. It reduces type
  families before deciding — so `KnownNat (BitSize SomeRecord)` and a ground
  `1 <= 8` are answered rather than declined — but an instance context it
  still cannot decide falls back to *not tracing* (never a compile error).
  It says so — `-Wx-circuit-context-undecided`, kept apart
  from the proved `-Wx-circuit-context-untraced` precisely because a wire
  lost to the approximation is as likely to be a limitation here as a fact
  about your types. Promote it with `-Werror=` on a design whose waveform
  you depend on, or golden-test your trace key sets.

## Building and testing

```
./check.sh                       # default GHC
./check.sh -w ghc-9.10.3         # or any other GHC ≥ 9.6
```

`check.sh` builds everything and runs seven suites — `manual-instrumentation`
(hand-instrumented reference), `oracle-fallback` (oracle decisions per type),
`auto-instrumentation` (the same design written naturally, instrumented
entirely by the plugin), `notation` (the real circuit-notation desugarer,
against the vendored `deps/circuit-notation`), `adt-sidecar` (the
clash-shockwaves half of the output), `plugin-diagnostics` (constraint
synonyms, idempotence under a deliberate double enable, and one live example
of every warning category) and `waveform-tests` (the test suite in two levels:
[`tests/Example/`](tests/Example/) is usage, kept honest by running, and
[`tests/Test/`](tests/Test/) pins features — recorder properties, the capture
contract, and the tasty-sequenced tests that decode the waveforms the Example
level wrote) — then diffs the generated VCDs against the goldens in
`goldens/`, asserts every warning category fired during the build, and
compiles a one-binding module three ways to check that a decline warns by
default, disappears under `-Wno-`, and is fatal under `-Werror=`. That last
one is a claim about the compiler's behaviour rather than this package's,
which is why it is checked rather than described.
VCD output is fully deterministic by design, so the goldens are
byte-identical across GHC 9.6 and 9.10.

`notation` exists only on this branch: it needs the `trace-ports` patch in
`deps/circuit-notation`, so on `main` — whose `cabal.project` is just
`packages: .` — `check.sh` runs the other suites.

## Proof of concept: tracing a real design

`main` is the plugin alone, and builds from a plain clone. Two branches carry
the proof of concept — this plugin applied to
[bittide-hardware](https://github.com/bittide/bittide-hardware), with the
instrumented checkouts committed so the waveforms can be reproduced:

| Branch | What it shows |
| --- | --- |
| `dogfood/bittide` | bittide instrumented, **every dependency on its pristine upstream pin** — what you get when you may only touch your own design |
| `dogfood/deps` | the above **plus instrumented `clash-protocols`, `clash-protocols-memmap`, `clash-cores` and `circuit-notation`** — what dependency instrumentation adds |
| `dogfood/shockwaves` | the above **plus [`clash-shockwaves`](https://github.com/clash-lang/clash-shockwaves)** — hierarchy *and* the ADT structure that makes the bits readable, in one simulation |

Each branch has a `deps/README.md` covering setup and how to generate all of its
waveforms. The diff between them is the measure of what instrumenting
dependencies buys: the headline is register *contents*, invisible from the design
side because `registerWb`'s state is a `where`-binding inside a low-level
`Circuit go`.

The findings behind both, including the ones that argue against the current
design, are in [`docs/dogfooding-bittide.md`](docs/dogfooding-bittide.md) and
[`docs/dep-instrumentation-assessment.md`](docs/dep-instrumentation-assessment.md).
The most important: `HasCircuitContext` is necessary but not sufficient — only
combinators desugared through circuit-notation get traced, so annotating a
`Circuit`-constructor combinator adds nothing.
