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
or project-wide (`ghc-options: -fplugin=Clash.CircuitContext.Plugin`).

1. **Component wrap** — a toplevel function with an `OPAQUE` pragma *and* a
   written `HasCircuitContext` constraint has its body wrapped in
   `component "<functionName>"`. Its `where`-bindings are moved into a
   `let` under the wrap (required: `where`-bindings would otherwise be
   elaborated against the *caller's* context and escape the new scope).
2. **Auto-trace** — inside any function whose signature carries
   `HasCircuitContext` (the synonym or a raw `?circuitContext`), every
   named zero-argument binding whose type is traceable becomes
   `autoTrace "<binder>" …`. Untraceable types (no `BitPack`, unknown
   domain, missing evidence for a polymorphic type) fall back to identity —
   silently, by design, decided by a typechecker-plugin oracle
   (`CanTrace`), so instrumented code never fails to compile because
   something cannot be traced.
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
  site. This is often exactly what you want for small helpers.
* Traceable today: `Signal dom a` with `(KnownDomain dom, BitPack a,
  NFDataX a, Typeable a)`, and `Vec n` thereof (traced element-wise as
  `name_0`, `name_1`, …). Extend by adding `Traceable` instances.
* Multiple instances of the same component under one parent are
  disambiguated **design-deterministically**: siblings are ordered by
  instantiation call site (never by evaluation order) and named
  `name_0`, `name_1`, …. Instances born at the *same* call site (e.g.
  `fmap acc`-style replication) cannot be design-ordered — name those by
  structural position with `imapComponents`.

### Opting out

* Prefix a binder with `_` to leave it uninstrumented.
* Omit the `OPAQUE` pragma or the constraint to keep a function untouched.
* Don't enable the plugin on a module to leave the whole module untouched.

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

## Limitations

* GHC 9.6 and 9.10 are supported (`tested-with: 9.6.7, 9.10.3`).
  `clash-prelude >= 1.11` is required and not yet on Hackage — the
  `cabal.project` here pins the upstream commit this package is tested
  against.
* Constraint synonyms bundling `HasCircuitContext` with other constraints
  are not recognized at renamer stage; write the constraint (or the raw
  implicit parameter) literally in the signature.
* Instance-method and class-default bodies are not instrumented.
* The traceability oracle is a conservative approximation: an exotic
  instance context it cannot decide falls back to *not tracing* (never a
  compile error). Golden-test your trace key sets if you depend on them.

## Building and testing

```
./check.sh                       # default GHC
./check.sh -w ghc-9.10.3         # or any other GHC ≥ 9.6
```

`check.sh` builds everything and runs three suites — `manual-smoke`
(hand-instrumented reference), `fallback` (oracle decisions per type), and
`auto-smoke` (the same design written naturally, instrumented entirely by
the plugin) — then diffs both generated VCDs against the goldens in
`goldens/` and asserts the fall-through warning fires. VCD output is fully
deterministic by design, so the goldens are byte-identical across GHC 9.6
and 9.10.
