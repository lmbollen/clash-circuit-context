# What the plugin instruments

The plugin `Clash.CircuitContext.Plugin` has two parts. A renamer pass
rewrites the source before type checking. A type checker plugin, the oracle,
decides for each binding if it can be recorded (see [About the
oracle](../explanation/oracle.md)). This page lists the rules of the renamer
pass.

## Which functions

| The signature of a top-level function has | The plugin |
|---|---|
| `HasCircuitContext`, and the function has `OPAQUE` | makes the function a component, and traces its named local bindings |
| `HasCircuitContext`, without `OPAQUE` | traces its named local bindings, in the scope of the caller. Warning: `x-circuit-context-uninstrumented` |
| `HasProbe` | probes its named local bindings |
| neither | does not change it |

The plugin sees the constraint in these forms:

- `HasCircuitContext` or `HasProbe`, by name.
- The implicit parameter itself: `?circuitContext` or `?probe`.
- A constraint synonym that expands to one of them, declared in the module
  (also above a Template Haskell splice) or imported.

It does not see a constraint behind a type family, or in a superclass.

## Components

The body of a component `f` becomes `component "f" (…)`. Its `where`
bindings move into a `let` inside the wrap, so their signals are in the scope
of `f`.

| Shape of the equation | Result |
|---|---|
| One right-hand side | wrapped |
| Guards, of which the last is `otherwise` or has no guard | wrapped, with the guards in a `case` |
| Guards on the last equation of the function | wrapped, with the guards in a `case` |
| Guards on an equation that is not the last, and that can fail | not wrapped. Warning: `x-circuit-context` |
| A right-hand side with a higher-rank type | wrapped after eta-expansion (see below) |
| Already wrapped in `component "f"` | not wrapped again |

### Higher-rank components

The wrap cannot be instantiated at a type with a `forall` or a constraint
inside it. If the right-hand side has such a type after the patterns of the
equation, the plugin adds arguments to the equation until the type is
rank-1, and applies the body to them inside the wrap:

```haskell
withSyncReset :: (HasCircuitContext, KnownDomain dom) => Signal dom Bool -> ((HiddenReset dom) => r) -> r
withSyncReset clear = withReset (unsafeFromActiveHigh rstSig) where …
-- becomes
withSyncReset clear eta:0 = component "withSyncReset" (let … in withReset (…) eta:0)
```

Sometimes no number of arguments gives a rank-1 type. This needs
`ImpredicativeTypes`. Then the equation is wrapped as written.

## Local bindings

Inside an instrumented function, each named local binding is wrapped in
`autoTrace "<name>"` (or `autoProbe "<name>"` in a `HasProbe` function).

{{#drawio path="diagrams/binding.drawio" alt="The decisions for one named local binding, from left alone or skipped to recorded, and the warning for each outcome."}}

| Binding | Result |
|---|---|
| `x = e`, with no arguments | wrapped |
| A pattern binding, `(a, b) = …` or `Bus{addr, strobe} = …` | each binder is renamed, and a sibling `a = autoTrace "a" a'` is added |
| A function with arguments, `f x = …` | not wrapped |
| A name that starts with `_` | not wrapped |
| A name that contains `:` (made by a code generator) | not wrapped |
| A closed binding (no free local variables) without its own signature | not wrapped, and not reported |
| A closed binding with its own signature | wrapped |
| A binding with a higher-rank type | not wrapped, and not reported |

A binding is closed when its right-hand side uses no local variable.

## Modes

- A local signature with `HasProbe` or `HasCircuitContext` sets the mode
  for that binding and the bindings inside it. The innermost signature wins.
  So a `HasProbe` step function inside a component probes.
- A signature with both constraints probes. Warning:
  `x-circuit-context-uninstrumented`.

## Other rules

- An `OPAQUE` function without a signature is not instrumented. Warning:
  `x-circuit-context-uninstrumented`.
- The bodies of class methods and instance methods are not instrumented.
  Warning: `x-circuit-context-uninstrumented`, if the signature has the
  constraint.
- The pass is idempotent. If the plugin is turned on twice, the second run
  makes no change.
- A binding that already applies `traceSignalC`, `traceSignalSW`, `probe` or
  `probeSW` with its own name does not get a second recorder.
