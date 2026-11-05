# How to control the warnings of the plugin

The plugin reports each decision that costs a signal or a scope as a GHC
warning, in one of five categories (see [Warning
categories](../reference/warnings.md)). This guide shows how to turn a
category off, how to make it an error, and how to mark one function as
deliberate.

The categories are ordinary GHC warning flags, so you use the flags that you
use for other warnings. Custom warning categories need GHC 9.8 or later. On
GHC 9.6 the warnings are still there, and `-Werror` makes them errors, but
you cannot select one category.

## Turn a category off

Add `-Wno-<category>` to `ghc-options`, or to an `OPTIONS_GHC` pragma for one
module:

```cabal
ghc-options: -Wno-x-circuit-context-undescribed
```

## Make a category an error

Add `-Werror=<category>`. For example, to stop a build that loses a signal
because the oracle could not decide:

```cabal
ghc-options: -Werror=x-circuit-context-undecided
```

If your project already builds with `-Werror`, each warning of the plugin is
an error. Check this before you turn the plugin on in such a project.

## See the warnings of modules that were built before

GHC writes the warnings of a module only when it compiles the module. When
only warning flags change, GHC does not compile the module again, so a new
`-Werror=` or `-Wno-` has no effect on a module that was built before. To
see all warnings again, compile all modules again:

```bash
cabal build --ghc-options=-fforce-recomp
```

## Mark a function without `OPAQUE` as deliberate

A function with `HasCircuitContext` and without `OPAQUE` gets a warning in
`x-circuit-context-uninstrumented`: it has no scope of its own. If that is
deliberate, for example in a test harness whose signals must be at the top
of the waveform, say so on the function instead of turning the category off:

```haskell
{-# ANN runSystem NoCircuitScope #-}
runSystem :: (HasCircuitContext) => Signal System Int -> Signal System Int
runSystem = ...
```

`NoCircuitScope` is exported by `Clash.CircuitContext`. It turns the warning
off for that function only. The category stays on for each function that you
did not mark.

## Make tracing a requirement

A warning never stops the build on its own. To make the build fail when one
binding cannot be recorded, record it by hand:

- `traceNamed "<name>" x`, the method of `Traceable`, for each type that
  `Traceable` has: a signal, a record of signals, a vector, a tuple.
- `traceSignalC "<name>" s` for a signal.

Both take their requirements as real constraints, so a type that cannot be
recorded is a type error. This is also how to find out what an
`x-circuit-context-undecided` warning hides. For the types, see
[Modules](../reference/modules.md#traces).
