# About the oracle

The plugin wraps each named local binding in `autoTrace`. It does this in
the renamer pass, before GHC knows the type of the binding. Many bindings
cannot be recorded: a function, a list, or a type without `BitPack`. This
page explains how the plugin handles that, and why it reports what it
reports.

## Why there is an oracle

A simple design would require `Traceable` of each wrapped binding. Then
each binding of a type that cannot be recorded would be a type error, and an
instrumented design would not compile. To turn on the plugin for a whole
package would then be impossible.

Instead, `autoTrace` asks a question at the type level: `CanTrace t`. A type
checker plugin, the oracle, answers it with `'True` or `'False` for the
type of the binding. For `'False`, `autoTrace` is the identity. Code never
fails to compile because something cannot be recorded.

## Why the oracle is an approximation

To answer `CanTrace t`, the oracle must know if GHC can solve `Traceable t`.
It does not ask GHC to solve the constraint, because a failed attempt would
be an error in your module. It looks at the instances itself: it finds the
instance for `t`, and checks the context of that instance in the same way.
It reduces type families first, so `KnownNat (BitSize T)` and `1 <= 8` get
an answer.

Some contexts it cannot check, for example `1 <= n` where `n` is a type
variable. Then the oracle gives up. It does not guess `'True`, because a
wrong `'True` is a type error in your code. It answers `'False` and reports
`x-circuit-context-undecided`. That category is separate from
`x-circuit-context-untraced` because the two mean different things:
`untraced` is a fact about your types, but `undecided` can be a limit of
the oracle.

## Why a lost signal is a warning

The identity fallback has a disadvantage: a signal that is not recorded
looks the same as a signal that nobody asked for. The plugin therefore
reports each decision that costs a signal, as a GHC warning with the fix.
You control the warnings with the flags of GHC, so a project can make a lost
signal an error with `-Werror=`.

## Why some bindings are not reported

A warning without a fix is noise, and noise teaches users to turn the
category off. So the plugin does not report a binding that no recorder could
ever take:

- In a component, most named bindings are not signals: a `Bool` in a
  lambda, a configuration value, a helper function. A trace records signals
  only, so these are not lost.
- Some types can never be hardware: lists, functions, references. The family
  `NonTranslatable` declares them. A declaration is better than a guess here:
  a guess that a type of yours is not translatable would hide a real lost
  signal. With a declaration, a type that nobody declared still warns, and
  the warning offers the declaration as a fix.
- A binding with a higher-rank type is always a function, so it is never a
  signal.

Whether a type holds a signal is not declared. It is a fact about each type,
and the oracle reads it from the constructors.
