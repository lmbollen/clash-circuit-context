# Warning categories

The plugin reports in five GHC warning categories. Each category is on by
default. Each warning names the binding and, where one exists, the fix. For
how to turn a category off or make it an error, see [How to control the
warnings of the plugin](../how-to/warnings.md).

| Flag | The plugin reports |
|---|---|
| `-Wx-circuit-context` | It could not do something that the source clearly asks for. Example: a component equation whose guards can fail, and that is not the last equation. |
| `-Wx-circuit-context-uninstrumented` | A signature asks for instrumentation where the plugin does not instrument, or does not make a scope. The cases: `HasCircuitContext` without `OPAQUE`, `OPAQUE` without a signature, both constraints in one signature, and a class method or instance method. |
| `-Wx-circuit-context-undecided` | The oracle could not decide if a binding can be recorded, so it did not record it. This can be a limit of the oracle, not a fact about the type. |
| `-Wx-circuit-context-untraced` | A binding could be recorded, but a requirement is missing: an instance or a constraint. The message names it. |
| `-Wx-circuit-context-undescribed` | A binding is recorded, but as bits: its type has no `Waveform` instance. Nothing is lost. |

## Example

A warning from the test suite of this package. The type `Awkward n` has a
`BitPack` instance with the context `1 <= n`, and the function only knows
`KnownNat n`:

```text
tests/Test/PluginDiagnostics.hs:305:3: warning: [-Wx-circuit-context-undecided]
    not traced: Signal dom (Awkward n)
    the oracle could not decide: 1 <= n
    This may be a limit of the approximation rather than a fact about the type.
    Name the binding with traceSignalC, which takes Traceable as a real constraint, to find out which.
    |
305 |   held = inp
    |   ^^^^^^^^^^
```

## What is not reported

- A binding that no recorder could ever take, for example a list or a
  function. See [Types that can be recorded](recordable-types.md).
- A binding with a higher-rank type. It is always a function.
- A closed local binding without a signature. It is skipped before the
  oracle is asked (see [What the plugin
  instruments](instrumentation.md#local-bindings)).
- A function with `HasCircuitContext`, without `OPAQUE`, and with the
  annotation `{-# ANN f NoCircuitScope #-}`.

## Versions of GHC

The categories need GHC 9.8 or later. On GHC 9.6 the messages are ordinary
warnings: `-Werror` makes them errors, but you cannot select one category.
