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

## An example of each category

Each example below is a part of one module, `Warn.hs`. The text after each
example is the warning that GHC prints for it.

### `-Wx-circuit-context-untraced`

A payload without `BitPack`:

```haskell
loose :: (HasCircuitContext) => Signal System a -> Signal System a
loose inp = held
 where
  held = inp
{-# OPAQUE loose #-}
```

```text
Warn.hs:24:3: warning: [-Wx-circuit-context-untraced]
    not traced: Signal System a
    no instance for: BitPack a
    Add that constraint to the enclosing signature to record this binding.
```

### `-Wx-circuit-context-undecided`

The `BitPack` instance of `Awkward n` has the context `1 <= n`, and the
function only knows `KnownNat n`. The instance needs
`UndecidableInstances`:

```haskell
newtype Awkward n = Awkward (Unsigned n)
  deriving (Generic, NFDataX)

instance (KnownNat n, 1 <= n) => BitPack (Awkward n) where
  type BitSize (Awkward n) = n
  pack (Awkward u) = pack u
  unpack = Awkward . unpack

sized :: (HasCircuitContext, KnownNat n) => Signal System (Awkward n) -> Signal System (Awkward n)
sized inp = held
 where
  held = inp
{-# OPAQUE sized #-}
```

```text
Warn.hs:18:3: warning: [-Wx-circuit-context-undecided]
    not traced: Signal System (Awkward n)
    the oracle could not decide: 1 <= n
    This may be a limit of the approximation rather than a fact about the type.
    Name the binding with traceNamed, which takes Traceable as a real constraint, to find out which.
```

`traceNamed` is the method of `Traceable`. Write `held = traceNamed "held"
inp`: GHC then tries to solve `Traceable` itself, and reports what is
really missing, or records the signal. For a plain signal,
`traceSignalC "held" inp` does the same.

### `-Wx-circuit-context-undescribed`

A payload with `BitPack` and without `Waveform`:

```haskell
data Mode = Fast | Slow
  deriving (Generic, NFDataX, BitPack)

plain :: (HasCircuitContext) => Signal System Mode -> Signal System Mode
plain inp = mode
 where
  mode = inp
{-# OPAQUE plain #-}
```

```text
Warn.hs:33:3: warning: [-Wx-circuit-context-undescribed]
    recorded as raw bits, not described: Mode
    no instance for: Waveform Mode
    Derive or write a clash-shockwaves Waveform instance so a typed viewer shows its constructors and fields.
```

### `-Wx-circuit-context-uninstrumented`

`HasCircuitContext` without `OPAQUE`:

```haskell
helper :: (HasCircuitContext) => Signal System Int -> Signal System Int
helper inp = doubled
 where
  doubled = inp + inp
```

```text
Warn.hs:37:1: warning: [-Wx-circuit-context-uninstrumented]
    'helper' has HasCircuitContext but no OPAQUE pragma.
    Its bindings trace in the CALLER's scope, not a scope of its own.
    Add {-# OPAQUE helper #-} if you wanted one.
```

### `-Wx-circuit-context`

Guards that can fail, on an equation that is not the last:

```haskell
pick :: (HasCircuitContext) => Bool -> Signal System Int -> Signal System Int
pick b inp
  | b = out
 where
  out = inp
pick _ inp = inp
{-# OPAQUE pick #-}
```

```text
Warn.hs:42:1: warning: [-Wx-circuit-context]
    skipping component wrap for an equation of 'pick'.
    Its guards can fall through to a later equation, which the wrap would turn into a crash.
    Its bindings still trace, at the enclosing scope.
```

## What is not reported

- A binding that no recorder could ever take, for example a list or a
  function. See [Types that can be recorded](recordable-types.md).
- A binding with a higher-rank type. It is always a function.
- A closed local binding without a signature, unless it refers to itself. It
  is skipped before the oracle is asked (see [What the plugin
  instruments](instrumentation.md#local-bindings)).
- A function with `HasCircuitContext`, without `OPAQUE`, and with the
  annotation `{-# ANN f NoCircuitScope #-}`.

## Versions of GHC

The categories need GHC 9.8 or later. On GHC 9.6 the messages are ordinary
warnings: `-Werror` makes them errors, but you cannot select one category.
