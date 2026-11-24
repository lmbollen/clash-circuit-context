# Warning categories

The plugin reports in five GHC warning categories. Each category is on by
default. Each warning points at the binding, names the type or the
constraint that is missing, and, where one exists, gives the fix. Under
`MonoLocalBinds` (which `TypeFamilies` and `GADTs` turn on), GHC can solve
the constraints of several bindings of one type together; the plugin then
reports one of those bindings. For
how to turn a category off or make it an error, see [How to control the
warnings of the plugin](../how-to/warnings.md).

| Flag | The plugin reports |
|---|---|
| `-Wx-circuit-context` | It could not do something that the source clearly asks for. Example: a component equation whose guards can fail, and that is not the last equation. |
| `-Wx-circuit-context-uninstrumented` | A signature asks for instrumentation where the plugin does not instrument, or does not make a scope. The cases: `HasCircuitContext` without `OPAQUE`, `OPAQUE` without a signature, both constraints in one signature, the body of an instance method or a class default that loses something (a local binding that a recorder would take, or `OPAQUE` on a method with `HasCircuitContext`), and a step function without `HasProbe` given to `mealyProbed`, `mooreProbed`, `mealyBProbed`, `mooreBProbed` or `probeFmap`. |
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

A closed binding without a signature, which GHC did not generalise:

```haskell
pulses :: (HasCircuitContext) => Signal System Bool -> Signal System Bool
pulses inp = (&&) <$> inp <*> pulse
 where
  pulse = riseEvery clockGen resetGen enableGen d4
{-# OPAQUE pulses #-}
```

```text
Warn.hs:36:3: warning: [-Wx-circuit-context-untraced]
    not traced: pulse :: Signal System Bool
    It uses no local variable and has no type signature, and such a binding is left alone: GHC can generalise it, and recording it would stop that.
    GHC did not generalise this one. Give it its type signature to record it: pulse :: Signal System Bool
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

A step function without `HasProbe`, given to `mealyProbed`. A local
function without a signature has no `HasProbe`, so nothing in it is
probed:

```haskell
toggle :: (HasCircuitContext) => Signal System Bool -> Signal System Bool
toggle press = on
 where
  on = mealyProbed clockGen resetGen enableGen step False press
  step s p = (s', s)
   where
    s' = if p then not s else s
{-# OPAQUE toggle #-}
```

```text
Warn.hs:11:48: warning: [-Wx-circuit-context-uninstrumented]
    'step' probes nothing: it is the step function of mealyProbed, and its type has no HasProbe.
    The plugin probes the local bindings of a function only when its signature has HasProbe.
    To probe them, give 'step' a type signature with HasProbe (step :: (HasProbe) => ...), or call probe in it.
    If it has nothing to probe, use mealy in place of mealyProbed.
```

The warning points at the argument. For `probeFmap`, it says "the function
of probeFmap" and "use fmap". A step function with `HasProbe` in its
signature is not reported, also when it has nothing to probe, and neither
is a step function that calls `probe`.

An instance method whose body has a local binding that a recorder would
take. The plugin does not instrument the body of an instance method or a
class default:

```haskell
class Filter f where
  run :: (HasCircuitContext) => f -> Signal System Int -> Signal System Int

data Delay = Delay

instance Filter Delay where
  run _ inp = held
   where
    held = register clockGen resetGen enableGen 0 inp
```

```text
Warn.hs:35:3: warning: [-Wx-circuit-context-uninstrumented]
    'run' is an instance method, and its class gives it HasCircuitContext.
    The plugin does not reach the body of an instance method or a class default, so these local bindings are not traced here: held.
    To trace them, move the body to a top-level function with HasCircuitContext and OPAQUE, and call that function here.
```

`OPAQUE` on an instance method of a class with `HasCircuitContext`:

```haskell
data Twice = Twice

instance Filter Twice where
  run _ inp = inp + inp
  {-# OPAQUE run #-}
```

```text
Warn.hs:40:3: warning: [-Wx-circuit-context-uninstrumented]
    'run' is an instance method, and its class gives it HasCircuitContext.
    OPAQUE does not make 'run' a component: the plugin makes components of top-level functions only.
    Clash makes an HDL module of it for each call site, and these modules differ in their names only.
    Put OPAQUE on the top-level function that it calls, and remove it here.
```

The class signature is not reported: it needs the constraint, so that the
instances can call instrumented functions. An instance method that only
calls a top-level component is not reported either. See [How to control
the warnings of the
plugin](../how-to/warnings.md#instrument-a-class-method).

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
- The signature of a class method, with `HasCircuitContext` or `HasProbe`.
- The body of an instance method or a class default that loses nothing:
  for example, one that only calls a top-level function, or one whose
  local bindings are functions or start with `_`.
- A step function with `HasProbe` in its signature, also when it has
  nothing to probe, and a step function that calls `probe`.

## Versions of GHC

The categories need GHC 9.8 or later. On GHC 9.6 the messages are ordinary
warnings: `-Werror` makes them errors, but you cannot select one category.

## The plugin is not on

A module that defines a function whose signature has `HasCircuitContext`
must be compiled with the plugin. Without it, a call of that function stops
the build, in the module of the call:

```text
Could not deduce ‘Clash.CircuitContext.Core.TurnOnTheCircuitContextPlugin’
  arising from a use of ‘f’
```

Add `-fplugin=Clash.CircuitContext.Plugin` to the `ghc-options` of the
package that defines `f`, or to the command line of `clash`. See [If the plugin
is not on](../how-to/add-to-project.md#if-the-plugin-is-not-on).
