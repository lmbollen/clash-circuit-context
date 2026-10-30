# Types that can be recorded

A trace records a signal. A probe records a value in each step of a mealy
machine. This page lists the types that each one records, how a type is
described for a typed viewer, and which types are never recorded.

## Traces: the class `Traceable`

| Type | Recorded as |
|---|---|
| `Signal dom a`, with `KnownDomain dom`, `BitPack a`, `NFDataX a` | one signal |
| `Signal dom Integer`, `Signal dom Natural` | one signal of 64 bits, as Clash translates them |
| `DSignal dom d a` | the signal under it |
| `Reset dom`, `Enable dom` | the signal under it (a reset as its raw level) |
| `Vec n t`, with `Traceable t` | one signal for each element: `name_0`, `name_1`, … |
| A tuple of up to 12 components | each component that can be recorded, on its own |
| A record with `Generic` and an empty `Traceable` instance | each field that can be recorded: `name_field` |
| `Tagged p t` (the ports of circuit-notation) | as `t` |
| `()` | nothing |

`Typeable` is not necessary to record a signal. A field or component that
cannot be recorded is left out on its own. The others are still recorded.

## Probes: the class `Probeable`

| Type | Recorded as |
|---|---|
| A value with `BitPack` and `NFDataX` | its bits |
| `Integer`, `Natural` | 64 bits, as Clash translates them |
| `SNat n` | the `Index (n + 1)` that holds `n`. `SNat 0` is not recorded |

## Description for a typed viewer

A recorded value is described in the sidecar when its type has a `Waveform`
instance (from clash-shockwaves). `Waveform` has the superclasses `Typeable`
and `BitPack`. A value without an instance is recorded as bits, with a
warning in `x-circuit-context-undescribed`.

## Never recorded, and not reported

The plugin does not warn about a binding that no recorder could ever take:

- **In a trace**, a type with no `Signal` in its fields: a plain value in a
  component (a `Bool`, a `Vec 4 Bool`, a memory map), or a function.
- **In a probe**, a type that holds a `Signal` or a clock.
- **In both**, a type that Clash cannot translate.

Which types Clash cannot translate is declared by the open type family
`NonTranslatable t :: Bool`, exported by `Clash.CircuitContext`. It is
`'True` for:

- Lists, `NonEmpty`, functions, `IO`, and `SimOnly`.
- `IORef`, `STRef`, `MVar`, `Chan`, `ST`, `ThreadId`, `Handle`, `Ptr`,
  `FunPtr`, `ForeignPtr`, `StablePtr`, `StableName`, `Weak`, `TypeRep`,
  `Dynamic` and `SomeException`.
- `Signal`, `DSignal`, `Vec`, `Maybe`, `Either` and tuples of up to eight
  components, when they hold a type for which it is `'True`.

A type that nobody declared is taken to be translatable. A binding of it
that is not recorded is reported. To declare a type of your own, put the
instance beside the type:

```haskell
data Config = Config {names :: [String], limit :: Int}

type instance NonTranslatable Config = 'True
```

`Integer` and `Natural` are not in this family, because Clash translates
them. Inside another type, for example `Maybe Integer`, they have no
`BitPack`. The warning then tells you to use `Signed 64` or `Unsigned 64`.
