# About typed waveforms

A VCD knows where a signal is and how wide it is: `top.bus`, 72 bits. It does
not know that those bits are a record with an address field and a strobe.
This page explains how clash-circuit-context and
[clash-shockwaves](https://github.com/clash-lang/clash-shockwaves) work
together, and why describing is separate from recording.

## Two packages, two questions

| | clash-circuit-context | clash-shockwaves |
|---|---|---|
| Knows | where a signal is | what a signal is |
| Output | one VCD scope for each component instance | the structure of a type, as a JSON sidecar |
| Source of the information | the hierarchy of the source | the `Waveform` instance of the payload type |

The plugin records a signal, and also takes the description of its type from
the `Waveform` instance at that moment, when the type is still known. The
dump writes both files from one set of maps, and keys the sidecar by the
paths that the VCD declares. Nothing is matched by name later.

## Why recording and describing are separate

An earlier version recorded a signal only if its type had a `Waveform`
instance. On a real design, that lost 10 % of the signals. The two
requirements differ:

- Recording needs `BitPack` and `NFDataX`.
- Describing needs `Waveform`, and its superclass `Typeable`.

A type that has the first and not the second is common. For example,
`Waveform (Index n)` needs `1 <= n`, but `BitPack (Index n)` needs only
`KnownNat n`. The state of a mealy machine, which only a probe can see, is
often such a type.

So the plugin decides the two questions separately. A signal is recorded
when it can be, and described when it can be. A signal that is recorded but
not described gets a warning in its own category,
`x-circuit-context-undescribed`. It is not `untraced`, because nothing is
lost: a project that makes lost signals an error does not fail for a
missing description.

## Where instances belong

A `Waveform` instance belongs in the package that defines the type, beside
its `BitPack` instance. An orphan instance in a different package is visible
only where that package is imported, so the same type can be described in
one design and not in another.
