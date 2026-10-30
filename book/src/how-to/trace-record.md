# How to trace a record of signals

A binding whose type is a record of signals (a bus, or the ports of a
block) is not a `Signal`, so the plugin cannot record it without help. This
guide shows how to record each field of such a record.

1. Derive `Generic` for the record.
2. Write an empty `Traceable` instance. Use only ordinary class constraints
   in its context, for example `KnownDomain dom`.

```haskell,clash group=record hidden
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}
module Record where

import Clash.Explicit.Prelude
import Clash.CircuitContext
import BookSupport
```

```haskell,clash group=record
data Bus dom = Bus
  { addr :: Signal dom (Unsigned 8)
  , strobe :: Signal dom Bool
  }
  deriving (Generic)

instance (KnownDomain dom) => Traceable (Bus dom)

master :: (HasCircuitContext) => Signal System (Unsigned 8) -> Signal System (Unsigned 8)
master a = mux (strobe bus) (addr bus) 0
 where
  bus = Bus{addr = a, strobe = (> 3) <$> a}
{-# OPAQUE master #-}
```

Each field of `bus` is a signal with the name `<binding>_<field>`, in the
scope of the function:

```haskell,clash group=record
>>> printWaveform 3 (master (fromList [1, 4, 5]))
master
  bus_strobe
  bus_addr
```

## Notes

- A field that cannot be recorded is left out, and the other fields are
  still recorded. A port that mixes signals with other values, for example a
  memory map, therefore loses only the part that is not a signal.
- A record inside a record is recorded the same way. Its names join the
  field names with `_`: a field `req` that holds a `Bus` gives
  `port_req_addr` and `port_req_strobe`.
- A field is recorded only if the simulation uses it. A field that nothing
  reads is never evaluated, so it has no values to record.
- The oracle of the plugin reads the context of your instance. A context
  with type families or other constraints that are not class constraints
  can make the oracle give up: see [About the oracle](../explanation/oracle.md).
