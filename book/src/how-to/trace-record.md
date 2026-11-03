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
scope of the function. (`printWaveform` is a helper of this book: see [the
introduction](../introduction.md#the-examples-are-checked).)

```haskell,clash group=record
>>> printWaveform 3 (master (fromList [1, 4, 5]))
master
  bus_addr
  bus_strobe
```

## A record inside a record

The inner record needs its own empty `Traceable` instance too. Its names
join the field names with `_`:

```haskell,clash group=record
data Port dom = Port
  { req :: Bus dom
  , ack :: Signal dom Bool
  }
  deriving (Generic)

instance (KnownDomain dom) => Traceable (Port dom)

slave :: (HasCircuitContext) => Signal System (Unsigned 8) -> Signal System (Unsigned 8)
slave a = mux (ack port) 0 (addr (req port))
 where
  port = Port{req = Bus{addr = a, strobe = (> 3) <$> a}, ack = strobe (req port)}
{-# OPAQUE slave #-}

>>> printWaveform 3 (slave (fromList [1, 4, 5]))
slave
  port_ack
  port_req_addr
  port_req_strobe
```

The fields of one binding all have the position of that binding in the
source, so they come in the order of their names, not in the order of the
declaration.

## Notes

- A field that cannot be recorded is left out, and the other fields are
  still recorded. A port that mixes signals with other values, for example a
  memory map, therefore loses only the part that is not a signal.
- A field is recorded only if the simulation uses it. A field that nothing
  reads is never evaluated, so it is not in the waveform at all.
- The oracle of the plugin reads the context of your instance. A context
  with type families or other constraints that are not class constraints
  can make the oracle give up: see [About the oracle](../explanation/oracle.md).
- To record a record by hand, use `traceNamed "<name>"`, the method of
  `Traceable`.
