# About the design hierarchy

A waveform of a large design is hard to read when all its signals are in
one list. clash-circuit-context gives the waveform the hierarchy of the
source: one scope for each component instance, nested as the components
call each other. This page explains how the package knows that hierarchy,
and why it names instances as it does.

{{#drawio path="diagrams/hierarchy.drawio" alt="The design top calls counter twice. In the VCD, top has the signal out and two scopes, counter_0 and counter_1, each with count and next"}}

## The context is an implicit parameter

Clash code is ordinary Haskell, and a simulation is the evaluation of lazy
lists. There is no elaboration step that walks the design and can tell
which instance is inside which. The package therefore carries the position
in the hierarchy as a value: the implicit parameter `?circuitContext`, which
`HasCircuitContext` names.

`component "f" e` evaluates `e` with a context that has one more level, `f`.
Each trace and probe inside `e` takes its path from the context when it is
made. That is why a function without `OPAQUE` records into the scope of its
caller: it has no `component` of its own, so it uses the context that the
caller gives it.

The constraint has a cost: it spreads to each caller. A function that calls
an instrumented function must also have `HasCircuitContext`, or call it
under `withCircuitContext` or `withoutCircuitContext`. The type checker
makes you decide where the hierarchy starts, and where recording stops.

## Why `OPAQUE`

The plugin needs a mark that a function is a component, and the constraint
alone is not enough: a small helper with `HasCircuitContext` often belongs in
the scope of its caller. `OPAQUE` is that mark.

It is also the correct mark, because of what it does to GHC. `OPAQUE` stops
GHC from inlining or specialising the function, so each call stays a
separate application. The package tells instances apart by their identity on
the heap (see below), and that needs each call to stay separate: a call that
GHC inlines, or shares with another call, loses its own identity. Clash also
reads `OPAQUE`: each `OPAQUE` function is a module of its own in the HDL. A
component in the waveform is then also a module in the hardware.

## How instances are named

Two calls of the same component under one parent need two names. The
package identifies an instance by the identity of its data on the heap, not
by its name, and then gives the siblings numbers: `name_0`, `name_1`, … .

The numbers come from the order of the call sites in the source, not from
the order in which the simulation evaluates them. A lazy simulation can
evaluate the second instance first. If the numbers followed evaluation, a
small change to a test could swap the names of two instances. With the
order of the source, the same design gives the same names in each
simulation.

Instances made at one call site, for example by `map` over a vector, have
the same call site, so the source cannot order them. For these, the
position in the vector is the information that is missing. `imapComponents`
gives it, and names element `i` as `name_i`.

## Scopes are components, and nothing else

A record or a vector is data, not a level of the design. The package
therefore records the fields of a record as siblings (`bus_addr`), not as a
scope `bus` with a signal `addr`. Before this rule, most scopes in the
waveforms of a real test suite were data structures, and the hierarchy of
the design was hard to find among them.
