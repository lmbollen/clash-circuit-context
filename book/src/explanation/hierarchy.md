# About the design hierarchy

A waveform of a large design is hard to read when all its signals are in
one list. clash-circuit-context gives the waveform the hierarchy of the
source: one scope for each component instance, nested as the components
call each other. This page explains how the package knows that hierarchy,
and why it names instances as it does.

{{#drawio path="diagrams/hierarchy.drawio" alt="The design top calls counter twice. In the waveform, top has the signal out and two scopes, counter_0 and counter_1, each with count and next"}}

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
under a function that supplies the context, such as `writeWaveform` or
`withoutCircuitContext`. The type checker
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

The numbers come from the calls, in the order of the source: the call of
the function, for a component that the plugin makes, and the call of
`component` otherwise. A lazy simulation can evaluate the second instance
first. If the numbers followed evaluation, a small change to a test could
swap the names of two instances.

The plugin writes `component "f"` inside `f`, so all instances of `f` reach
one `component` call. To tell them apart, each call passes its place in the
source: `HasCircuitContext` asks for an implicit parameter of type
`CallStack`, and GHC adds the site of each call to it, as it does for
`HasCallStack`. This works for each way of calling: plain calls, `$`,
operators, partial application and higher-rank arguments, also from another
module. In HDL, the call sites have no effect: a component is one module,
whatever the number of calls.

The numbers rank the instances that the run evaluated. A test that
evaluates only some of the instances numbers those in the same order, but
from 0: if a test reads only one output of a sorting network, the
comparators that this output needs are `cas_0`, `cas_1`, …, and `cas_4` can
be another comparator than in a test that reads all outputs. A single
evaluated instance has no number: `syncFF`, not `syncFF_0`. Within one
waveform, the names are in the order of the source. Between two waveforms,
compare names only when the tests evaluate the same parts of the design.

Some instances are numbered in the order in which the simulation first
reaches them:

- The instances that one call makes, for example `map` over a vector. The
  source cannot order them, and the order of evaluation can be the reverse
  of the index. `imapComponents` names element `i` as `name_i`.
- The instances made through a local function without a type signature.
  GHC adds a call site only for a function whose type asks for it, so two
  calls of such a helper are told apart only by the calls inside it. Give
  the helper a signature with `HasCircuitContext`.
- The instances made by a class method, and by a function with an
  annotation other than `NoCircuitScope`.

For how signals and scopes are ordered and named in a waveform, see
[Waveform files](../reference/waveform-files.md#the-waveform).

## Scopes are components, and nothing else

A record or a vector is data, not a level of the design. The package
therefore records the fields of a record as siblings (`bus_addr`), not as a
scope `bus` with a signal `addr`. Before this rule, most scopes in the
waveforms of a real test suite were data structures, and the hierarchy of
the design was hard to find among them.
