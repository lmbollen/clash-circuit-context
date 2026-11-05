---
no-links: true
---

# CHANGED
The instances of a component are numbered by where the component is
called, also when the plugin made the component. The plugin splits each
function whose signature asks for `HasCircuitContext`, top-level or local,
into a wrapper with its name and signature, which adds the site of each call
to the context's call path, and a worker `$wf` with its body and pragmas
(a `SPECIALISE` too), which Clash makes one HDL module of, named `f`. The
calls of a helper with a signature order the instances it makes, and the
signals it records into its caller's scope come call by call. Before, the
instances of a plugin-made component were numbered in the order the
simulation first reached them. A `map` or `fmap` over a vector, a class
method, a local function without a signature, and a function with an
annotation other than `NoCircuitScope` still number their instances that
way. The numbers rank the instances that the run evaluated: a test that
evaluates only some of them numbers those in the same order, but from 0, and
a single evaluated instance gets no number.

`HasCircuitContext` is now `(CallSitesOnly, CircuitContextOnly,
TurnOnTheCircuitContextPlugin)`. The call sites are an implicit parameter
`?circuitCallSites :: CallStack` of their own: a function's own
`HasCallStack`, `error`, and `withFrozenCallStack` are not touched. The
context is a `SimOnly CircuitContext`; `noCircuitContext` is one too, so
`let ?circuitContext = noCircuitContext in …` still supplies it. Only the
plugin solves `TurnOnTheCircuitContextPlugin`: compiling a call of an
instrumented function without the plugin (for example `clash Design.hs`
without `-fplugin`, which ignores an `OPTIONS_GHC` plugin) is now an error
that names it, instead of HDL with one module per call. Code instrumented by
hand without the plugin asks for `CircuitContextOnly` instead. The arguments
of `component`, `imapComponents`, `withoutCircuitContext` and the waveform
functions are given `CircuitContextOnly`.

The plugin writes `HasCircuitContext` out as its constraints in the
wrapper's signature, also behind a synonym of the designer's (`type MyC dom
= (HasCircuitContext, KnownDomain dom)`): GHC 9.8 gives two calls in one
expression one call stack when the callee's type asks for a constraint
tuple.

A top entity (`Synthesize`) whose signature asks for `HasCircuitContext` is
an error: Clash would add ports for the context, and the port names of the
annotation would land on the wrong ports.
