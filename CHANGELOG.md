# Changelog

## 0.1.0.0 — unreleased

Initial version.

* Runtime (`Clash.CircuitContext.Core`): scoped simulation context on
  unmodified clash-prelude — `component` hierarchy, per-simulation trace
  maps (`traceSignalC`), probes inside mealy step functions
  (`mealyProbed`/`probe`), hierarchical VCD dumping (`dumpVCDC`).
  Instance identity is heap identity (StableName), resolved to
  design-deterministic `_0`/`_1` sibling names ordered by instantiation
  call site; `imapComponents` names replicated instances by structural
  position.
* GHC plugin (`Clash.CircuitContext.Plugin`): automatic instrumentation.
  `OPAQUE` + `HasCircuitContext` toplevel functions become components
  (guarded equations are case-encoded when they cannot fall through);
  named local bindings in `HasCircuitContext` functions are auto-traced,
  in `HasProbe` functions auto-probed; `_`-prefix opts a binding out.
  Traceability is decided by a typechecker-plugin oracle (`CanTrace`/
  `CanProbe`), so untraceable types fall back to identity without errors.
* Supported GHCs: 9.6, 9.10.
