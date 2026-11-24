---
no-links: true
---

# FIXED
An unsigned local binding that uses no local variable (a *closed* binding,
such as `pulse = riseEvery clockGen resetGen enableGen d4`) is still not
recorded, but it is no longer silent: when GHC did not generalise it, the
plugin reports it in `-Wx-circuit-context-untraced` and names the type
signature that records it. A closed binding that GHC did generalise (a
polymorphic helper such as `noWrite = pure Nothing`) has no single waveform
and is not reported.
