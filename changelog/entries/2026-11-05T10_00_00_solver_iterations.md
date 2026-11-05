---
no-links: true
---

# FIXED
A component whose signals have a width such as `aw + 1` compiles with
`ghc-typelits-natnormalise` and `ghc-typelits-knownnat` enabled before the
plugin. Before, it could fail with `solveWanteds: too many iterations`: the
plugin decided whether to record a binding in rounds of GHC's constraint
solver that the typelits plugins needed too. It now decides while GHC
rewrites the constraint, when it can.
