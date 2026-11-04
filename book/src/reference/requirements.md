# Requirements

| Item | Supported |
|---|---|
| GHC | 9.6, 9.8, 9.10 and 9.12. CI tests 9.6.7, 9.8.4, 9.10.3 and 9.12.4 |
| clash-prelude | `>= 1.9 && < 1.12`. The `cabal.project` of the package pins a commit of clash-prelude 1.11, which is not on Hackage yet |
| clash-shockwaves | `>= 1.1.1 && < 1.1.4`, and `>= 1.1.3` on GHC 9.6. Only verified releases are in the range, because the sidecar uses internal modules of clash-shockwaves |
| Hackage index | on GHC 9.6, an `index-state` of 2026-09-23 or later, for clash-shockwaves 1.1.3 |
| Patched packages | none. The package works with an unmodified clash-prelude |

## Warning categories

Selecting one category of warnings needs GHC 9.8 or later. See [Warning
categories](warnings.md).

## HDL

The instrumentation adds no registers and no logic to the HDL that Clash
generates. If you run `clash` on a source file, give
`-fplugin=Clash.CircuitContext.Plugin` on the command line. See [How to
generate HDL from an instrumented design](../how-to/hdl.md).
