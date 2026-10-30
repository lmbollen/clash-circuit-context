# Requirements

| Item | Supported |
|---|---|
| GHC | 9.6, 9.8, 9.10 and 9.12. CI tests 9.6.7, 9.8.4, 9.10.3 and 9.12.4 |
| clash-prelude | `>= 1.9 && < 1.12`. The `cabal.project` of the package pins a commit of clash-prelude 1.11, which is not on Hackage yet |
| clash-shockwaves | the range in `clash-circuit-context.cabal`. Only verified releases are in it, because the sidecar uses internal modules of clash-shockwaves |
| Patched packages | none. The package works with an unmodified clash-prelude |

## Warning categories

Selecting one category of warnings needs GHC 9.8 or later. See [Warning
categories](warnings.md).

## HDL

The instrumentation has no effect on the HDL that Clash generates. See [How
to generate HDL from an instrumented design](../how-to/hdl.md).
