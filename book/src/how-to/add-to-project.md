# How to add clash-circuit-context to a project

This guide shows how to make a Cabal project use clash-circuit-context and
its plugin. For the versions of GHC, clash-prelude and clash-shockwaves that
the package supports, see [Requirements](../reference/requirements.md). For a
new project, the [tutorial](../tutorials/first-waveform.md#make-the-project)
gives complete files.

1. The package is not on Hackage. Add it to `cabal.project` as a
   source-repository package. Use a commit hash as the tag:

   ```cabal
   source-repository-package
     type: git
     location: https://github.com/lmbollen/clash-circuit-context.git
     tag: <commit>
   ```

   If you have a local checkout of the package, you can add its directory to
   `packages:` instead, for example `packages: . ../clash-circuit-context`.

2. The package is tested with a commit of clash-prelude 1.11, which is not
   on Hackage yet. If your project does not already pin a clash-prelude,
   pin the same one. Copy the `tag` and the `index-state` from the
   `cabal.project` of the package. When this book was written, they were:

   ```cabal
   source-repository-package
     type: git
     location: https://github.com/clash-lang/clash-compiler.git
     tag: e16599387a68d6361134b4fe7e63bf4ad3ae8408
     subdir: clash-prelude

   index-state: 2026-09-23T00:00:00Z
   ```

   On GHC 9.6, the `index-state` must be 2026-09-23 or later, for
   clash-shockwaves 1.1.3, the first release that builds on GHC 9.6. To
   generate HDL, you also need `clash-lib` and `clash-ghc` from
   the same commit: see [How to generate HDL from an instrumented
   design](hdl.md#get-a-clash-executable).

3. In the `.cabal` file of the package that holds your designs, add the
   dependency and turn on the plugin for the whole package:

   ```cabal
   build-depends: clash-circuit-context
   ghc-options:   -fplugin=Clash.CircuitContext.Plugin
   ```

   The plugin changes only functions whose signature has
   `HasCircuitContext` or `HasProbe`. Other modules compile to the same code
   as before.

   If you derive `Waveform` for your types (see [How to show payload types
   in a waveform viewer](typed-waveforms.md)), also add `clash-shockwaves`
   to `build-depends`.

4. If you want the plugin in some modules only, do not do step 3's
   `ghc-options`. Add this line at the top of each of those modules instead:

   ```haskell
   {-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}
   ```

   Do not do both. The plugin then runs twice. This is harmless, but it does
   twice the work.

5. Build the package. A function with `HasCircuitContext` and `OPAQUE` is
   now a component: see [How to make a function a
   component](component.md).

## If the plugin is not on

If a module with an empty `Traceable` instance is compiled without the
plugin, the build fails with an error like this one:

```text
Bus.hs:10:10: error: [GHC-39999]
    • Could not deduce ‘Clash.CircuitContext.Auto.TurnOnThePlugin
                          "add -fplugin=Clash.CircuitContext.Plugin to the ghc-options of this package, or to the command line of clash"’
        arising from a use of ‘Clash.CircuitContext.Auto.$dmtraceNamed’
```

The error says what to do: turn on the plugin for that module (see step 3
or step 4). The
`clash` executable does not read the `OPTIONS_GHC` pragma of a source file:
see [How to generate HDL from an instrumented
design](hdl.md#run-clash-on-a-source-file).
