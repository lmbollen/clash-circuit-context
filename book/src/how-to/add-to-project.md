# How to add clash-circuit-context to a project

This guide shows how to make a Cabal project use clash-circuit-context and
its plugin. For the versions of GHC, clash-prelude and clash-shockwaves that
the package supports, see [Requirements](../reference/requirements.md). For a
new project, the [tutorial](../tutorials/first-waveform.md#make-the-project)
gives complete files.

1. The package is not on Hackage. Add it to `cabal.project` as a
   source-repository package. Use the full commit hash, of 40 characters, as
   the tag (Cabal does not accept a short hash):

   ```cabal
   source-repository-package
     type: git
     location: https://github.com/lmbollen/clash-circuit-context.git
     tag: <commit>
   ```

   If you have a local checkout of the package, you can add its directory to
   `packages:` instead, for example `packages: . ../clash-circuit-context`.
   Then do not turn on the tests for all packages of the project, with
   `tests: True` at the top level of `cabal.project` or with
   `cabal build all --enable-tests`: Cabal then also builds the test suites
   of clash-circuit-context. Turn on the tests of your package only:

   ```cabal
   package <your-package>
     tests: True
   ```

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

Each module that defines a function whose signature has `HasCircuitContext`
needs the plugin. A module that only calls such functions, for example a
test suite, does not.

If a module that defines such a function is compiled without the plugin,
the function keeps the constraint `TurnOnTheCircuitContextPlugin`. Only the
plugin solves this constraint. The build stops at a call of the function
from code without `HasCircuitContext`, in a module without the plugin. For
example, the design that you give to `writeWaveform` in a test suite, or a
call under `withoutCircuitContext`:

```text
Main.hs:8:69: error: [GHC-39999]
    • Could not deduce ‘Clash.CircuitContext.Core.TurnOnTheCircuitContextPlugin’
        arising from a use of ‘counter’
```

The error can come later than the module that needs the plugin:

- A function with `HasCircuitContext` in a module without the plugin passes
  the constraint on to its callers. So the error can name a function that
  only calls the function that was compiled without the plugin.
- In a module with the plugin, the plugin solves the constraint. A call
  there is not an error. The function that was compiled without the plugin
  is then not instrumented: its bindings are not recorded, and it is not a
  component.

Turn on the plugin in each module that defines a function with
`HasCircuitContext`, not in the module of the call.

A module with an empty `Traceable` instance, compiled without the plugin,
stops with an error that names `TurnOnThePlugin` and the flag to add.

The error says what to do: turn on the plugin for that module (see step 3
or step 4). The
`clash` executable does not read the `OPTIONS_GHC` pragma of a source file:
see [How to generate HDL from an instrumented
design](hdl.md#run-clash-on-a-source-file).
