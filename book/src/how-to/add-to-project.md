# How to add clash-circuit-context to a project

This guide shows how to make a Cabal project use clash-circuit-context and
its plugin. For the versions of GHC, clash-prelude and clash-shockwaves that
the package supports, see [Requirements](../reference/requirements.md).

1. The package is not on Hackage. Add it to `cabal.project` as a
   source-repository package. Use a commit hash as the tag:

   ```cabal
   source-repository-package
     type: git
     location: https://github.com/lmbollen/clash-circuit-context.git
     tag: <commit>
   ```

2. If your project does not use a released clash-prelude, pin the same
   clash-prelude commit as this package. See its `cabal.project`.

3. In the `.cabal` file of the package that holds your designs, add the
   dependency and turn on the plugin for the whole package:

   ```cabal
   build-depends: clash-circuit-context
   ghc-options:   -fplugin=Clash.CircuitContext.Plugin
   ```

   The plugin changes only functions whose signature has
   `HasCircuitContext` or `HasProbe`. Other modules compile to the same code
   as before.

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
