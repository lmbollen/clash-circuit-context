{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}

{- | A constraint synonym for "Test.PluginDiagnostics" to use from ANOTHER
module — the half of the synonym question the plugin cannot answer from the
group it is handed, because an imported synonym's right-hand side lives in
an interface.
-}
module Test.DesignCtx (DesignCtx) where

import Clash.Explicit.Prelude

import Clash.CircuitContext (HasCircuitContext)

{- | The shape a design-wide context alias actually takes in a real project:
the domain constraints the module needs, plus tracing.
-}
type DesignCtx dom = (KnownDomain dom, HasCircuitContext)
