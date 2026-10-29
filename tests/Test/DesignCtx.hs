{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

{- | Synonyms for "Test.PluginDiagnostics" and "Test.Downstream" to use from
ANOTHER module — the half of the synonym question the plugin cannot answer
from the group it is handed, because an imported synonym's right-hand side
lives in an interface.
-}
module Test.DesignCtx (DesignCtx, ImportedRst) where

import Clash.Explicit.Prelude
import Clash.Signal (HiddenReset)

import Clash.CircuitContext (HasCircuitContext)

{- | The shape a design-wide context alias actually takes in a real project:
the domain constraints the module needs, plus tracing.
-}
type DesignCtx dom = (KnownDomain dom, HasCircuitContext)

{- | A higher-rank type behind an imported synonym: the argument brings its
own reset. See @topRankNImportedSyn@ in "Test.Downstream".
-}
type ImportedRst dom r = ((HiddenReset dom) => r) -> r
