{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The clash-circuit-context GHC plugin. Enable with

> ghc-options: -fplugin=Clash.CircuitContext.Plugin

(package-wide) or per module via @OPTIONS_GHC@ — but not BOTH: the renamer
pass then runs twice, which used to nest every component wrap in itself
(@switch.switch@). It is now idempotent, so the second pass is a no-op.

Two cooperating actions:

* a renamer-stage rewrite ("Clash.CircuitContext.Plugin.Rename") that instruments
  qualifying declarations, and
* a typechecker plugin ("Clash.CircuitContext.Plugin.Oracle") that reduces the
  'Clash.CircuitContext.Auto.CanTrace' family, deciding traceability per type.

After type checking, the plugin reports what only the types show: a closed
local binding that GHC did not generalise
("Clash.CircuitContext.Plugin.Closed"), and a step function without
@HasProbe@ given to @mealyProbed@ ("Clash.CircuitContext.Plugin.Steps").

Neither half takes options. What the plugin does not instrument it reports
as a real GHC warning, in a category you tune with @-Wno-@ \/ @-Werror=@;
see "Clash.CircuitContext.Plugin.Diagnostics".
-}
module Clash.CircuitContext.Plugin (plugin) where

import qualified GHC.Plugins as GHC
import qualified GHC.TcPlugin.API as API

import Clash.CircuitContext.Plugin.Closed (reportClosedSkips)
import Clash.CircuitContext.Plugin.Oracle (oracle)
import Clash.CircuitContext.Plugin.Rename (renamePass)
import Clash.CircuitContext.Plugin.Steps (reportUnprobedSteps)

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.renamedResultAction = renamePass
    , GHC.typeCheckResultAction = \opts ms env ->
        reportClosedSkips opts ms env >>= reportUnprobedSteps opts ms
    , GHC.tcPlugin = \_opts -> Just (API.mkTcPlugin oracle)
    , GHC.pluginRecompile = GHC.purePlugin
    }
