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

Both halves take the same options; see "Clash.CircuitContext.Plugin.Options".
-}
module Clash.CircuitContext.Plugin (plugin) where

import qualified GHC.Plugins as GHC
import qualified GHC.TcPlugin.API as API

import Clash.CircuitContext.Plugin.Options (parseOptions)
import Clash.CircuitContext.Plugin.Oracle (oracle)
import Clash.CircuitContext.Plugin.Rename (renamePass)

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.renamedResultAction = renamePass
    , GHC.tcPlugin = \opts -> Just (API.mkTcPlugin (oracle (fst (parseOptions opts))))
    , -- Options change what the plugin REPORTS, never what it emits, so a
      -- pure recompile check stays correct.
      GHC.pluginRecompile = GHC.purePlugin
    }
