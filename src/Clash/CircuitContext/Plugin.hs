{- | The clash-circuit-context GHC plugin. Enable with

> ghc-options: -fplugin=Clash.CircuitContext.Plugin

(package-wide) or per module via @OPTIONS_GHC@. Two cooperating actions:

* a renamer-stage rewrite ("Clash.CircuitContext.Plugin.Rename") that instruments
  qualifying declarations, and
* a typechecker plugin ("Clash.CircuitContext.Plugin.Oracle") that reduces the
  'Clash.CircuitContext.Auto.CanTrace' family, deciding traceability per type.
-}
module Clash.CircuitContext.Plugin (plugin) where

import qualified GHC.Plugins as GHC
import qualified GHC.TcPlugin.API as API

import Clash.CircuitContext.Plugin.Oracle (oracle)
import Clash.CircuitContext.Plugin.Rename (renamePass)

plugin :: GHC.Plugin
plugin =
  GHC.defaultPlugin
    { GHC.renamedResultAction = renamePass
    , GHC.tcPlugin = \_opts -> Just (API.mkTcPlugin oracle)
    , GHC.pluginRecompile = GHC.purePlugin
    }
