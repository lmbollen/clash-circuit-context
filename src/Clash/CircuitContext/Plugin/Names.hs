{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Resolution of the plugin ABI 'Name's. Injection is by exact 'Name', so
instrumented modules need no imports of the Auto module — only a package
dependency on clash-circuit-context.

'GHC.lookupOrig' resolves against a name's ORIGINAL (defining) module:
@autoTrace@\/@autoProbe@ live in "Clash.CircuitContext.Auto", while the constraint
synonyms @HasCircuitContext@\/@HasProbe@ and 'component' originate in
"Clash.CircuitContext.Core".
-}
module Clash.CircuitContext.Plugin.Names (
  AbiNames (..),
  lookupAbiNames,
) where

import qualified GHC.Iface.Env as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Utils.Monad as GHC
import qualified GHC.Unit.Finder as GHC

data AbiNames = AbiNames
  { abiAutoTrace :: GHC.Name
  , abiAutoProbe :: GHC.Name
  , abiComponent :: GHC.Name
  , abiHasCircuitContext :: GHC.Name
  , abiHasProbe :: GHC.Name
  , abiNoCircuitScope :: GHC.Name
  {- ^ The DATA constructor, which is what an @ANN@ pragma's expression
  mentions.
  -}
  }

{- | 'Nothing' when the module being compiled does not depend (transitively)
on clash-circuit-context; callers should pass the program through unchanged.
-}
lookupAbiNames :: GHC.TcM (Maybe AbiNames)
lookupAbiNames = do
  hscEnv <- GHC.getTopEnv
  autoMod <- findMod hscEnv "Clash.CircuitContext.Auto"
  coreMod <- findMod hscEnv "Clash.CircuitContext.Core"
  case (autoMod, coreMod) of
    (Just am, Just cm) -> do
      autoTraceN <- GHC.lookupOrig am (GHC.mkVarOcc "autoTrace")
      autoProbeN <- GHC.lookupOrig am (GHC.mkVarOcc "autoProbe")
      componentN <- GHC.lookupOrig cm (GHC.mkVarOcc "component")
      hasCircuitContextN <- GHC.lookupOrig cm (GHC.mkTcOcc "HasCircuitContext")
      hasProbeN <- GHC.lookupOrig cm (GHC.mkTcOcc "HasProbe")
      noCircuitScopeN <- GHC.lookupOrig cm (GHC.mkDataOcc "NoCircuitScope")
      pure
        ( Just
            AbiNames
              { abiAutoTrace = autoTraceN
              , abiAutoProbe = autoProbeN
              , abiComponent = componentN
              , abiHasCircuitContext = hasCircuitContextN
              , abiHasProbe = hasProbeN
              , abiNoCircuitScope = noCircuitScopeN
              }
        )
    _ -> pure Nothing
 where
  findMod hscEnv nm = do
    found <-
      GHC.liftIO $
        GHC.findImportedModule hscEnv (GHC.mkModuleName nm) GHC.NoPkgQual
    pure $ case found of
      GHC.Found _ md -> Just md
      _ -> Nothing
