{-# LANGUAGE LambdaCase #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The closed local bindings the renamer leaves alone, reported once the type
checker is done with them.

The renamer does not record a /closed/ local binding (one that uses no local
variable) without a type signature: GHC may generalise such a binding, and
the recording would stop that (see @closedBind@ in
"Clash.CircuitContext.Plugin.Rename"). The renamer cannot see types, so it
cannot tell a polymorphic helper (@noWrite = pure Nothing@, used at two
types) from a wire (@pulse = riseEvery clockGen resetGen enableGen d4@).

After type checking, GHC's answer is in the binder's type. A binder whose
type has no @forall@ and no constraint was not generalised, so recording it
would have changed nothing about its type: it is a wire that was lost, and
it is reported, with the signature that records it. A binder GHC did
generalise has no single value to record (it is a function of its types,
like a binding with a higher-rank type), and nothing is said about it.
Neither is a binding no recorder could take (a plain value in a traced
component, a function, a type declared @NonTranslatable@): the same rule as
the oracle's (see @neverRecordable@ in "Clash.CircuitContext.Plugin.Oracle").

Only trace mode is reported. A closed binding in a traced component can be a
signal that changes from cycle to cycle (@riseEvery clockGen …@). In probe
mode the binding is a value in a step function, and a closed one cannot
depend on the state or the input: its probe would be the same value in every
cycle (@limit = 5 :: Index 8@, @start = minBound@), so no signal is lost.
-}
module Clash.CircuitContext.Plugin.Closed (
  noteClosedSkip,
  reportClosedSkips,
) where

import Control.Monad (forM_, unless, when)
import qualified Data.Generics as SYB
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)

import GHC.Hs
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC
import qualified GHC.Tc.Utils.Env as GHC (tcLookupTyCon)
import qualified GHC.Tc.Utils.Monad as GHC (getGblEnv)
import qualified GHC.Tc.Utils.TcType as GHC (tcSplitSigmaTy, tyCoVarsOfTypeList)
import GHC.Types.SrcLoc (GenLocated (..))

import qualified Clash.CircuitContext.Plugin.Diagnostics as Diag
import Clash.CircuitContext.Plugin.Names (AbiNames (..), lookupAbiNames)
import Clash.CircuitContext.Plugin.Oracle (recordableShape)

{- | A closed binder the renamer left alone in trace mode: its name, where,
and the type variables a local signature can name there (see @ctxScoped@ in
"Clash.CircuitContext.Plugin.Rename").
-}
data Skip = Skip GHC.Name GHC.SrcSpan [GHC.OccName]

{- | The skips per module, from the renamer (which runs once per declaration
group) to 'reportClosedSkips' (once per module). Modules compiled in
parallel each have their own entry.
-}
skips :: IORef (Map.Map GHC.Module [Skip])
skips = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE skips #-}

{- | Remember a closed binder the renamer left alone in this module, in trace
mode (see the module header for why not in probe mode).
-}
noteClosedSkip :: GHC.Name -> GHC.SrcSpan -> [GHC.OccName] -> GHC.TcM ()
noteClosedSkip nm spn scoped = do
  md <- GHC.tcg_mod <$> GHC.getGblEnv
  GHC.liftIO $
    atomicModifyIORef' skips (\m -> (Map.insertWith (<>) md [Skip nm spn scoped] m, ()))

{- | Report the closed binders of this module that GHC did not generalise
and that a recorder would take (see the module header).
-}
reportClosedSkips ::
  [GHC.CommandLineOption] -> GHC.ModSummary -> GHC.TcGblEnv -> GHC.TcM GHC.TcGblEnv
reportClosedSkips _ _ env = do
  mine <-
    GHC.liftIO $
      atomicModifyIORef'
        skips
        (\m -> (Map.delete (GHC.tcg_mod env) m, Map.findWithDefault [] (GHC.tcg_mod env) m))
  unless (null mine) $
    lookupAbiNames >>= \case
      Nothing -> pure ()
      Just abi -> do
        nonTranslatableTc <- GHC.tcLookupTyCon (abiNonTranslatable abi)
        let types = binderTypes (GHC.tcg_binds env)
        -- A binder is noted once per run of the renamer pass (twice when the
        -- plugin is enabled twice); 'Diag.report' says each message once.
        forM_ mine $ \(Skip nm spn scoped) ->
          forM_ (Map.lookup nm types) $ \ty -> do
            let (tvs, theta, rho) = GHC.tcSplitSigmaTy ty
            when (null tvs && null theta && not (GHC.isFunTy rho)) $ do
              recordable <- recordableShape nonTranslatableTc True ty
              when recordable $
                Diag.report spn Diag.Untraced $
                  [ "not traced: "
                      <> GHC.getOccString nm
                      <> " :: "
                      <> Diag.renderPlain ty
                  , "It uses no local variable and has no type signature, and such a"
                      <> " binding is left alone: GHC can generalise it, and recording"
                      <> " it would stop that."
                  , "GHC did not generalise this one. Give it its type signature to"
                      <> " record it: "
                      <> GHC.getOccString nm
                      <> " :: "
                      <> Diag.renderPlain ty
                  ]
                    -- Only for a type variable the signature cannot name as
                    -- it is: one the enclosing signature does not bind with an
                    -- explicit forall under ScopedTypeVariables (by name: the
                    -- type checker may have renumbered it).
                    <> [ "The type names type variables of the enclosing signature: bind"
                           <> " them there with an explicit forall (ScopedTypeVariables)."
                       | any
                           ((`notElem` scoped) . GHC.occName)
                           (GHC.tyCoVarsOfTypeList ty)
                       ]
  pure env

{- | The final type of every local binder, by name: a generalised binder's
(its poly 'GHC.Id' in an @AbsBinds@ export), and a binder that was not
generalised at all.
-}
binderTypes :: LHsBinds GhcTc -> Map.Map GHC.Name GHC.Type
binderTypes binds =
  Map.fromList $
    [(GHC.getName i, GHC.idType i) | ABE{abe_poly = i} <- SYB.listify isExport binds]
      <> [ (GHC.getName i, GHC.idType i)
         | FunBind{fun_id = L _ i} <- SYB.listify isFunBind binds
         ]
 where
  isExport :: ABExport -> Bool
  isExport _ = True
  isFunBind :: HsBindLR GhcTc GhcTc -> Bool
  isFunBind FunBind{} = True
  isFunBind _ = False
