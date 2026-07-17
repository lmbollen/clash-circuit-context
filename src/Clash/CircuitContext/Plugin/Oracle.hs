{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

{- | The typechecker-plugin half: decides the 'Clash.CircuitContext.Auto.CanTrace'
and @CanProbe@ oracles by approximating the solvability of @Traceable t@
(resp. @Probeable t@) at the occurrence. The plugin builds NO instance
dictionaries — it only reduces the Bool-kinded families (plus a cast for
stuck cases); ordinary instance resolution does the rest.

Two cooperating stages — spike findings:

* GHC does NOT reliably pass the enclosing implication's givens to
  type-family REWRITER plugins (empirically @[]@), so
  'API.tcPluginRewrite' only decides GROUND types (no free type variables);
  everything else defers.
* 'API.tcPluginSolve' catches the resulting stuck wanteds
  @AutoTrace (CanTrace t) t@ / @AutoProbe (CanProbe t) t@ — the solver
  stage DOES receive givens — decides from them, and solves by emitting the
  flag-substituted wanted plus a univ-cast of its evidence.
* GHC also presents these wanteds during @simplifyInfer@ of local bindings,
  BEFORE wrapping them into the enclosing implication (empty inert set):
  skolem-involving decisions are never made NEGATIVELY in a givens-free
  round — deferred instead; the real round always carries at least the
  enclosing @?circuitContext@\/@?probe@ given.

Solvability approximation (constraints-emerge style, no re-entrant solver):
direct given hit → yes; built-in classes (KnownNat\/KnownSymbol literals,
ground Typeable) decided directly; otherwise unique instance-environment
match → recurse on the instantiated context; conservative no (or defer, per
the rule above) when nothing applies.
-}
module Clash.CircuitContext.Plugin.Oracle (oracle) where

import Data.Maybe (isJust, mapMaybe)

import qualified GHC.Builtin.Names as GHC
import qualified GHC.Builtin.Types as GHC
import qualified GHC.Core.Class as GHC
import qualified GHC.Core.InstEnv as GHC
import qualified GHC.Core.Predicate as GHC
import qualified GHC.Core.TyCo.FVs as GHC
import qualified GHC.Core.TyCon as GHC
import qualified GHC.Core.Type as GHC
import qualified GHC.Plugins as GHC (Role (Nominal, Representational), text)
import qualified GHC.Tc.Utils.TcType as GHC
import qualified GHC.TcPlugin.API as API
import qualified GHC.TcPlugin.API.Internal as APIInternal (unsafeLiftTcM)
import qualified GHC.Types.Name as GHC (getName)
import qualified GHC.Types.Unique.FM as GHC
import qualified GHC.Iface.Load as GHC (loadSysInterface)
import qualified GHC.Tc.Utils.Monad as GHC (initIfaceTcRn)
import qualified GHC.Types.Var as GHC (isTyVar)

data OracleEnv = OracleEnv
  { canTraceTyCon :: API.TyCon
  , canProbeTyCon :: API.TyCon
  , autoTraceClass :: API.Class
  , autoProbeClass :: API.Class
  , traceableClass :: API.Class
  , probeableClass :: API.Class
  }

oracle :: API.TcPlugin
oracle =
  API.TcPlugin
    { API.tcPluginInit = initEnv
    , API.tcPluginSolve = solveStuck
    , API.tcPluginRewrite = rewriters
    , API.tcPluginPostTc = \_ -> pure ()
    , API.tcPluginShutdown = \_ -> pure ()
    }

initEnv :: API.TcPluginM 'API.Init OracleEnv
initEnv = do
  let modName = API.mkModuleName "Clash.CircuitContext.Auto"
  pkgQual <- API.resolveImport modName Nothing
  found <- API.findImportedModule modName pkgQual
  case found of
    API.Found _ md -> do
      -- Force the FULL interface (including its instances) into the EPS:
      -- instrumented modules never import Clash.CircuitContext.Auto, so without
      -- this the first oracle query can run before the Traceable/Probeable
      -- instances are visible and wrongly decide 'False (observed).
      _ <-
        APIInternal.unsafeLiftTcM
          ( GHC.initIfaceTcRn
              (GHC.loadSysInterface (GHC.text "clash-circuit-context") md)
          )
      let
        tc nm = API.tcLookupTyCon =<< API.lookupOrig md (API.mkTcOcc nm)
        cl nm = API.tcLookupClass =<< API.lookupOrig md (API.mkTcOcc nm)
      OracleEnv
        <$> tc "CanTrace"
        <*> tc "CanProbe"
        <*> cl "AutoTrace"
        <*> cl "AutoProbe"
        <*> cl "Traceable"
        <*> cl "Probeable"
    _ ->
      error
        "Clash.CircuitContext.Plugin: Clash.CircuitContext.Auto not found (missing clash-circuit-context dependency?)"

--------------------------------------------------------------------------------
-- Rewriter stage: ground types only (givens are NOT available here)
--------------------------------------------------------------------------------

rewriters :: OracleEnv -> GHC.UniqFM API.TyCon API.TcPluginRewriter
rewriters env =
  GHC.listToUFM
    [ (canTraceTyCon env, rewriteGround env (canTraceTyCon env) (traceableClass env))
    , (canProbeTyCon env, rewriteGround env (canProbeTyCon env) (probeableClass env))
    ]

rewriteGround ::
  OracleEnv ->
  API.TyCon ->
  API.Class ->
  [API.Ct] ->
  [API.Type] ->
  API.TcPluginM 'API.Rewrite API.TcPluginRewriteResult
rewriteGround _env fam cls _givens [t]
  | GHC.noFreeVarsOfType t = do
      decision <- solvablePred API.getInstEnvs [] fuel0 (GHC.mkClassPred cls [t])
      case decision of
        Just b ->
          pure
            $ API.TcPluginRewriteTo
              (API.mkTyFamAppReduction "clash-circuit-context" GHC.Nominal [] fam [t] (boolTy b))
              []
        Nothing -> pure API.TcPluginNoRewrite
rewriteGround _ _ _ _ _ = pure API.TcPluginNoRewrite

--------------------------------------------------------------------------------
-- Solver stage: stuck Auto{Trace,Probe} wanteds (givens available)
--------------------------------------------------------------------------------

solveStuck :: OracleEnv -> API.TcPluginSolver
solveStuck env givens wanteds = do
  let givenPreds = map API.ctPred givens
  stucks <- traverse (stuck givenPreds) wanteds
  results <- traverse solve1 [x | Just x <- stucks]
  pure (API.TcPluginOk (map fst results) (map snd results))
 where
  solve1 (ct, cls, t, b) = do
    let
      predOld = API.ctPred ct
      predNew = GHC.mkClassPred cls [boolTy b, t]
    nw <- API.newWanted (API.ctLoc ct) predNew
    let
      co = API.mkPluginUnivCo "clash-circuit-context" GHC.Representational [] predNew predOld
      ev = API.EvExpr (API.evCast (API.ctEvExpr nw) co)
    pure ((ev, ct), API.mkNonCanonical nw)

  -- Wanteds of shape @Auto{Trace,Probe} (Can{Trace,Probe} t) t@ whose flag
  -- is still stuck, paired with a decision for them.
  stuck givenPreds ct =
    case GHC.classifyPredType (API.ctPred ct) of
      GHC.ClassPred cls [flagTy, t]
        | Just (famTc, [t']) <- GHC.splitTyConApp_maybe flagTy
        , t' `GHC.eqType` t
        , Just target <- dispatch cls famTc -> do
            decision <-
              solvablePred API.getInstEnvs givenPreds fuel0 (GHC.mkClassPred target [t])
            pure ((,,,) ct cls t <$> decision)
      _ -> pure Nothing

  dispatch cls famTc
    | cls == autoTraceClass env, famTc == canTraceTyCon env =
        Just (traceableClass env)
    | cls == autoProbeClass env, famTc == canProbeTyCon env =
        Just (probeableClass env)
    | otherwise = Nothing

--------------------------------------------------------------------------------
-- Solvability approximation
--------------------------------------------------------------------------------

fuel0 :: Int
fuel0 = 20

boolTy :: Bool -> API.Type
boolTy True = GHC.mkTyConTy GHC.promotedTrueDataCon
boolTy False = GHC.mkTyConTy GHC.promotedFalseDataCon

{- | Approximate whether @pred@ is solvable given the in-scope given
predicates. 'Nothing' = undecided: metavariables present, or a
skolem-involving NEGATIVE in a givens-free round (see module header).

Monadic over the instance-environment fetch, re-read at EVERY lookup:
classifying a matched instance's context forces lazy interface loading as a
side effect (e.g. @instance BitPack Int@'s home module), so a single
snapshot taken up front can miss instances that are visible by the time the
recursion needs them — observed as a wrong ''False' on the module's very
first oracle query.
-}
solvablePred ::
  Monad m =>
  m GHC.InstEnvs ->
  [API.PredType] ->
  Int ->
  API.PredType ->
  m (Maybe Bool)
solvablePred getEnvs givenPreds = go
 where
  go fuel pred0
    | fuel <= 0 = pure (Just False)
    | hasMetas pred0 = pure Nothing
    | any (GHC.eqType pred0) givenPreds = pure (Just True)
    | otherwise = case GHC.classifyPredType pred0 of
        GHC.ClassPred cls args
          | GHC.getName cls == GHC.knownNatClassName ->
              pure (litOr (all (isJust . GHC.isNumLitTy) args) pred0)
          | GHC.getName cls == GHC.knownSymbolClassName ->
              pure (litOr (all (isJust . GHC.isStrLitTy) args) pred0)
          | GHC.getName cls == GHC.typeableClassName ->
              pure (litOr (GHC.noFreeVarsOfType pred0) pred0)
          | otherwise -> byInstance fuel cls args pred0
        _ -> pure (negativeOrDefer pred0)

  litOr True _ = Just True
  litOr False pred0 = negativeOrDefer pred0

  byInstance fuel cls args pred0 = do
    ienvs <- getEnvs
    case GHC.lookupInstEnv False ienvs cls args of
      (matches, _, _)
        | [(inst, dfunTys)] <- matches
        , Just tys <- sequence dfunTys -> do
            let
              (tvs, theta, _, _) = GHC.instanceSig inst
              subst = GHC.zipTvSubst tvs tys
              theta' = GHC.substTheta subst theta
            conj <$> traverse (go (fuel - 1)) theta'
        | [] <- matches -> pure (negativeOrDefer pred0)
        | otherwise -> pure (negativeOrDefer pred0) -- ambiguous match

  -- All must hold; one definite no wins over undecided.
  conj rs
    | any (== Just False) rs = Just False
    | all (== Just True) rs = Just True
    | otherwise = Nothing

  -- A negative involving skolems is only trustworthy when givens were
  -- actually present (simplifyInfer rounds have none).
  negativeOrDefer pred0
    | hasSkolems pred0, null givenPreds = Nothing
    | otherwise = Just False

hasMetas :: API.PredType -> Bool
hasMetas = GHC.anyFreeVarsOfType (\v -> GHC.isTyVar v && GHC.isMetaTyVar v)

hasSkolems :: API.PredType -> Bool
hasSkolems = GHC.anyFreeVarsOfType (\v -> GHC.isTyVar v && not (GHC.isMetaTyVar v))
