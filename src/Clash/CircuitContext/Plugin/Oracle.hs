{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The typechecker-plugin half: decides the 'Clash.CircuitContext.Auto.CanTrace'
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

A conservative no is the mechanism that lets an untraceable payload degrade
to identity instead of failing the build — and also the reason a wire can go
missing with nothing said about it. So the 'No' carries the requirement it
got stuck on ('Decision'), and
@-fplugin-opt=Clash.CircuitContext.Plugin:diagnostics@ reports both: the type
that will not be traced, and what would have to hold for it to be.
-}
module Clash.CircuitContext.Plugin.Oracle (oracle) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import System.IO (hPutStrLn, stderr)

import qualified GHC.Builtin.Names as GHC
import qualified GHC.Builtin.Types as GHC
import qualified GHC.Builtin.Types.Literals as GHC (typeNatTyCons)
import qualified GHC.Core.InstEnv as GHC
import qualified GHC.Core.Predicate as GHC
import qualified GHC.Core.TyCo.FVs as GHC
import qualified GHC.Core.TyCon as GHC
import qualified GHC.Core.Type as GHC
import qualified GHC.Iface.Load as GHC (loadSysInterface)
import qualified GHC.Plugins as GHC (
  getOccString,
  idType,
  moduleName,
  moduleNameString,
  nameModule,
  nameModule_maybe,
  ppr,
  text,
 )
import qualified GHC.Tc.Types.Constraint as GHC (ctLocSpan)
import qualified GHC.Tc.Utils.Monad as GHC (initIfaceTcRn)
import qualified GHC.Tc.Utils.TcType as GHC
import qualified GHC.TcPlugin.API as API
import qualified GHC.TcPlugin.API.Internal as APIInternal (unsafeLiftTcM)
import qualified GHC.Types.Name as GHC (getName)
import qualified GHC.Types.Unique.FM as GHC
import qualified GHC.Utils.Outputable as GHC (
  SDocContext (sdocStyle, sdocSuppressUniques),
  defaultSDocContext,
  defaultUserStyle,
  renderWithContext,
 )

import Clash.CircuitContext.Plugin.Options (Options (..))

data OracleEnv = OracleEnv
  { canTraceTyCon :: API.TyCon
  , canProbeTyCon :: API.TyCon
  , autoTraceClass :: API.Class
  , autoProbeClass :: API.Class
  , traceableClass :: API.Class
  , probeableClass :: API.Class
  , canDescribeTyCon :: API.TyCon
  , autoDescribeClass :: API.Class
  , describableClass :: API.Class
  , reported :: Maybe (IORef (Set.Set String))
  {- ^ Under @diagnostics@, the messages already on stderr. The same query
  arrives many times (once per constraint-solving round, per occurrence), and
  a decision repeated is not news.
  -}
  }

oracle :: Options -> API.TcPlugin
oracle opts =
  API.TcPlugin
    { API.tcPluginInit = initEnv opts
    , API.tcPluginSolve = solveStuck
    , API.tcPluginRewrite = rewriters
    , API.tcPluginPostTc = \_ -> pure ()
    , API.tcPluginShutdown = \_ -> pure ()
    }

initEnv :: Options -> API.TcPluginM 'API.Init OracleEnv
initEnv opts = do
  let modName = API.mkModuleName "Clash.CircuitContext.Auto"
  pkgQual <- API.resolveImport modName Nothing
  found <- API.findImportedModule modName pkgQual
  case found of
    API.Found _ md -> do
      -- Force the FULL interface (including its instances) into the EPS:
      -- instrumented modules never import Clash.CircuitContext.Auto, so without
      -- this the first oracle query can run before the Traceable/Probeable
      -- instances are visible and wrongly decide 'False (observed).
      forceInterface md
      -- Same hazard one package over: the Traceable Signal instance requires
      -- clash-shockwaves' Waveform per payload, and a module that never
      -- mentions clash-shockwaves never loads its instances — every stock
      -- payload (BitVector, Index, …) would silently stop tracing there
      -- (observed on bittide's Prbs). The module cannot be resolved by name
      -- from an arbitrary unit (only DIRECT dependencies are), so read the
      -- class off 'waveformClassWitness' and force its home interface.
      witId <-
        API.tcLookupId =<< API.lookupOrig md (API.mkVarOcc "waveformClassWitness")
      let (_, theta, _) = GHC.tcSplitSigmaTy (GHC.idType witId)
      case [cls | pred0 <- theta, GHC.ClassPred cls _ <- [GHC.classifyPredType pred0]] of
        (cls : _) -> forceInterface (GHC.nameModule (GHC.getName cls))
        [] ->
          error
            "Clash.CircuitContext.Plugin: waveformClassWitness carries no class constraint"
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
        <*> tc "CanDescribe"
        <*> cl "AutoDescribe"
        <*> cl "Describable"
        <*> if optDiagnostics opts
          then Just <$> liftIO (newIORef Set.empty)
          else pure Nothing
    _ ->
      error
        "Clash.CircuitContext.Plugin: Clash.CircuitContext.Auto not found (missing clash-circuit-context dependency?)"
 where
  forceInterface md =
    ()
      <$ APIInternal.unsafeLiftTcM
        ( GHC.initIfaceTcRn
            (GHC.loadSysInterface (GHC.text "clash-circuit-context") md)
        )

--------------------------------------------------------------------------------
-- Rewriter stage: ground types only (givens are NOT available here)
--------------------------------------------------------------------------------

rewriters :: OracleEnv -> GHC.UniqFM API.TyCon API.TcPluginRewriter
rewriters env =
  GHC.listToUFM
    [ (canTraceTyCon env, rewriteGround env (canTraceTyCon env) (traceableClass env))
    , (canProbeTyCon env, rewriteGround env (canProbeTyCon env) (probeableClass env))
    ,
      ( canDescribeTyCon env
      , rewriteGround env (canDescribeTyCon env) (describableClass env)
      )
    ]

rewriteGround ::
  OracleEnv ->
  API.TyCon ->
  API.Class ->
  [API.Ct] ->
  [API.Type] ->
  API.TcPluginM 'API.Rewrite API.TcPluginRewriteResult
rewriteGround env fam cls _givens [t]
  | GHC.noFreeVarsOfType t = do
      decision <-
        solvablePred API.getInstEnvs (totalClassesOf env) [] fuel0 (GHC.mkClassPred cls [t])
      case decision of
        Undecided -> pure API.TcPluginNoRewrite
        _ -> do
          loc <- API.rewriteEnvCtLoc <$> API.askRewriteEnv
          reportDeclined env loc cls t decision
          pure $
            API.TcPluginRewriteTo
              ( API.mkTyFamAppReduction
                  "clash-circuit-context"
                  GHC.Nominal
                  []
                  fam
                  [t]
                  (boolTy (isYes decision))
              )
              []
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
              solvablePred
                API.getInstEnvs
                (totalClassesOf env)
                givenPreds
                fuel0
                (GHC.mkClassPred target [t])
            case decision of
              Undecided -> pure Nothing
              _ -> do
                reportDeclined env (API.ctLoc ct) target t decision
                pure (Just (ct, cls, t, isYes decision))
      _ -> pure Nothing

  dispatch cls famTc
    | cls == autoTraceClass env
    , famTc == canTraceTyCon env =
        Just (traceableClass env)
    | cls == autoProbeClass env
    , famTc == canProbeTyCon env =
        Just (probeableClass env)
    | cls == autoDescribeClass env
    , famTc == canDescribeTyCon env =
        Just (describableClass env)
    | otherwise = Nothing

--------------------------------------------------------------------------------
-- Diagnostics
--------------------------------------------------------------------------------

{- | Report a binding that will NOT be traced (resp. probed), and the
requirement that stopped it.

Only under @diagnostics@, and only for definite decisions: an 'Undecided' is
a round the solver will come back from, not an answer. This is the counter to
the silent-skip contract — @traceSignalC@ takes 'Traceable' as a real
constraint and so fails to compile on a payload it cannot describe, but an
AUTO-traced binding degrades to identity, which is exactly what makes a wire
go missing with nothing said. Every message names the type and what would
have to hold for it, so the output reads as a list of what to write an
instance for (or to name with an explicit 'traceSignalC').

Deliberately verbose: it lists every untraced binding, not only the
surprising ones. Which binding is surprising is what the reader knows and
this plugin does not.
-}
reportDeclined ::
  (API.MonadTcPlugin m, MonadIO m) =>
  OracleEnv ->
  API.CtLoc ->
  -- | 'Traceable' or 'Probeable' — the requirement that was asked for
  API.Class ->
  -- | the payload type
  API.Type ->
  Decision ->
  m ()
reportDeclined env loc cls t decision = case (reported env, decision) of
  (Just seen, No blame) -> do
    let
      msg =
        render (GHC.ctLocSpan loc)
          <> ": "
          <> outcome (GHC.getOccString (GHC.getName cls))
          <> ": "
          <> render t
          <> "\n    blocked on: "
          <> render blame
    fresh <-
      liftIO $
        atomicModifyIORef' seen $ \s ->
          (Set.insert msg s, not (Set.member msg s))
    when fresh $
      liftIO (hPutStrLn stderr ("clash-circuit-context plugin: " <> msg))
  _ -> pure ()
 where
  -- A declined 'Describable' is the mildest of the three: the binding still
  -- records, it just renders as bits. Say which it is.
  outcome "Probeable" = "not probed"
  outcome "Describable" = "probed as raw bits (no ADT description)"
  outcome _ = "not traced"

  -- Uniques suppressed: a skolem printed as @dom_ajlh[sk:1]@ is noise in a
  -- message whose reader is looking at the source line it names.
  render :: (API.Outputable a) => a -> String
  render =
    GHC.renderWithContext
      GHC.defaultSDocContext
        { GHC.sdocSuppressUniques = True
        , -- User style, not the default DUMP style: a dump prints a skolem's
          -- typechecker details (@dom[sk:1]@) beside it.
          GHC.sdocStyle = GHC.defaultUserStyle
        }
      . GHC.ppr

--------------------------------------------------------------------------------
-- Solvability approximation
--------------------------------------------------------------------------------

{- | The flag-dispatch classes, whose constraints are total (see the guard in
'solvablePred').
-}
totalClassesOf :: OracleEnv -> [API.Class]
totalClassesOf env =
  [autoTraceClass env, autoProbeClass env, autoDescribeClass env]

fuel0 :: Int
fuel0 = 20

boolTy :: Bool -> API.Type
boolTy True = GHC.mkTyConTy GHC.promotedTrueDataCon
boolTy False = GHC.mkTyConTy GHC.promotedFalseDataCon

{- | The oracle's answer about one requirement.

'No' carries the requirement it got stuck on, which is the whole difference
between "this will not be traced" and a message a reader can act on: for
@Signal dom (Maybe (Packet dimX dimY))@ the blame is @Waveform (Packet dimX
dimY)@ (write the instance), for a size-polymorphic payload it is the
@KnownNat@ or @1 <= n@ the oracle cannot discharge (name it with an explicit
'Clash.CircuitContext.Core.traceSignalC' instead).
-}
data Decision
  = Yes
  | -- | with the requirement to blame
    No API.PredType
  | {- | metavariables present, or a skolem-involving negative in a
    givens-free round (see the module header)
    -}
    Undecided

isYes :: Decision -> Bool
isYes Yes = True
isYes _ = False

{- | Approximate whether @pred@ is solvable given the in-scope given
predicates.

Monadic over the instance-environment fetch, re-read at EVERY lookup:
classifying a matched instance's context forces lazy interface loading as a
side effect (e.g. @instance BitPack Int@'s home module), so a single
snapshot taken up front can miss instances that are visible by the time the
recursion needs them — observed as a wrong ''False' on the module's very
first oracle query.
-}
solvablePred ::
  (Monad m) =>
  m GHC.InstEnvs ->
  {- | The flag-dispatch classes (@AutoTrace@\/@AutoProbe@), which are TOTAL:
  see the note at the guard that uses this.
  -}
  [API.Class] ->
  [API.PredType] ->
  Int ->
  API.PredType ->
  m Decision
solvablePred getEnvs totalClasses givenPreds = go
 where
  go fuel pred0
    | fuel <= 0 = pure (No pred0)
    | hasMetas pred0 = pure Undecided
    | any (GHC.eqType pred0) givenPreds = pure Yes
    | otherwise = case GHC.classifyPredType pred0 of
        GHC.ClassPred cls args
          -- @AutoTrace (CanTrace c) c@ is solvable for EVERY c: the
          -- 'AutoTrace' ''False' instance matches any type with no context,
          -- and the ''True' one applies exactly when this oracle answers
          -- yes. So answer yes directly.
          --
          -- Recursing instead would answer a conservative NO, because the
          -- flag is a stuck family application and both 'AutoTrace'
          -- instances then look like candidates — not the unique match
          -- 'byInstance' requires. That matters because composite
          -- 'Traceable' instances carry their fields' requirements in this
          -- form, precisely so an untraceable field degrades on its own:
          -- a conservative NO here would make every tuple and record
          -- untraceable.
          | cls `elem` totalClasses -> pure Yes
          | GHC.getName cls == GHC.knownNatClassName
          , [n] <- args ->
              knownNat fuel cls n
          | GHC.getName cls == GHC.knownSymbolClassName ->
              pure (litOr (all (isJust . GHC.isStrLitTy) args) pred0)
          | GHC.getName cls == GHC.typeableClassName ->
              pure (litOr (GHC.noFreeVarsOfType pred0) pred0)
          | otherwise -> byInstance fuel cls args pred0
        _ -> pure (negativeOrDefer pred0)

  litOr True _ = Yes
  litOr False pred0 = negativeOrDefer pred0

  -- @KnownNat n@: a literal is known; a given is known (caught by the
  -- eqType check above on recursion); and an application of a type-level
  -- arithmetic function is known iff all its operands are — mirroring what
  -- ghc-typelits-knownnat/-extra solve in the instrumented module. (Code
  -- whose types CONTAIN such arithmetic constraints requires those solver
  -- plugins to typecheck at all, so predicting their success adds no new
  -- requirement.) This is what lets a @Signal dom (WishboneM2S (30 - CLog 2
  -- (n + 1)) 4)@ port trace inside a bus-width-polymorphic component.
  knownNat fuel cls n0
    | Just _ <- GHC.isNumLitTy n = pure Yes
    | Just (tc, tcArgs) <- GHC.splitTyConApp_maybe n
    , isNatArithTyCon tc
    , not (null tcArgs) =
        conj <$> traverse (go (fuel - 1) . mkKN) tcArgs
    | otherwise = pure (negativeOrDefer (mkKN n))
   where
    n = GHC.expandTypeSynonyms n0
    mkKN t = GHC.mkClassPred cls [t]

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

  -- All must hold; one definite no wins over undecided, and the FIRST no is
  -- the one reported: it is the requirement written leftmost in the
  -- instance context, which is where a reader looks first.
  conj ds
    | (blame : _) <- [p | No p <- ds] = No blame
    | all isYes ds = Yes
    | otherwise = Undecided

  -- A negative involving skolems is only trustworthy when givens were
  -- actually present (simplifyInfer rounds have none).
  negativeOrDefer pred0
    | hasSkolems pred0, null givenPreds = Undecided
    | otherwise = No pred0

{- | Type-level Nat arithmetic whose result is 'GHC.TypeNats.KnownNat' when
its operands are: GHC's built-in @+ - * ^ Div Mod Log2@ families (compared
by 'TyCon' identity) plus the @ghc-typelits-extra@ families (by name — they
live in a user package). The corresponding solver plugins
(@ghc-typelits-knownnat@ with the @-extra@ instances) construct the actual
evidence during real constraint solving; the oracle only predicts them.
-}
isNatArithTyCon :: API.TyCon -> Bool
isNatArithTyCon tc =
  tc `elem` GHC.typeNatTyCons
    || (inMod "GHC.TypeLits.Extra" && occ `elem` extraFams)
 where
  occ = GHC.getOccString tc
  inMod m =
    case GHC.nameModule_maybe (GHC.getName tc) of
      Just md -> GHC.moduleNameString (GHC.moduleName md) == m
      Nothing -> False
  extraFams =
    ["Div", "Mod", "DivRU", "DivMod", "CLog", "FLog", "Log", "GCD", "LCM", "Max", "Min"]

hasMetas :: API.PredType -> Bool
hasMetas = GHC.anyFreeVarsOfType (\v -> GHC.isTyVar v && GHC.isMetaTyVar v)

hasSkolems :: API.PredType -> Bool
hasSkolems = GHC.anyFreeVarsOfType (\v -> GHC.isTyVar v && not (GHC.isMetaTyVar v))
