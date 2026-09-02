{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
missing with nothing said about it. Two things separate those:

* 'Decision' distinguishes a PROOF ('NoInstance': the instance environment
  has no match) from the oracle GIVING UP ('GaveUp': fuel exhausted, a
  constraint that is not a class, an ambiguous instance match). Both fall
  back to identity — they must — but only the first is an answer. The second
  is this approximation admitting it may be wrong, which is a different thing
  to tell a reader, and a different thing to promote to an error.
* both carry the requirement they stopped at, so the message names what would
  have to hold.

They report in separate warning categories; see
"Clash.CircuitContext.Plugin.Diagnostics".
-}
module Clash.CircuitContext.Plugin.Oracle (oracle) where

import Data.Maybe (isJust)

import qualified GHC.Builtin.Names as GHC
import qualified GHC.Builtin.Types as GHC
import qualified GHC.Builtin.Types.Literals as GHC (typeNatTyCons)
import qualified GHC.Core.InstEnv as GHC
import qualified GHC.Core.Predicate as GHC
import qualified GHC.Core.Reduction as GHC (reductionReducedType)
import qualified GHC.Core.TyCo.FVs as GHC
import qualified GHC.Core.TyCon as GHC
import qualified GHC.Core.Type as GHC
import qualified GHC.Data.Strict as Strict
import qualified GHC.Iface.Load as GHC (loadSysInterface)
import qualified GHC.Plugins as GHC (
  getOccString,
  idType,
  moduleName,
  moduleNameString,
  nameModule,
  nameModule_maybe,
  text,
 )
import qualified GHC.Tc.Types.Constraint as GHC (ctLocSpan)
import qualified GHC.Tc.Utils.Monad as GHC (initIfaceTcRn)
import qualified GHC.Tc.Utils.TcType as GHC
import qualified GHC.TcPlugin.API as API
import qualified GHC.TcPlugin.API.Internal as APIInternal (unsafeLiftTcM)
import qualified GHC.Types.Name as GHC (getName)
import qualified GHC.Types.SrcLoc as GHC (SrcSpan (RealSrcSpan))
import qualified GHC.Types.Unique.FM as GHC

import qualified Clash.CircuitContext.Plugin.Diagnostics as Diag

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
        solvablePred
          API.getInstEnvs
          reduceFamily
          (totalClassesOf env)
          []
          fuel0
          (GHC.mkClassPred cls [t])
      case decision of
        Defer -> pure API.TcPluginNoRewrite
        _ -> do
          loc <- API.rewriteEnvCtLoc <$> API.askRewriteEnv
          reportDeclined loc cls t decision
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
                reduceFamily
                (totalClassesOf env)
                givenPreds
                fuel0
                (GHC.mkClassPred target [t])
            case decision of
              Defer -> pure Nothing
              _ -> do
                reportDeclined (API.ctLoc ct) target t decision
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

Two categories, because the reader needs to know which of these happened:

* @x-circuit-context-untraced@ — the instance environment has no match. A
  real answer, and the common one: most local bindings in a design are
  @Int@s and @Bool@s that were never going to trace.
* @x-circuit-context-undecided@ — the oracle could not decide and fell back
  to not tracing. This one may be a limitation of the approximation rather
  than a fact about the type, so it is the one worth promoting to an error
  (@-Werror=x-circuit-context-undecided@) on a design whose waveform is load
  bearing.

Both fall back to identity either way; @traceSignalC@ is the escape hatch
that takes @Traceable@ as a real constraint, so a payload it cannot describe
is a compile error rather than a hole.

'Defer' is not reported at all: it is a round the solver will come back
from, not an answer.
-}
reportDeclined ::
  forall m.
  (API.MonadTcPlugin m) =>
  API.CtLoc ->
  -- | @Traceable@, @Probeable@ or @Describable@ — what was asked for
  API.Class ->
  -- | the payload type
  API.Type ->
  Decision ->
  m ()
reportDeclined loc cls t = \case
  NoInstance blame ->
    say
      Diag.Untraced
      [ headline <> Diag.renderPlain t
      , "no instance for: " <> Diag.renderPlain blame
      ]
  GaveUp blame ->
    say
      Diag.Undecided
      [ headline <> Diag.renderPlain t
      , "the oracle could not decide: " <> Diag.renderPlain blame
      , "This may be a limit of the approximation rather than a fact about"
          <> " the type."
      , "Name the binding with traceSignalC, which takes Traceable as a real"
          <> " constraint, to find out which."
      ]
  _ -> pure ()
 where
  say :: Diag.Category -> [String] -> m ()
  say cat body =
    APIInternal.unsafeLiftTcM (Diag.report spn cat body)

  -- 'ctLocSpan' is the REAL span; the diagnostic machinery takes the sum.
  spn = GHC.RealSrcSpan (GHC.ctLocSpan loc) Strict.Nothing

  -- A declined @Describable@ is the mildest of the three: the binding still
  -- records, it just renders as bits. Say which it is.
  headline = case GHC.getOccString (GHC.getName cls) of
    "Probeable" -> "not probed: "
    "Describable" -> "probed as raw bits (no ADT description): "
    _ -> "not traced: "

--------------------------------------------------------------------------------
-- Solvability approximation
--------------------------------------------------------------------------------

{- | One type-family reduction step, as 'solvablePred' wants it: the reduced
type, or 'Nothing' where no equation matches.
-}
reduceFamily ::
  (API.MonadTcPlugin m) => API.TyCon -> [API.Type] -> m (Maybe API.Type)
reduceFamily tc args =
  fmap GHC.reductionReducedType <$> API.matchFam tc args

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

Both negative answers carry the requirement they stopped at, which is the
difference between "this will not be traced" and a message a reader can act
on: for @Signal dom (Maybe (Packet dimX dimY))@ the blame is @Waveform
(Packet dimX dimY)@ (write the instance), for a size-polymorphic payload it
is the @KnownNat@ or @1 <= n@ the oracle cannot discharge (name it with an
explicit 'Clash.CircuitContext.Core.traceSignalC' instead).

They are kept APART because they mean different things. 'NoInstance' is a
proof; 'GaveUp' is this approximation reaching its limit, and a wire lost to
it is a bug in the oracle as often as it is a fact about the design.
-}
data Decision
  = Yes
  | {- | PROVED unsolvable — the instance environment has no match for this
    requirement. An answer.
    -}
    NoInstance API.PredType
  | {- | The oracle GAVE UP on this requirement: fuel exhausted, a predicate
    that is not a class (a type family such as the @Assert@ behind
    @1 <= n@), an ambiguous instance match, a literal-only class at a
    non-literal. Falls back the same way a 'NoInstance' does, but it is not
    an answer, and saying so is the difference between a limitation the
    reader can work around and one that hides.
    -}
    GaveUp API.PredType
  | {- | Not an answer at all: metavariables present, or a skolem-involving
    negative in a givens-free round (see the module header). The solver
    will ask again.
    -}
    Defer

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
  {- | One-step type-family reduction. The oracle walks the instance
  environment itself and never runs the typechecker's solver, so without this
  a family application it cannot see through — @BitSize SomeRecord@, the
  @Assert@ behind @1 <= 4096@ — looks like a type nothing can be known about.
  Reducing first is the difference between declining a wire and tracing it.
  -}
  (API.TyCon -> [API.Type] -> m (Maybe API.Type)) ->
  {- | The flag-dispatch classes (@AutoTrace@\/@AutoProbe@), which are TOTAL:
  see the note at the guard that uses this.
  -}
  [API.Class] ->
  [API.PredType] ->
  Int ->
  API.PredType ->
  m Decision
solvablePred getEnvs reduceFam totalClasses givenPreds = go
 where
  go fuel pred0
    | fuel <= 0 = pure (gaveUp pred0)
    | hasMetas pred0 = pure Defer
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
          -- @Traceable@ instances carry their fields' requirements in this
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
        -- Not a class constraint: an equality, or a type family such as the
        -- @Assert@ behind @1 <= n@. Reduce it and look again — @1 <= 4096@
        -- normalises to the empty constraint, which is a real Yes — and only
        -- give up when it will not budge. Nothing on this path is ever a
        -- proof of failure: not understanding a constraint says nothing about
        -- whether it holds.
        _ -> do
          reduced <- normalise fuel pred0
          if GHC.eqType reduced pred0
            then pure (gaveUp pred0)
            else
              if emptyConstraint reduced
                then pure Yes
                else reblame pred0 <$> go (fuel - 1) reduced

  litOr True _ = Yes
  litOr False pred0 = gaveUp pred0

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
    -- A bare type variable with no matching given: there is genuinely no
    -- evidence for it, and no solver plugin can conjure one. THIS is the
    -- proof.
    | isJust (GHC.getTyVar_maybe n) = pure (noInstance (mkKN n))
    -- Anything else — @BitSize SomeRecord@, a family this oracle has never
    -- heard of — gets reduced and re-asked. If it still will not budge, the
    -- honest answer is that we do not know: @KnownNat@ is exactly the class
    -- the typelits solver plugins discharge, and they run in the real
    -- compile while this walk does not. Calling that "no instance" was
    -- reporting a guess as a fact (Helios F1: @KnownNat (BitSize
    -- ManticoreStatus)@ is satisfiable, and was declined as proved-absent).
    | otherwise = do
        reduced <- normalise fuel n
        if GHC.eqType reduced n
          then pure (gaveUp (mkKN n))
          else reblame (mkKN n) <$> knownNat (fuel - 1) cls reduced
   where
    n = GHC.expandTypeSynonyms n0
    mkKN t = GHC.mkClassPred cls [t]

  {- Bottom-up type-family normalisation, fuel-bounded. 'API.matchFam' takes
  ONE step and matches only against already-reduced arguments, so
  @Assert (1 <=? 4096) msg@ needs its argument reduced before the @Assert@
  equation applies at all — which is why this recurses into the arguments
  first rather than just calling matchFam at the root. -}
  normalise fuel ty
    | fuel <= 0 = pure ty
    | Just (tc, args) <- GHC.splitTyConApp_maybe ty = do
        args' <- traverse (normalise (fuel - 1)) args
        if GHC.isTypeFamilyTyCon tc
          then do
            step <- reduceFam tc args'
            case step of
              Just ty' -> normalise (fuel - 1) ty'
              Nothing -> pure (GHC.mkTyConApp tc args')
          else pure (GHC.mkTyConApp tc args')
    | otherwise = pure ty

  {- Report the requirement the DESIGNER wrote, not the normal form this
  oracle reduced it to. Normalising is an internal step; blaming
  @Assert (OrdCond (CmpNat 1 n) 'True 'True 'False) (TypeError …)@ when the
  signature says @1 <= n@ hands the reader a type they never typed. -}
  reblame orig = \case
    GaveUp _ -> gaveUp orig
    NoInstance _ -> noInstance orig
    other -> other

  -- @() :: Constraint@, what a discharged @Assert@ reduces to.
  emptyConstraint ty = case GHC.splitTyConApp_maybe ty of
    Just (tc, []) -> GHC.isCTupleTyConName (GHC.getName tc)
    _ -> False

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
        | [] <- matches -> pure (noInstance pred0)
        -- Instances exist; the oracle could not pick one. Reporting this as
        -- "no instance" would be a lie the reader cannot check.
        | otherwise -> pure (gaveUp pred0)

  -- All must hold. A proof of failure outranks a give-up (the conjunction
  -- definitely fails, and the proof is the better blame), and both outrank a
  -- defer. The FIRST of a kind is the one reported: it is the requirement
  -- written leftmost in the instance context, which is where a reader looks
  -- first.
  conj ds
    | (blame : _) <- [p | NoInstance p <- ds] = NoInstance blame
    | (blame : _) <- [p | GaveUp p <- ds] = GaveUp blame
    | all isYes ds = Yes
    | otherwise = Defer

  -- A negative involving skolems is only trustworthy when givens were
  -- actually present (simplifyInfer rounds have none).
  noInstance = negativeOr NoInstance
  gaveUp = negativeOr GaveUp

  negativeOr mk pred0
    | hasSkolems pred0, null givenPreds = Defer
    | otherwise = mk pred0

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
