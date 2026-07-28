{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The renamer-stage rewrite ('GHC.renamedResultAction'):

* Top-level binders whose written signature carries 'HasCircuitContext' (resolved
  synonym Name or raw @?circuitContext@) are instrumented in TRACE mode; 'HasProbe'
  (@?probe@) selects PROBE mode. Local signatures win over the inherited
  mode (a @HasProbe@-annotated step function inside a traced component
  switches its subtree to probing).

* Every named local declaration @x = e@ (zero-argument 'FunBind' or
  'VarPat' 'PatBind') inside an instrumented function is rewritten to
  @x = autoTrace "x" e@ (resp. 'autoProbe'), with the binding's span copied
  onto the injected occurrence so 'GHC.Stack.HasCallStack' sees the design
  location.

* A local PATTERN binding (tuple, record, banged, …) — the dominant Clash
  idiom for multi-output circuits, @(a, b) = unbundle …@ or
  @Out{x, y} = f …@ — cannot have its right-hand side wrapped (only some
  components of the pattern may be traceable). Instead every wanted binder
  @x@ in the pattern is renamed to a fresh @x'@ and a sibling binding
  @x = autoTrace "x" x'@ is added, so all other code keeps referring to
  the traced @x@. The enclosing group is marked recursive; the extra
  binds carry precise free-variable sets, so the typechecker's dependency
  analysis re-orders them correctly.

* A top-level binder that is additionally OPAQUE (and in trace mode) is a
  COMPONENT: its body becomes @component "f" (let <where-binds> in body)@.
  The where→let move is what scopes the bindings under the pushed
  hierarchy segment — 'HasCircuitContext'-polymorphic where-bindings would
  otherwise discharge @?circuitContext@ from the enclosing given. Guarded equations
  are case-encoded inside the wrap when self-exhaustive (final alternative
  unguarded or @otherwise@) or when they are the last equation; only
  non-final fall-through guard sets are skipped, with a warning (their
  bindings still trace, at the enclosing scope).

* Opt-outs: binder names starting with @_@; compiler-generated bindings are
  skipped via their non-real spans; per module, don't enable the plugin.

Supports GHC 9.6 and 9.10 (CPP at the few AST churn points).
-}
module Clash.CircuitContext.Plugin.Rename (renamePass) where

import Control.Monad (unless)
import qualified Data.Generics as SYB
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import System.IO (hPutStrLn, stderr)

import qualified GHC.Builtin.Names as GHC (otherwiseIdName)
import qualified GHC.Data.Bag as GHC
import GHC.Hs
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC
import GHC.Types.Basic (Origin)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import GHC.Types.Unique.Supply (MonadUnique (getUniqueM))

import Clash.CircuitContext.Plugin.Names (AbiNames (..), lookupAbiNames)

data Mode = TraceMode | ProbeMode
  deriving (Eq)

renamePass ::
  [GHC.CommandLineOption] ->
  GHC.TcGblEnv ->
  HsGroup GhcRn ->
  GHC.TcM (GHC.TcGblEnv, HsGroup GhcRn)
renamePass _opts env grp =
  lookupAbiNames >>= \case
    Nothing -> pure (env, grp)
    Just abi -> do
      (grp', warnings) <- rewriteGroup abi grp
      unless (null warnings) $
        GHC.liftIO $
          mapM_ (hPutStrLn stderr . ("clash-circuit-context plugin: " <>)) warnings
      pure (env, grp')

rewriteGroup :: AbiNames -> HsGroup GhcRn -> GHC.TcM (HsGroup GhcRn, [String])
rewriteGroup abi grp = case hs_valds grp of
  XValBindsLR (NValBinds groups sigs) -> do
    let
      modes = modesFromSigs abi sigs
      opaques =
        [ n
        | L _ (InlineSig _ (L _ n) prag) <- sigs
        , isOpaqueSpec (GHC.inl_inline prag)
        ]
    results <-
      mapM
        ( \(r, bs) ->
            (,) r <$> mapM (onTopBind abi modes opaques) (GHC.bagToList bs)
        )
        groups
    let
      groups' = [(r, GHC.listToBag (map fst prs)) | (r, prs) <- results]
      warnings = concat [concatMap snd prs | (_, prs) <- results]
    pure (grp{hs_valds = XValBindsLR (NValBinds groups' sigs)}, warnings)
  _ -> pure (grp, [])

isOpaqueSpec :: GHC.InlineSpec -> Bool
isOpaqueSpec GHC.Opaque{} = True
isOpaqueSpec _ = False

--------------------------------------------------------------------------------
-- Signature classification
--------------------------------------------------------------------------------

modesFromSigs :: AbiNames -> [LSig GhcRn] -> [(GHC.Name, Mode)]
modesFromSigs abi sigs =
  [ (n, m)
  | L _ (TypeSig _ ns sigTy) <- sigs
  , Just m <- [sigMode abi sigTy]
  , L _ n <- ns
  ]

{- | Mode from a signature's context spine; probe wins when both appear.
Only the spine counts: a nested-rank occurrence (like 'component'\'s own
@(HasCircuitContext => r)@ argument) does not bring the parameter into scope.
-}
sigMode :: AbiNames -> LHsSigWcType GhcRn -> Maybe Mode
sigMode abi (HsWC _ (L _ (HsSig _ _ body)))
  | any (isCtxHit (abiHasProbe abi) "probe") elems = Just ProbeMode
  | any (isCtxHit (abiHasCircuitContext abi) "circuitContext") elems = Just TraceMode
  | otherwise = Nothing
 where
  elems = ctxElems body

ctxElems :: LHsType GhcRn -> [LHsType GhcRn]
ctxElems (L _ t) = case t of
  HsForAllTy _ _ inner -> ctxElems inner
  HsQualTy _ (L _ ctxt) inner -> ctxt ++ ctxElems inner
  HsParTy _ inner -> ctxElems inner
  _ -> []

isCtxHit :: GHC.Name -> String -> LHsType GhcRn -> Bool
isCtxHit n ip (L _ t) = case t of
  HsTyVar _ _ (L _ n') -> n' == n
  HsIParamTy _ (L _ (HsIPName fs)) _ -> fs == GHC.fsLit ip
  HsParTy _ inner -> isCtxHit n ip inner
  _ -> False

--------------------------------------------------------------------------------
-- Top-level rewrite
--------------------------------------------------------------------------------

onTopBind ::
  AbiNames ->
  [(GHC.Name, Mode)] ->
  [GHC.Name] ->
  LHsBind GhcRn ->
  GHC.TcM (LHsBind GhcRn, [String])
onTopBind abi modes opaques lb@(L l b) = case b of
  FunBind{fun_id = L _ nm}
    | Just mode <- lookup nm modes -> do
        mg1 <- rewriteInside abi mode (fun_matches b)
        let
          (mg2, ws)
            | mode == TraceMode
            , nm `elem` opaques =
                componentWrapMG abi nm mg1
            | otherwise = (mg1, [])
        pure (L l b{fun_matches = mg2}, ws)
  _ -> pure (lb, [])

{- | Top-down generic traversal threading the current 'Mode'; intercepts
local binding groups (where their signatures live) so an inner
@HasProbe@\/@HasCircuitContext@ signature switches the mode for that binding's
subtree. Monadic only to mint fresh names for pattern-bound binders.
-}
rewriteInside ::
  AbiNames ->
  Mode ->
  MatchGroup GhcRn (LHsExpr GhcRn) ->
  GHC.TcM (MatchGroup GhcRn (LHsExpr GhcRn))
rewriteInside abi mode0 = goM mode0
 where
  goM :: (SYB.Data a) => Mode -> a -> GHC.TcM a
  goM m = SYB.gmapM (goM m) `SYB.extM` onVB m

  onVB :: Mode -> HsValBindsLR GhcRn GhcRn -> GHC.TcM (HsValBindsLR GhcRn GhcRn)
  onVB m (XValBindsLR (NValBinds groups sigs)) = do
    let localModes = modesFromSigs abi sigs
    groups' <- mapM (onGroup m localModes) groups
    pure (XValBindsLR (NValBinds groups' sigs))
  onVB _ vb = pure vb

  -- A bind that expands (pattern binders renamed + sibling trace binds)
  -- makes the group recursive: the extra binds reference binders of their
  -- own group. Their free-variable sets are precise, so the typechecker's
  -- dependency analysis restores the right order.
  onGroup ::
    Mode ->
    [(GHC.Name, Mode)] ->
    (GHC.RecFlag, LHsBinds GhcRn) ->
    GHC.TcM (GHC.RecFlag, LHsBinds GhcRn)
  onGroup m localModes (r, bs) = do
    results <- mapM (onLocalBind m localModes) (GHC.bagToList bs)
    let
      r' = if all (null . snd) results then r else GHC.Recursive
      binds = concatMap (\(b0, extras) -> b0 : extras) results
    pure (r', GHC.listToBag binds)

  onLocalBind ::
    Mode ->
    [(GHC.Name, Mode)] ->
    LHsBind GhcRn ->
    GHC.TcM (LHsBind GhcRn, [LHsBind GhcRn])
  onLocalBind inherited localModes (L l b) = case b of
    FunBind{fun_id = L _ nm} -> do
      let m = fromMaybe inherited (lookup nm localModes)
      ms <- goM m (fun_matches b)
      pure (L l (wrapFunBind abi m nm (locA l) b{fun_matches = ms}), [])
    PatBind{pat_lhs = L _ (VarPat _ (L _ nm))} -> do
      let m = fromMaybe inherited (lookup nm localModes)
      rhs <- goM m (pat_rhs b)
      pure (L l (wrapPatBind abi m nm (locA l) b{pat_rhs = rhs}), [])
    -- General pattern binding (tuple, record, bang, …): rename each wanted
    -- binder fresh inside the pattern and emit a sibling
    -- @x = autoTrace "x" x'@ per binder. The pattern keeps its shape, so
    -- strictness/refutability are untouched; every other binding keeps
    -- referring to the traced @x@.
    PatBind{pat_lhs = lpat} -> do
      rhs <- goM inherited (pat_rhs b)
      let binders = patBinders lpat
      if null binders
        then pure (L l b{pat_rhs = rhs}, [])
        else do
          renames <- mapM (\(nm, spn) -> (,) nm . (,) spn <$> freshen nm) binders
          let
            table = [(nm, nm') | (nm, (_, nm')) <- renames]
            lpat' = renameVarPats table lpat
            extras =
              [ mkTraceLocalBind
                  abi
                  (fromMaybe inherited (lookup nm localModes))
                  spn
                  nm
                  nm'
              | (nm, (spn, nm')) <- renames
              ]
          pure (L l b{pat_lhs = lpat', pat_rhs = rhs}, extras)
    _ -> do
      b' <- SYB.gmapM (goM inherited) b
      pure (L l b', [])

{- | The wanted binders of a pattern (each with its own source span):
every 'VarPat' not opted out. @x\@p@ aliases ('AsPat') are left alone.

The span is the binder NAME's own when it has one, falling back to the
enclosing pattern's. Code generators that synthesize patterns — notably
circuit-notation's port bindings — put the original source span on the
pattern but @noLoc@ on the located name inside it; without the fallback
the good-span gate would silently drop every generated port binder. A
pattern with no location anywhere is still skipped as compiler-generated.
-}
patBinders :: LPat GhcRn -> [(GHC.Name, GHC.SrcSpan)]
patBinders lpat =
  [ (nm, spn)
  | L lp (VarPat _ (L li nm)) <- SYB.listify isVarPatL lpat
  , let spn = pickSpan (locA li) (locA lp)
  , wantedBinder nm spn
  ]
 where
  isVarPatL :: LPat GhcRn -> Bool
  isVarPatL (L _ VarPat{}) = True
  isVarPatL _ = False

  pickSpan nameSpan patSpan
    | GHC.isGoodSrcSpan nameSpan = nameSpan
    | otherwise = patSpan

{- | Replace the given binders inside a pattern (occurrences in expressions
are a different constructor, 'HsVar', and cannot be hit).
-}
renameVarPats :: [(GHC.Name, GHC.Name)] -> LPat GhcRn -> LPat GhcRn
renameVarPats table = SYB.everywhere (SYB.mkT go)
 where
  go :: Pat GhcRn -> Pat GhcRn
  go (VarPat x (L li nm)) | Just nm' <- lookup nm table = VarPat x (L li nm')
  go p = p

{- | A fresh name for a pattern binder: same source span, primed occurrence
(purely cosmetic — resolution is by unique).
-}
freshen :: GHC.Name -> GHC.TcM GHC.Name
freshen nm = do
  u <- getUniqueM
  pure
    ( GHC.mkInternalName
        u
        (GHC.mkVarOcc (GHC.getOccString nm <> "'"))
        (GHC.nameSrcSpan nm)
    )

{- FOURMOLU_DISABLE -}
-- fourmolu cannot parse CPP inside a declaration; keep this region verbatim.

-- | @nmOld = autoTrace "nmOld" nmFresh@ (resp. 'autoProbe'), carrying a
-- precise free-variable set for dependency analysis.
mkTraceLocalBind ::
  AbiNames -> Mode -> GHC.SrcSpan -> GHC.Name -> GHC.Name -> LHsBind GhcRn
mkTraceLocalBind abi m spn nmOld nmFresh =
  L (noAnnSrcSpan spn)
    $ PatBind
      { pat_ext = GHC.unitNameSet nmFresh
      , pat_lhs = L (noAnnSrcSpan spn) (VarPat noExtField (L (noAnnSrcSpan spn) nmOld))
#if __GLASGOW_HASKELL__ >= 910
      , pat_mult = HsNoMultAnn noExtField
#endif
      , pat_rhs =
          GRHSs
            emptyComments
            [L (noAnnSrcSpan spn) (GRHS noAnn [] body)]
            (EmptyLocalBinds noExtField)
      }
 where
  injector = case m of
    TraceMode -> abiAutoTrace abi
    ProbeMode -> abiAutoProbe abi
  body = mkApp2 injector spn (GHC.getOccString nmOld) (mkVarE spn nmFresh)
{- FOURMOLU_ENABLE -}

-- | Wrap a zero-argument local function binding's right-hand sides.
wrapFunBind ::
  AbiNames -> Mode -> GHC.Name -> GHC.SrcSpan -> HsBind GhcRn -> HsBind GhcRn
wrapFunBind abi m nm spn b@FunBind{fun_ext = fvs, fun_matches = MG ext (L la ms)}
  | wantedBinder nm spn
  , not (closedBind nm fvs)
  , all zeroPat ms =
      b{fun_matches = MG ext (L la (map (fmap wrapMatch) ms))}
 where
  zeroPat (L _ (Match _ _ pats _)) = null pats
  zeroPat _ = False
  wrapMatch (Match mx mc [] grhss) =
    Match mx mc [] (wrapGRHSs abi m nm spn grhss)
  wrapMatch other = other
wrapFunBind _ _ _ _ b = b

wrapPatBind ::
  AbiNames -> Mode -> GHC.Name -> GHC.SrcSpan -> HsBind GhcRn -> HsBind GhcRn
wrapPatBind abi m nm spn b@PatBind{pat_ext = fvs, pat_rhs = grhss}
  | wantedBinder nm spn
  , not (closedBind nm fvs) =
      b{pat_rhs = wrapGRHSs abi m nm spn grhss}
wrapPatBind _ _ _ _ b = b

wantedBinder :: GHC.Name -> GHC.SrcSpan -> Bool
wantedBinder nm spn =
  not ("_" `isPrefixOf` GHC.getOccString nm)
    && GHC.isGoodSrcSpan spn

{- | Is this local binding /closed/ — no free variables besides top-level
('GHC.isExternalName') names and itself? Closed bindings are the ones GHC
generalizes even under @MonoLocalBinds@ (polymorphic local helpers:
@noWrite = pure Nothing@, shared configs, …). They must NOT be wrapped in
@autoTrace@: the injected constraint carries a @CanTrace@ type-family
application, which GHC refuses to quantify over in an inferred type, so the
binder would be monomorphized at its first use — and a second use at a
different type becomes a baffling type error (found on bittide's @timeWb@,
whose @noWrite@ is used at two register types; F9 in the dogfooding notes).

The trade-off: a closed binding is a constant by construction (it can
mention hidden clocks only through its type), so its trace is a flat line —
skipping it costs almost nothing. A closed binding that is genuinely
monomorphic (e.g. a self-contained @cnt = register 0 (cnt + 1)@ that never
mentions an argument) is skipped too; trace it explicitly with
'Clash.CircuitContext.Core.traceSignalC' if it matters. Self-references do
not open a binding: monomorphic recursion is resolved before
generalization, so tracing cannot rescue it either way.
-}
closedBind :: GHC.Name -> GHC.NameSet -> Bool
closedBind self fvs =
  all (\n -> GHC.isExternalName n || n == self) (GHC.nameSetElemsStable fvs)

wrapGRHSs ::
  AbiNames ->
  Mode ->
  GHC.Name ->
  GHC.SrcSpan ->
  GRHSs GhcRn (LHsExpr GhcRn) ->
  GRHSs GhcRn (LHsExpr GhcRn)
wrapGRHSs abi m nm spn (GRHSs x grhss localBinds) =
  GRHSs x (map (fmap wrapG) grhss) localBinds
 where
  injector = case m of
    TraceMode -> abiAutoTrace abi
    ProbeMode -> abiAutoProbe abi
  wrapG (GRHS gx guards body) =
    GRHS gx guards (mkApp2 injector spn (GHC.getOccString nm) body)
  wrapG g = g

--------------------------------------------------------------------------------
-- Component wrapping
--------------------------------------------------------------------------------

{- | @f args = body where binds@  ⇒  @f args = component "f" (let binds in
body)@. The where→let move is mandatory for correct hierarchy (see module
header). Guarded equations are wrapped too, via a case encoding

> f args | g1 = e1 | otherwise = e2  where binds
>   ⇒  f args = component "f" (let binds in case () of _ | g1 -> e1
>                                                        | otherwise -> e2)

The encoding is safe unless failing guards could FALL THROUGH to a later
equation — the case would turn that continuation into a runtime error. So
an equation is wrapped when its guards are self-exhaustive (final
alternative unguarded or @otherwise@) OR it is the LAST equation: with
nothing to fall through to, guard failure was already bottom, and the wrap
preserves that (modulo the pattern-match error's wording). Only
non-exhaustive guards on a non-final equation are skipped, with a warning.
Each equation is wrapped independently (only one equation's body ever
evaluates per instance, so no phantom instances arise).
-}
componentWrapMG ::
  AbiNames ->
  GHC.Name ->
  MatchGroup GhcRn (LHsExpr GhcRn) ->
  (MatchGroup GhcRn (LHsExpr GhcRn), [String])
componentWrapMG abi nm (MG ext (L la ms)) =
  (MG ext (L la (map fst results)), concatMap snd results)
 where
  results = zipWith wrapLM ms isLasts
  isLasts = map (== length ms) [1 ..]

  wrapLM (L lm m) isLast =
    let (m', ws) = wrapMatch (locA lm) isLast m in (L lm m', ws)

  wrapMatch spn isLast (Match mx mc pats (GRHSs gx galts localBinds))
    -- Single unguarded right-hand side.
    | [L lg (GRHS ggx [] body)] <- galts =
        ( Match
            mx
            mc
            pats
            (GRHSs gx [L lg (GRHS ggx [] (wrap spn localBinds body))] emptyLB)
        , []
        )
    -- Guarded: case-encode inside the wrap, unless failing guards could
    -- fall through to a later equation.
    | L lg (GRHS ggx _ _) : _ <- galts
    , selfExhaustive galts || isLast =
        let caseE = mkGuardCase ext spn gx galts
         in ( Match
                mx
                mc
                pats
                (GRHSs gx [L lg (GRHS ggx [] (wrap spn localBinds caseE))] emptyLB)
            , []
            )
    | otherwise =
        ( Match mx mc pats (GRHSs gx galts localBinds)
        ,
          [ "skipping component wrap for an equation of '"
              <> GHC.getOccString nm
              <> "' (its guards can fall through to a later equation); its"
              <> " bindings still trace at the enclosing scope"
          ]
        )

  wrap spn lbs body =
    mkApp2 (abiComponent abi) spn (GHC.getOccString nm) (mkLetE spn lbs body)

  emptyLB = EmptyLocalBinds noExtField

{- | Do these guarded alternatives always produce a value (final one
unguarded or a plain @otherwise@)?
-}
selfExhaustive :: [LGRHS GhcRn (LHsExpr GhcRn)] -> Bool
selfExhaustive [] = False
selfExhaustive galts = case last galts of
  L _ (GRHS _ [] _) -> True
  L _ (GRHS _ [L _ (BodyStmt _ (L _ (HsVar _ (L _ g))) _ _)] _) ->
    g == GHC.otherwiseIdName
  _ -> False

{- | @case () of { _ | g1 -> e1 | ... }@ carrying the original guarded
alternatives verbatim (their where-scope has moved to the enclosing let).
-}
{- FOURMOLU_DISABLE -}
-- fourmolu cannot parse CPP inside a declaration; keep this region verbatim.
mkGuardCase ::
  Origin ->
  GHC.SrcSpan ->
  XCGRHSs GhcRn (LHsExpr GhcRn) ->
  [LGRHS GhcRn (LHsExpr GhcRn)] ->
  LHsExpr GhcRn
mkGuardCase origin spn gx galts =
  L
    (noAnnSrcSpan spn)
#if __GLASGOW_HASKELL__ >= 910
    (HsCase CaseAlt scrut mg)
#else
    (HsCase noExtField scrut mg)
#endif
 where
  scrut =
    L
      (noAnnSrcSpan spn)
      (HsVar noExtField (L (noAnnSrcSpan spn) (GHC.getName GHC.unitDataCon)))
  mg =
    MG
      origin
      ( L
          (noAnnSrcSpan spn)
          [ L
              (noAnnSrcSpan spn)
              ( Match
                  noAnn
                  CaseAlt
                  [L (noAnnSrcSpan spn) (WildPat noExtField)]
                  (GRHSs gx galts (EmptyLocalBinds noExtField))
              )
          ]
      )

mkLetE ::
  GHC.SrcSpan -> HsLocalBinds GhcRn -> LHsExpr GhcRn -> LHsExpr GhcRn
mkLetE _ (EmptyLocalBinds _) body = body
mkLetE spn lbs body =
  L
    (noAnnSrcSpan spn)
#if __GLASGOW_HASKELL__ >= 910
    (HsLet noExtField lbs body)
#else
    (HsLet noExtField noHsTok lbs noHsTok body)
#endif
{- FOURMOLU_ENABLE -}

--------------------------------------------------------------------------------
-- Expression construction
--------------------------------------------------------------------------------

{- | @fn "name" (e)@ with every introduced node carrying the original
binding's span, so HasCallStack-derived design locations stay put.
-}
mkApp2 :: GHC.Name -> GHC.SrcSpan -> String -> LHsExpr GhcRn -> LHsExpr GhcRn
mkApp2 fn spn nm body =
  L spanA (unLoc (mkHsApp (mkHsApp (mkVarE spn fn) litE) (nlHsPar body)))
 where
  spanA = noAnnSrcSpan spn
  litE = nlHsLit (mkHsString nm)

-- | A variable occurrence carrying the given span.
mkVarE :: GHC.SrcSpan -> GHC.Name -> LHsExpr GhcRn
mkVarE spn nm =
  L (noAnnSrcSpan spn) (HsVar noExtField (L (noAnnSrcSpan spn) nm))
