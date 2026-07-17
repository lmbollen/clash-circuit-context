{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | The renamer-stage rewrite ('GHC.renamedResultAction'):

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
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Generics as SYB
import System.IO (hPutStrLn, stderr)

import qualified GHC.Builtin.Names as GHC (otherwiseIdName)
import qualified GHC.Builtin.Types as GHC (unitDataCon)
import qualified GHC.Data.Bag as GHC
import GHC.Hs
import GHC.Parser.Annotation (locA, noAnn, noAnnSrcSpan)
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC
import GHC.Types.Basic (Origin)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)

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
      let (grp', warnings) = rewriteGroup abi grp
      unless (null warnings)
        $ GHC.liftIO
        $ mapM_ (hPutStrLn stderr . ("clash-circuit-context plugin: " <>)) warnings
      pure (env, grp')

rewriteGroup :: AbiNames -> HsGroup GhcRn -> (HsGroup GhcRn, [String])
rewriteGroup abi grp = case hs_valds grp of
  XValBindsLR (NValBinds groups sigs) ->
    let
      modes = modesFromSigs abi sigs
      opaques =
        [ n
        | L _ (InlineSig _ (L _ n) prag) <- sigs
        , isOpaqueSpec (GHC.inl_inline prag)
        ]
      results =
        [ (r, GHC.mapBag (onTopBind abi modes opaques) bs)
        | (r, bs) <- groups
        ]
      groups' = [(r, fmap fst bs) | (r, bs) <- results]
      warnings =
        concat [concat (GHC.bagToList (fmap snd bs)) | (_, bs) <- results]
     in
      (grp{hs_valds = XValBindsLR (NValBinds groups' sigs)}, warnings)
  _ -> (grp, [])

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

-- | Mode from a signature's context spine; probe wins when both appear.
-- Only the spine counts: a nested-rank occurrence (like 'component'\'s own
-- @(HasCircuitContext => r)@ argument) does not bring the parameter into scope.
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
  (LHsBind GhcRn, [String])
onTopBind abi modes opaques lb@(L l b) = case b of
  FunBind{fun_id = L _ nm}
    | Just mode <- lookup nm modes ->
        let
          mg1 = rewriteInside abi mode (fun_matches b)
          (mg2, ws)
            | mode == TraceMode, nm `elem` opaques =
                componentWrapMG abi nm mg1
            | otherwise = (mg1, [])
         in
          (L l b{fun_matches = mg2}, ws)
  _ -> (lb, [])

{- | Top-down generic traversal threading the current 'Mode'; intercepts
local binding groups (where their signatures live) so an inner
@HasProbe@\/@HasCircuitContext@ signature switches the mode for that binding's
subtree.
-}
rewriteInside ::
  AbiNames ->
  Mode ->
  MatchGroup GhcRn (LHsExpr GhcRn) ->
  MatchGroup GhcRn (LHsExpr GhcRn)
rewriteInside abi mode0 = goT mode0
 where
  goT :: SYB.Data a => Mode -> a -> a
  goT m = SYB.gmapT (goT m) `SYB.extT` onVB m

  onVB :: Mode -> HsValBindsLR GhcRn GhcRn -> HsValBindsLR GhcRn GhcRn
  onVB m (XValBindsLR (NValBinds groups sigs)) =
    let
      localModes = modesFromSigs abi sigs
      groups' =
        [ (r, GHC.mapBag (onLocalBind m localModes) bs)
        | (r, bs) <- groups
        ]
     in
      XValBindsLR (NValBinds groups' sigs)
  onVB _ vb = vb

  onLocalBind ::
    Mode -> [(GHC.Name, Mode)] -> LHsBind GhcRn -> LHsBind GhcRn
  onLocalBind inherited localModes (L l b) = case b of
    FunBind{fun_id = L _ nm} ->
      let
        m = fromMaybe inherited (lookup nm localModes)
        b1 = b{fun_matches = goT m (fun_matches b)}
       in
        L l (wrapFunBind abi m nm (locA l) b1)
    PatBind{pat_lhs = L _ (VarPat _ (L _ nm))} ->
      let
        m = fromMaybe inherited (lookup nm localModes)
        b1 = b{pat_rhs = goT m (pat_rhs b)}
       in
        L l (wrapPatBind abi m nm (locA l) b1)
    _ -> L l (SYB.gmapT (goT inherited) b)

-- | Wrap a zero-argument local function binding's right-hand sides.
wrapFunBind ::
  AbiNames -> Mode -> GHC.Name -> GHC.SrcSpan -> HsBind GhcRn -> HsBind GhcRn
wrapFunBind abi m nm spn b@FunBind{fun_matches = MG ext (L la ms)}
  | wantedBinder nm spn
  , all zeroPat ms =
      b{fun_matches = MG ext (L la (map (fmap wrapMatch) ms))}
 where
  zeroPat (L _ (Match _ _ pats _)) = null pats
  wrapMatch (Match mx mc [] grhss) =
    Match mx mc [] (wrapGRHSs abi m nm spn grhss)
  wrapMatch other = other
wrapFunBind _ _ _ _ b = b

wrapPatBind ::
  AbiNames -> Mode -> GHC.Name -> GHC.SrcSpan -> HsBind GhcRn -> HsBind GhcRn
wrapPatBind abi m nm spn b@PatBind{pat_rhs = grhss}
  | wantedBinder nm spn =
      b{pat_rhs = wrapGRHSs abi m nm spn grhss}
wrapPatBind _ _ _ _ b = b

wantedBinder :: GHC.Name -> GHC.SrcSpan -> Bool
wantedBinder nm spn =
  not ("_" `isPrefixOf` GHC.getOccString nm)
    && GHC.isGoodSrcSpan spn

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
        , [ "skipping component wrap for an equation of '"
              <> GHC.getOccString nm
              <> "' (its guards can fall through to a later equation); its"
              <> " bindings still trace at the enclosing scope"
          ]
        )
  wrapMatch _ _ m = (m, [])

  wrap spn lbs body =
    mkApp2 (abiComponent abi) spn (GHC.getOccString nm) (mkLetE spn lbs body)

  emptyLB = EmptyLocalBinds noExtField

-- | Do these guarded alternatives always produce a value (final one
-- unguarded or a plain @otherwise@)?
selfExhaustive :: [LGRHS GhcRn (LHsExpr GhcRn)] -> Bool
selfExhaustive [] = False
selfExhaustive galts = case last galts of
  L _ (GRHS _ [] _) -> True
  L _ (GRHS _ [L _ (BodyStmt _ (L _ (HsVar _ (L _ g))) _ _)] _) ->
    g == GHC.otherwiseIdName
  _ -> False

-- | @case () of { _ | g1 -> e1 | ... }@ carrying the original guarded
-- alternatives verbatim (their where-scope has moved to the enclosing let).
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

--------------------------------------------------------------------------------
-- Expression construction
--------------------------------------------------------------------------------

-- | @fn "name" (e)@ with every introduced node carrying the original
-- binding's span, so HasCallStack-derived design locations stay put.
mkApp2 :: GHC.Name -> GHC.SrcSpan -> String -> LHsExpr GhcRn -> LHsExpr GhcRn
mkApp2 fn spn nm body =
  L spanA (unLoc (mkHsApp (mkHsApp varE litE) (nlHsPar body)))
 where
  spanA = noAnnSrcSpan spn
  varE = L spanA (HsVar noExtField (L (noAnnSrcSpan spn) fn))
  litE = nlHsLit (mkHsString nm)
