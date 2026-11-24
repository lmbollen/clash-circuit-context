{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

A step function that is given to 'Clash.CircuitContext.Core.mealyProbed' (or
to @mooreProbed@, @mealyBProbed@, @mooreBProbed@, @probeFmap@) but cannot
probe anything, reported once the type checker is done with it.

The step function's parameter has the type @(HasProbe) => s -> i -> (s, o)@,
so GHC type checks the argument under a binder of the @?probe@ evidence
(a 'WpEvLam' in the wrapper of the argument). Everything in the argument that
probes takes this evidence: a call of 'Clash.CircuitContext.Core.probe', a
function with @HasProbe@ in its signature (also one the plugin probes, and
also when it is partly applied, composed or imported), and a function
argument that itself has the type @(HasProbe) => …@. When nothing in the
argument uses the evidence, the step function probes nothing: its type has
no @HasProbe@, so the plugin did not probe its local bindings either. That is
reported in @-Wx-circuit-context-uninstrumented@.

A step function that has @HasProbe@ in its signature but no binding to probe
is not reported: the reference to it takes the evidence. Its signature asks
for probes, so whatever it holds is what the designer wants probed.

The check reads the arguments of these five functions only, not those of
other functions with a @(HasProbe) => …@ parameter (a designer's own wrapper
around @mealyProbed@): such a wrapper passes its parameter on, and the check
is made where it does so.
-}
module Clash.CircuitContext.Plugin.Steps (reportUnprobedSteps) where

import Control.Monad (forM_)
import qualified Data.Generics as SYB
import Data.Maybe (fromMaybe, listToMaybe)

import qualified GHC.Builtin.Names as GHC (dollarName, ipClassName)
import GHC.Hs
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC
import GHC.Tc.Types.Evidence (EvBind (..), HsWrapper (..), TcEvBinds (..))
import GHC.Types.SrcLoc (GenLocated (..))

import qualified Clash.CircuitContext.Plugin.Diagnostics as Diag
import Clash.CircuitContext.Plugin.Names (AbiNames (..), lookupAbiNames)

{- | Report each step function given to one of the probed combinators that
uses no @?probe@ evidence (see the module header).
-}
reportUnprobedSteps ::
  [GHC.CommandLineOption] -> GHC.ModSummary -> GHC.TcGblEnv -> GHC.TcM GHC.TcGblEnv
reportUnprobedSteps _ _ env =
  lookupAbiNames >>= \case
    Nothing -> pure env
    Just abi -> do
      let combinators = abiProbedCombinators abi
      forM_ (unprobedSteps (map fst combinators) (GHC.tcg_binds env)) $
        \(spn, fun, arg) -> do
          let
            funOcc = GHC.getOccString fun
            plain = fromMaybe "" (lookup fun combinators)
            what
              | plain == "fmap" = "function"
              | otherwise = "step function"
          Diag.report spn Diag.Uninstrumented $
            [ case arg of
                Just n ->
                  quoted n
                    <> " probes nothing: it is the "
                    <> what
                    <> " of "
                    <> funOcc
                    <> ", and its type has no HasProbe."
                Nothing ->
                  "The " <> what <> " of " <> funOcc <> " probes nothing: its type has no HasProbe."
            , "The plugin probes the local bindings of a function only when its"
                <> " signature has HasProbe."
            , "To probe them, "
                <> maybe
                  "make it a function with a type signature with HasProbe"
                  (\n -> "give " <> quoted n <> " a type signature with HasProbe")
                  arg
                <> " ("
                <> maybe "step" occ arg
                <> " :: (HasProbe) => ...), or call probe in it."
            , "If it has nothing to probe, use " <> plain <> " in place of " <> funOcc <> "."
            ]
      pure env

-- | A name in quotes, as the other messages give it.
quoted :: GHC.Name -> String
quoted n = "'" <> occ n <> "'"

-- | A name as the source writes it in a prefix position.
occ :: GHC.Name -> String
occ n
  | GHC.isSymOcc (GHC.getOccName n) = "(" <> GHC.getOccString n <> ")"
  | otherwise = GHC.getOccString n

{- | The arguments of an application of one of the given functions whose
@?probe@ evidence is not used: their span, the function applied, and the
argument's name when it is a variable.
-}
unprobedSteps ::
  [GHC.Name] -> LHsBinds GhcTc -> [(GHC.SrcSpan, GHC.Name, Maybe GHC.Name)]
unprobedSteps targets =
  SYB.everythingBut (<>) (([], False) `SYB.mkQ` onExpr `SYB.extQ` stopTy `SYB.extQ` stopCo)
 where
  onExpr :: HsExpr GhcTc -> ([(GHC.SrcSpan, GHC.Name, Maybe GHC.Name)], Bool)
  onExpr = \case
    HsApp _ f arg
      | Just fun <- appliedTarget f
      , (wrappers, inner) <- peel arg
      , evs@(_ : _) <- concatMap probeEvLams wrappers
      , not (usesAny evs arg) ->
          ([(getLocA arg, fun, varName inner)], False)
    _ -> ([], False)

  stopTy :: GHC.Type -> ([a], Bool)
  stopTy _ = ([], True)
  stopCo :: GHC.Coercion -> ([a], Bool)
  stopCo _ = ([], True)

  -- The function at the head of an application spine, when it is one of the
  -- targets; through @f $ x@ too.
  appliedTarget :: LHsExpr GhcTc -> Maybe GHC.Name
  appliedTarget e = case spine e of
    (Just h, _) | h `elem` targets -> Just h
    (Just h, [g]) | h == GHC.dollarName -> appliedTarget g
    _ -> Nothing

  spine :: LHsExpr GhcTc -> (Maybe GHC.Name, [LHsExpr GhcTc])
  spine (L _ e) = case snd (peelE e) of
    HsApp _ f a -> (++ [a]) <$> spine f
    HsVar _ (L _ i) -> (Just (GHC.getName i), [])
    e'
      | Just inner <- typeAppInner e' -> spine inner
      | otherwise -> (Nothing, [])

  varName :: HsExpr GhcTc -> Maybe GHC.Name
  varName = \case
    HsVar _ (L _ i) -> Just (GHC.getName i)
    _ -> Nothing

{- | The wrappers around an expression, outermost first, through parentheses,
and what they wrap.
-}
peel :: LHsExpr GhcTc -> ([HsWrapper], HsExpr GhcTc)
peel (L _ e) = peelE e

peelE :: HsExpr GhcTc -> ([HsWrapper], HsExpr GhcTc)
peelE e = case e of
  XExpr x | Just (w, inner) <- wrapped x -> let (ws, e') = peelE inner in (w : ws, e')
  _
    | Just inner <- parInner e -> peel inner
    | otherwise -> ([], e)

{- FOURMOLU_DISABLE -}
-- fourmolu cannot parse CPP inside a declaration; keep this region verbatim.
wrapped :: XXExprGhcTc -> Maybe (HsWrapper, HsExpr GhcTc)
#if __GLASGOW_HASKELL__ >= 912
wrapped (WrapExpr w inner) = Just (w, inner)
#else
wrapped (WrapExpr (HsWrap w inner)) = Just (w, inner)
#endif
wrapped _ = Nothing
{- FOURMOLU_ENABLE -}

-- | The expression in parentheses (the field differs between GHCs).
parInner :: HsExpr GhcTc -> Maybe (LHsExpr GhcTc)
parInner e@HsPar{} = listToMaybe (SYB.gmapQ (SYB.mkQ [] (: [])) e >>= id)
parInner _ = Nothing

-- | The expression of a visible type application.
typeAppInner :: HsExpr GhcTc -> Maybe (LHsExpr GhcTc)
typeAppInner e@HsAppType{} = listToMaybe (SYB.gmapQ (SYB.mkQ [] (: [])) e >>= id)
typeAppInner _ = Nothing

-- | The @?probe@ evidence a wrapper binds.
probeEvLams :: HsWrapper -> [GHC.Var]
probeEvLams = \case
  WpCompose a b -> probeEvLams a <> probeEvLams b
  WpEvLam ev | isProbeEv ev -> [ev]
  _ -> []

isProbeEv :: GHC.Var -> Bool
isProbeEv ev = case GHC.splitTyConApp_maybe (GHC.varType ev) of
  Just (tc, nameTy : _) ->
    GHC.getName tc == GHC.ipClassName && GHC.isStrLitTy nameTy == Just (GHC.fsLit "probe")
  _ -> False

{- | Does any of these evidence variables occur in this tree, other than
where a 'WpEvLam' binds it? Evidence bindings are read by hand: their 'SYB.Data'
instance does not descend. A binding that is not zonked yet cannot be read,
and counts as a use.
-}
usesAny :: (SYB.Data a) => [GHC.Var] -> a -> Bool
usesAny evs = go
 where
  go :: (SYB.Data b) => b -> Bool
  go =
    (\x -> or (SYB.gmapQ go x))
      `SYB.extQ` onVar
      `SYB.extQ` onWrapper
      `SYB.extQ` onEvBinds
      `SYB.extQ` (\(_ :: GHC.Type) -> False)
      `SYB.extQ` (\(_ :: GHC.Coercion) -> False)

  onVar :: GHC.Var -> Bool
  onVar v = v `elem` evs

  onWrapper :: HsWrapper -> Bool
  onWrapper = \case
    WpEvLam _ -> False
    w -> or (SYB.gmapQ go w)

  onEvBinds :: TcEvBinds -> Bool
  onEvBinds = \case
    EvBinds bs -> any (\b -> go (eb_rhs b)) bs
    TcEvBinds _ -> True
