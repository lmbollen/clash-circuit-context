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
  synonym Name, raw @?circuitContext@, or a CONSTRAINT SYNONYM that expands to
  either — see 'ctxMentions') are instrumented in TRACE mode; 'HasProbe'
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

* The pass is IDEMPOTENT: an injection this pass would make, and that is
  already there under the same name, is not made twice (see
  'alreadyInjected'). Two things stop being surprises because of it —
  enabling the plugin both package-wide and per module with an
  @OPTIONS_GHC@ pragma (which nested every component wrap in itself,
  @switch.switch@), and hand-writing the @component "f"@ the plugin would
  have written.

* Opt-outs: binder names starting with @_@; compiler-generated bindings are
  skipped via their non-real spans; per module, don't enable the plugin.

* The silent near-misses are reported as GHC warnings, in the categories
  "Clash.CircuitContext.Plugin.Diagnostics" defines: 'HasCircuitContext'
  without @OPAQUE@ (traces, but into the caller's scope), @OPAQUE@ without a
  signature (never instrumented, because the mode is read from the
  signature), a signature carrying both constraints (probe wins, and a probed
  function never becomes a component), and a class or instance method (this
  pass rewrites value bindings only, so it never reaches the body).

Supports GHC 9.6 and 9.10 (CPP at the few AST churn points).
-}
module Clash.CircuitContext.Plugin.Rename (renamePass) where

import qualified Data.Generics as SYB
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe)

import qualified GHC.Builtin.Names as GHC (ipClassName, otherwiseIdName)
import qualified GHC.Data.Bag as GHC
import GHC.Hs
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC
import qualified GHC.Tc.Utils.Env as GHC (tcLookupTyCon)
import qualified GHC.Tc.Utils.Monad as GHC (tryTcDiscardingErrs)
import GHC.Types.Basic (Origin)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import GHC.Types.Unique.Supply (MonadUnique (getUniqueM))

import qualified Clash.CircuitContext.Plugin.Diagnostics as Diag
import Clash.CircuitContext.Plugin.Names (AbiNames (..), lookupAbiNames)

data Mode = TraceMode | ProbeMode
  deriving (Eq)

{- | Everything the rewrite reads but never changes: the injection ABI, the
module's own constraint synonyms (needed to see through one in a signature —
the renamer runs before synonyms are expanded), and the module itself (to
tell a local name, which must be resolved from the group, from an imported
one, which is resolved from its interface).
-}
data Ctx = Ctx
  { ctxAbi :: AbiNames
  , ctxSyns :: [(GHC.Name, LHsType GhcRn)]
  , ctxMod :: GHC.Module
  }

{- | One thing to say, and where. Rendered by
"Clash.CircuitContext.Plugin.Diagnostics", which decides the warning flag it
answers to.
-}
type Note = (GHC.SrcSpan, Diag.Category, [String])

renamePass ::
  [GHC.CommandLineOption] ->
  GHC.TcGblEnv ->
  HsGroup GhcRn ->
  GHC.TcM (GHC.TcGblEnv, HsGroup GhcRn)
renamePass _opts env grp =
  lookupAbiNames >>= \case
    Nothing -> pure (env, grp)
    Just abi -> do
      let
        ctx =
          Ctx
            { ctxAbi = abi
            , ctxSyns = localSynonyms grp
            , ctxMod = GHC.tcg_mod env
            }
      (grp', notes) <- rewriteGroup ctx grp
      methods <- methodNotes ctx grp
      mapM_ (\(spn, cat, body) -> Diag.report spn cat body) (notes <> methods)
      pure (env, grp')

-- | The module's own type synonyms, by name, for 'ctxMentions'.
localSynonyms :: HsGroup GhcRn -> [(GHC.Name, LHsType GhcRn)]
localSynonyms grp =
  [ (n, rhs)
  | tyclGroup <- hs_tyclds grp
  , L _ SynDecl{tcdLName = L _ n, tcdRhs = rhs} <- group_tyclds tyclGroup
  ]

rewriteGroup :: Ctx -> HsGroup GhcRn -> GHC.TcM (HsGroup GhcRn, [Note])
rewriteGroup ctx grp = case hs_valds grp of
  XValBindsLR (NValBinds groups sigs) -> do
    (modes, both) <- classifySigs ctx sigs
    let
      opaques =
        [ n
        | L _ (InlineSig _ (L _ n) prag) <- sigs
        , isOpaqueSpec (GHC.inl_inline prag)
        ]
      signed = [n | L _ (TypeSig _ ns _) <- sigs, L _ n <- ns]
    results <-
      mapM
        ( \(r, bs) ->
            (,) r <$> mapM (onTopBind ctx modes opaques) (GHC.bagToList bs)
        )
        groups
    let
      groups' = [(r, GHC.listToBag (map fst prs)) | (r, prs) <- results]
      skipped = concat [concatMap snd prs | (_, prs) <- results]
      notes = diagnose modes opaques signed both groups
    pure (grp{hs_valds = XValBindsLR (NValBinds groups' sigs)}, skipped <> notes)
  _ -> pure (grp, [])

isOpaqueSpec :: GHC.InlineSpec -> Bool
isOpaqueSpec GHC.Opaque{} = True
isOpaqueSpec _ = False

--------------------------------------------------------------------------------
-- Signature classification
--------------------------------------------------------------------------------

{- | The mode each signature selects, plus the binders whose signature wrote
BOTH constraints (probe wins; the pair is only worth reporting).
-}
classifySigs ::
  Ctx -> [LSig GhcRn] -> GHC.TcM ([(GHC.Name, Mode)], [GHC.Name])
classifySigs ctx sigs = do
  classified <-
    sequence
      [ (,) ns <$> sigModes ctx sigTy
      | L _ (TypeSig _ ns sigTy) <- sigs
      ]
  let
    modes =
      [ (n, if isProbe then ProbeMode else TraceMode)
      | (ns, (isProbe, isTrace)) <- classified
      , isProbe || isTrace
      , L _ n <- ns
      ]
    both = [n | (ns, (True, True)) <- classified, L _ n <- ns]
  pure (modes, both)

-- | The modes a signature's context spine asks for: @(HasProbe, HasCircuitContext)@.
modesFromSigs :: Ctx -> [LSig GhcRn] -> GHC.TcM [(GHC.Name, Mode)]
modesFromSigs ctx sigs = fst <$> classifySigs ctx sigs

{- | Does this signature's context spine ask for probing, for tracing, or for
both? Only the spine counts: a nested-rank occurrence (like 'component'\'s own
@(HasCircuitContext => r)@ argument) does not bring the parameter into scope.
-}
sigModes :: Ctx -> LHsSigWcType GhcRn -> GHC.TcM (Bool, Bool)
sigModes ctx (HsWC _ body) = sigModesT ctx body

{- | 'sigModes' on the wildcard-free signature type a class-method signature
carries.
-}
sigModesT :: Ctx -> LHsSigType GhcRn -> GHC.TcM (Bool, Bool)
sigModesT ctx (L _ (HsSig _ _ body)) = do
  isProbe <- anyM (ctxMentions ctx probeIP) elems
  isTrace <- anyM (ctxMentions ctx circuitContextIP) elems
  pure (isProbe, isTrace)
 where
  elems = ctxElems body

probeIP, circuitContextIP :: GHC.FastString
probeIP = GHC.fsLit "probe"
circuitContextIP = GHC.fsLit "circuitContext"

ctxElems :: LHsType GhcRn -> [LHsType GhcRn]
ctxElems (L _ t) = case t of
  HsForAllTy _ _ inner -> ctxElems inner
  HsQualTy _ (L _ ctxt) inner -> ctxt ++ ctxElems inner
  HsParTy _ inner -> ctxElems inner
  _ -> []

{- | Does this context element bring the implicit parameter @ip@ into scope?

The constraint the plugin looks for is an implicit parameter behind a
constraint synonym (@type HasCircuitContext = ?circuitContext::CircuitContext@),
and the renamer runs BEFORE synonyms are expanded — so a signature written
against the designer's own alias,

> type SwitchCtx dom = (HiddenClockResetEnable dom, HasCircuitContext)

used to look, to this pass, like no constraint at all: the function kept
tracing (the typechecker sees the expanded constraint) but silently lost its
@$scope@, because only the renamer decides what is a component. Both kinds of
synonym are followed here instead:

* one declared in the module being compiled is not in the type environment
  yet, so its right-hand side is read from the group itself ('ctxSyns') and
  walked in the same surface syntax;

* an imported one IS resolvable, and its already-expanded 'GHC.Core.Type.Type'
  right-hand side is searched for the implicit parameter directly.

The recursion is depth-bounded: a synonym cycle is a type error the
typechecker will report properly, and this pass must not hang first.
Superclasses are deliberately NOT followed — @class HasCircuitContext => C a@
does discharge the constraint for the typechecker, but a class is a design
decision to keep visible, not an alias to see through.
-}
ctxMentions :: Ctx -> GHC.FastString -> LHsType GhcRn -> GHC.TcM Bool
ctxMentions ctx ip = go (5 :: Int)
 where
  go fuel (L _ t)
    | fuel <= 0 = pure False
    | otherwise = case t of
        HsTyVar _ _ (L _ n)
          | n == target -> pure True
          | otherwise -> viaSynonym fuel n
        HsIParamTy _ (L _ (HsIPName fs)) _ -> pure (fs == ip)
        HsParTy _ inner -> go fuel inner
        HsKindSig _ inner _ -> go fuel inner
        -- An applied synonym: the head is what decides.
        HsAppTy _ f _ -> go fuel f
        -- A synonym's right-hand side is usually a constraint tuple.
        HsTupleTy _ _ tys -> anyM (go fuel) tys
        _ -> pure False

  target
    | ip == probeIP = abiHasProbe (ctxAbi ctx)
    | otherwise = abiHasCircuitContext (ctxAbi ctx)

  viaSynonym fuel n
    | Just rhs <- lookup n (ctxSyns ctx) = go (fuel - 1) rhs
    | GHC.isExternalName n
    , not (GHC.nameIsLocalOrFrom (ctxMod ctx) n) =
        maybe False (coreMentions ip . GHC.expandTypeSynonyms)
          <$> importedSynonymRhs n
    | otherwise = pure False

{- | The right-hand side of an imported type synonym, or 'Nothing' for
anything else (a class, a data type, a name that will not resolve). Errors
from the lookup are discarded rather than added to the module's: failing to
recognise a constraint is this pass's normal, silent outcome, never a reason
to fail the build.
-}
importedSynonymRhs :: GHC.Name -> GHC.TcM (Maybe GHC.Type)
importedSynonymRhs n =
  GHC.tryTcDiscardingErrs
    (pure Nothing)
    (GHC.synTyConRhs_maybe <$> GHC.tcLookupTyCon n)

{- | Does this already-expanded constraint mention the implicit parameter
@ip@? An implicit parameter is the class @IP@ at a symbol literal, and a
constraint synonym's right-hand side is a constraint tuple of them.
-}
coreMentions :: GHC.FastString -> GHC.Type -> Bool
coreMentions ip ty = case GHC.splitTyConApp_maybe ty of
  Just (tc, args)
    | GHC.getName tc == GHC.ipClassName
    , (nameTy : _) <- args ->
        GHC.isStrLitTy nameTy == Just ip
    | GHC.isCTupleTyConName (GHC.getName tc) -> any (coreMentions ip) args
  _ -> False

anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM p = go
 where
  go [] = pure False
  go (x : xs) =
    p x >>= \case
      True -> pure True
      False -> go xs

--------------------------------------------------------------------------------
-- Diagnostics
--------------------------------------------------------------------------------

{- | The near-misses: clean compiles that cost a scope or a wire.

All in the @x-circuit-context-uninstrumented@ category, because they share a
property — each is often exactly what the author meant. A small helper
flattening into its caller is the normal case, not a mistake, which is why
this category is the one a project is most likely to silence
(@-Wno-x-circuit-context-uninstrumented@) once it has read the list.

The span goes to the diagnostic, so the messages do not repeat it.
-}
diagnose ::
  [(GHC.Name, Mode)] ->
  -- | OPAQUE binders
  [GHC.Name] ->
  -- | binders with a type signature
  [GHC.Name] ->
  -- | binders whose signature wrote both constraints
  [GHC.Name] ->
  [(GHC.RecFlag, LHsBinds GhcRn)] ->
  [Note]
diagnose modes opaques signed both groups = concatMap check binders
 where
  binders =
    [ (nm, locA l, scopesSomething (fun_matches b))
    | (_, bs) <- groups
    , L l b@FunBind{fun_id = L _ nm} <- GHC.bagToList bs
    ]
  -- Nothing in an uninstrumented module is a near-miss: the plugin is
  -- package-wide and most modules are not designs.
  instrumented = not (null modes)

  check (nm, spn, hasBindings) =
    [ note spn $
        [ "'" <> occ <> "' has HasCircuitContext but no OPAQUE pragma."
        , "Its bindings trace in the CALLER's scope, not a scope of its own."
        , "Add {-# OPAQUE " <> occ <> " #-} if you wanted one."
        ]
    | lookup nm modes == Just TraceMode
    , nm `notElem` opaques
    , -- With no where/let bindings there is nothing that WOULD have been
    -- scoped, so the missing pragma costs nothing and saying so is noise.
    hasBindings
    ]
      <> [ note spn $
             [ "'" <> occ <> "' is OPAQUE but has no type signature."
             , "The mode is read from the signature, so this is not"
                 <> " instrumented at all."
             ]
         | instrumented
         , nm `elem` opaques
         , nm `notElem` signed
         ]
      <> [ note spn $
             [ "'" <> occ <> "' has both HasProbe and HasCircuitContext."
             , "Probe mode wins, and a probed binder never becomes a component."
             ]
         | nm `elem` both
         ]
   where
    occ = GHC.getOccString nm

  note spn body = (spn, Diag.Uninstrumented, body)

{- | Does this binder have @where@\/@let@ bindings that a component wrap
would have scoped?
-}
scopesSomething :: MatchGroup GhcRn (LHsExpr GhcRn) -> Bool
scopesSomething (MG _ (L _ ms)) = any nonEmpty ms
 where
  nonEmpty (L _ (Match _ _ _ (GRHSs _ _ lbs))) = not (isEmpty lbs)
  nonEmpty _ = False
  isEmpty (EmptyLocalBinds _) = True
  isEmpty _ = False

{- | Signatures on class methods and class defaults that ask for
instrumentation the pass never performs.

The rewrite walks value bindings ('hs_valds') only, so a class-default or
instance-method body is never reached — its local bindings are not
auto-traced and it never becomes a component. The CONSTRAINT still does its
job (the body can call instrumented code); what silently does not happen is
the instrumentation of the body itself.
-}
methodNotes :: Ctx -> HsGroup GhcRn -> GHC.TcM [Note]
methodNotes ctx grp =
  concat
    <$> mapM
      ( \(nm, spn, sigTy) -> do
          (isProbe, isTrace) <- sigModesT ctx sigTy
          pure
            [ ( spn
              , Diag.Uninstrumented
              ,
                [ "'"
                    <> GHC.getOccString nm
                    <> "' is a class method or a class default."
                , "This pass instruments value bindings only, so the body is"
                    <> " not reached:"
                , "its local bindings are not auto-traced and it never becomes"
                    <> " a component."
                , "Move the body to a top-level binding to instrument it."
                ]
              )
            | isProbe || isTrace
            ]
      )
      methodSigs
 where
  -- Generic rather than constructor-by-constructor: class-method and
  -- instance-method signatures do not share a Sig constructor across the
  -- supported GHCs, and both live somewhere under a TyClGroup.
  methodSigs =
    [ (nm, locA l, sigTy)
    | L l sig <- SYB.listify isLSig (hs_tyclds grp)
    , (nm, sigTy) <- named sig
    ]

  isLSig :: LSig GhcRn -> Bool
  isLSig _ = True

  named = \case
    TypeSig _ ns (HsWC _ body) -> [(n, body) | L _ n <- ns]
    ClassOpSig _ _ ns body -> [(n, body) | L _ n <- ns]
    _ -> []

--------------------------------------------------------------------------------
-- Top-level rewrite
--------------------------------------------------------------------------------

onTopBind ::
  Ctx ->
  [(GHC.Name, Mode)] ->
  [GHC.Name] ->
  LHsBind GhcRn ->
  GHC.TcM (LHsBind GhcRn, [Note])
onTopBind ctx modes opaques lb@(L l b) = case b of
  FunBind{fun_id = L _ nm}
    | Just mode <- lookup nm modes -> do
        mg1 <- rewriteInside ctx mode (fun_matches b)
        let
          (mg2, ws)
            | mode == TraceMode
            , nm `elem` opaques =
                componentWrapMG (ctxAbi ctx) nm mg1
            | otherwise = (mg1, [])
        pure (L l b{fun_matches = mg2}, ws)
  _ -> pure (lb, [])

{- | Top-down generic traversal threading the current 'Mode'; intercepts
local binding groups (where their signatures live) so an inner
@HasProbe@\/@HasCircuitContext@ signature switches the mode for that binding's
subtree. Monadic only to mint fresh names for pattern-bound binders.
-}
rewriteInside ::
  Ctx ->
  Mode ->
  MatchGroup GhcRn (LHsExpr GhcRn) ->
  GHC.TcM (MatchGroup GhcRn (LHsExpr GhcRn))
rewriteInside ctx mode0 = goM mode0
 where
  abi = ctxAbi ctx

  goM :: (SYB.Data a) => Mode -> a -> GHC.TcM a
  goM m = SYB.gmapM (goM m) `SYB.extM` onVB m

  onVB :: Mode -> HsValBindsLR GhcRn GhcRn -> GHC.TcM (HsValBindsLR GhcRn GhcRn)
  onVB m (XValBindsLR (NValBinds groups sigs)) = do
    localModes <- modesFromSigs ctx sigs
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
    let
      binds = GHC.bagToList bs
      -- Binders this pass has ALREADY aliased (a previous run of it, when the
      -- plugin is enabled twice): renaming them again would bury the traced
      -- name under a second alias.
      aliased = mapMaybe (aliasedBinder abi) binds
    results <- mapM (onLocalBind m localModes aliased) binds
    let
      r' = if all (null . snd) results then r else GHC.Recursive
      binds' = concatMap (\(b0, extras) -> b0 : extras) results
    pure (r', GHC.listToBag binds')

  onLocalBind ::
    Mode ->
    [(GHC.Name, Mode)] ->
    [GHC.Name] ->
    LHsBind GhcRn ->
    GHC.TcM (LHsBind GhcRn, [LHsBind GhcRn])
  onLocalBind inherited localModes aliased (L l b) = case b of
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
      let binders = [p | p@(nm, _) <- patBinders lpat, nm `notElem` aliased]
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

{- | Is this a binder the designer wrote and did not opt out of?

Three gates:

* a leading @_@ is the user-facing opt-out (and GHC's own "unused" idiom);

* a @:@ anywhere in the name marks a binder a code generator INVENTED. No
  source-language variable identifier can contain a colon, so the mark is
  unforgeable — it can never skip a binder the user wrote. This is a contract
  with circuit-notation, which names all its plumbing this way (@final:stmt@,
  @lam:@…, @val:in@…); see Note [Synthesised binder names] there. Name-based
  is deliberate: the SPAN cannot carry the mark, because generators need real
  spans for their own name uniquification and for error locations, and
  'patBinders' already falls back from the (often 'GHC.noLoc') name to the
  enclosing pattern's span;

* a binder with no good span anywhere is compiler-generated.
-}
wantedBinder :: GHC.Name -> GHC.SrcSpan -> Bool
wantedBinder nm spn =
  not ("_" `isPrefixOf` occ)
    && ':' `notElem` occ
    && GHC.isGoodSrcSpan spn
 where
  occ = GHC.getOccString nm

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
  wrapG (GRHS gx guards body)
    | alreadyInjected [abiAutoTrace abi, abiAutoProbe abi] occ body =
        GRHS gx guards body
    | otherwise = GRHS gx guards (mkApp2 injector spn occ body)
  wrapG g = g
  occ = GHC.getOccString nm

--------------------------------------------------------------------------------
-- Idempotence
--------------------------------------------------------------------------------

{- | Is @e@ already @f "nm" (…)@, for one of the plugin's own injectors @f@
and this binder's own name?

Both conditions matter. The injector alone would also match a DIFFERENT
name, and @component "inner" …@ written by hand as the body of @outer@ asks
for two levels, not one. Requiring the name to match makes the check
recognise exactly the injection this pass is about to make — which is what
turns the pass idempotent, so enabling the plugin twice (package-wide plus
an @OPTIONS_GHC@ pragma) is a no-op instead of a nested @switch.switch@
scope, and hand-writing the wrap the plugin would have written is too.
-}
alreadyInjected :: [GHC.Name] -> String -> LHsExpr GhcRn -> Bool
alreadyInjected injectors nm e = case appSpine e of
  (L _ (HsVar _ (L _ f)), arg : _) -> f `elem` injectors && stringLitIs nm arg
  _ -> False

{- | @Just x@ when this bind is the sibling alias a previous run of this pass
added for a pattern binder: @nm = autoTrace "nm" x@.
-}
aliasedBinder :: AbiNames -> LHsBind GhcRn -> Maybe GHC.Name
aliasedBinder abi = \case
  L
    _
    PatBind
      { pat_lhs = L _ (VarPat _ (L _ nm))
      , pat_rhs = GRHSs _ [L _ (GRHS _ [] body)] _
      }
      | (L _ (HsVar _ (L _ f)), [lit, L _ (HsVar _ (L _ x))]) <- appSpine body
      , f `elem` [abiAutoTrace abi, abiAutoProbe abi]
      , stringLitIs (GHC.getOccString nm) lit ->
          Just x
  _ -> Nothing

-- | An expression's head and its arguments, with parentheses seen through.
appSpine :: LHsExpr GhcRn -> (LHsExpr GhcRn, [LHsExpr GhcRn])
appSpine = go [] . stripPars
 where
  go args (L _ (HsApp _ f a)) = go (stripPars a : args) (stripPars f)
  go args h = (h, args)

  stripPars (L l e) = L l (stripParensHsExpr e)

-- | A string literal with this content ('OverloadedStrings' included).
stringLitIs :: String -> LHsExpr GhcRn -> Bool
stringLitIs s = \case
  L _ (HsLit _ (HsString _ fs)) -> GHC.unpackFS fs == s
  L _ (HsOverLit _ ol) | HsIsString _ fs <- ol_val ol -> GHC.unpackFS fs == s
  _ -> False

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

An equation already wrapped in @component "f"@ is left alone; see
'alreadyInjected'.
-}
componentWrapMG ::
  AbiNames ->
  GHC.Name ->
  MatchGroup GhcRn (LHsExpr GhcRn) ->
  (MatchGroup GhcRn (LHsExpr GhcRn), [Note])
componentWrapMG abi nm (MG ext (L la ms)) =
  (MG ext (L la (map fst results)), concatMap snd results)
 where
  results = zipWith wrapLM ms isLasts
  isLasts = map (== length ms) [1 ..]

  wrapLM (L lm m) isLast =
    let (m', ws) = wrapMatch (locA lm) isLast m in (L lm m', ws)

  wrapMatch spn isLast m0@(Match mx mc pats (GRHSs gx galts localBinds))
    -- Single unguarded right-hand side.
    | [L lg (GRHS ggx [] body)] <- galts =
        if alreadyInjected [abiComponent abi] occ body
          then (m0, [])
          else
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
          [
            ( spn
            , Diag.Unhonoured
            ,
              [ "skipping component wrap for an equation of '" <> occ <> "'."
              , "Its guards can fall through to a later equation, which the"
                  <> " wrap would turn into a crash."
              , "Its bindings still trace, at the enclosing scope."
              ]
            )
          ]
        )

  wrap spn lbs body =
    mkApp2 (abiComponent abi) spn occ (mkLetE spn lbs body)

  occ = GHC.getOccString nm
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
