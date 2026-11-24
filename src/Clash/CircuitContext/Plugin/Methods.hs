{-# LANGUAGE LambdaCase #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The bodies of instance methods and class defaults whose class gives them
@HasCircuitContext@ or @HasProbe@, reported once the type checker is done
with them.

The renamer pass rewrites value bindings only, so it does not reach the body
of an instance method or a class default. The constraint still does its job
there: the body can call instrumented code, and a body that only calls a
top-level function with @HasCircuitContext@ and @OPAQUE@ makes a component
of it. What is lost is what the pass would have done to the body if it were
a top-level function:

* a local binding that it would have traced (or probed) is not recorded;

* an @OPAQUE@ on the method does not make a component, as the pass makes
  components of top-level functions only. As the method is not split into a
  wrapper and a worker, it keeps the call sites as an argument, so Clash
  makes an HDL module of it for each call site.

The renamer finds the local bindings of such a body that it would have
recorded, with the same rewrite that it gives a top-level function, but
throws the rewrite away ('noteMethodBody'). After type checking, a binding is
kept only when a recorder could take its type, by the rule the oracle uses
for a binding it declines without a warning (see @neverRecordable@ in
"Clash.CircuitContext.Plugin.Oracle"), and a closed binding only when it would
have been reported as one (see "Clash.CircuitContext.Plugin.Closed"). The
body is reported, in @-Wx-circuit-context-uninstrumented@, when a binding is
left or when it has @OPAQUE@ in trace mode. A class signature is never
reported: the constraint has to be there for an instance to call
instrumented code, and a body that only calls a top-level function loses
nothing.
-}
module Clash.CircuitContext.Plugin.Methods (
  MethodBody (..),
  noteMethodBody,
  reportMethodBodies,
) where

import Control.Monad (filterM, forM_, unless)
import qualified Data.Generics as SYB
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)

import GHC.Hs
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC
import qualified GHC.Tc.Utils.Env as GHC (tcLookupTyCon)
import qualified GHC.Tc.Utils.Monad as GHC (getGblEnv)
import GHC.Types.SrcLoc (GenLocated (..))

import Clash.CircuitContext.Plugin.Closed (closedReported)
import qualified Clash.CircuitContext.Plugin.Diagnostics as Diag
import Clash.CircuitContext.Plugin.Names (AbiNames (..), lookupAbiNames)
import Clash.CircuitContext.Plugin.Oracle (recordableShape)

-- | The body of an instance method or a class default that asks for a mode.
data MethodBody = MethodBody
  { mbName :: GHC.Name
  -- ^ The method.
  , mbSpan :: GHC.SrcSpan
  -- ^ The binding of the body.
  , mbDefault :: Bool
  -- ^ A class default, not an instance method.
  , mbTrace :: Bool
  -- ^ The class gives it @HasCircuitContext@ ('True') or @HasProbe@.
  , mbWrapped :: [GHC.Name]
  -- ^ The local binders the rewrite would record.
  , mbClosed :: [GHC.Name]
  {- ^ The closed local binders the rewrite would leave alone, and report if
  GHC did not generalise them.
  -}
  , mbOpaque :: Bool
  -- ^ The method has @OPAQUE@.
  }

{- | The bodies per module, from the renamer (which runs once per declaration
group) to 'reportMethodBodies' (once per module).
-}
bodies :: IORef (Map.Map GHC.Module [MethodBody])
bodies = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE bodies #-}

-- | Remember a method body of this module, for 'reportMethodBodies'.
noteMethodBody :: MethodBody -> GHC.TcM ()
noteMethodBody mb = do
  md <- GHC.tcg_mod <$> GHC.getGblEnv
  GHC.liftIO $
    atomicModifyIORef' bodies (\m -> (Map.insertWith (<>) md [mb] m, ()))

{- | Report the method bodies of this module that lose a binding a recorder
would take, or that have @OPAQUE@ in trace mode (see the module header).
-}
reportMethodBodies ::
  [GHC.CommandLineOption] -> GHC.ModSummary -> GHC.TcGblEnv -> GHC.TcM GHC.TcGblEnv
reportMethodBodies _ _ env = do
  mine <-
    GHC.liftIO $
      atomicModifyIORef'
        bodies
        (\m -> (Map.delete (GHC.tcg_mod env) m, Map.findWithDefault [] (GHC.tcg_mod env) m))
  unless (null mine) $
    lookupAbiNames >>= \case
      Nothing -> pure ()
      Just abi -> do
        nonTranslatableTc <- GHC.tcLookupTyCon (abiNonTranslatable abi)
        let
          types = binderTypes (GHC.tcg_binds env)
          typed f = filterM (\n -> maybe (pure False) f (Map.lookup n types))
        -- A body is noted once per run of the renamer pass (twice when the
        -- plugin is enabled twice); 'Diag.report' says each message once.
        forM_ mine $ \mb -> do
          wrapped <- typed (recordableShape nonTranslatableTc (mbTrace mb)) (mbWrapped mb)
          closed <-
            if mbTrace mb
              then typed (closedReported nonTranslatableTc) (mbClosed mb)
              else pure []
          let
            lost = nub (wrapped <> closed)
            opaque = mbOpaque mb && mbTrace mb
          unless (null lost && not opaque) $
            Diag.report (mbSpan mb) Diag.Uninstrumented (message mb lost (nub closed) opaque)
  pure env

message :: MethodBody -> [GHC.Name] -> [GHC.Name] -> Bool -> [String]
message mb lost closed opaque =
  [ quoted
      <> " is "
      <> (if mbDefault mb then "a class default" else "an instance method")
      <> ", and its class gives it "
      <> constraint
      <> "."
  ]
    <> ( if null lost
           then []
           else
             [ "The plugin does not reach the body of an instance method or a class"
                 <> " default, so these local bindings are "
                 <> verb
                 <> " here: "
                 <> intercalate ", " (map GHC.getOccString lost)
                 <> "."
             , "To "
                 <> (if mbTrace mb then "trace" else "probe")
                 <> " them, move the body to a top-level function with "
                 <> constraint
                 <> (if mbTrace mb then " and OPAQUE" else "")
                 <> ", and call that function here."
             ]
               <> [ "A binding that uses no local variable ("
                      <> intercalate ", " (map GHC.getOccString closed)
                      <> ") is traced only with a type signature of its own."
                  | not (null closed)
                  ]
       )
    <> ( if opaque
           then
             [ "OPAQUE does not make "
                 <> quoted
                 <> " a component: the plugin makes components of top-level"
                 <> " functions only."
             , "Clash makes an HDL module of it for each call site, and these"
                 <> " modules differ in their names only."
             , "Put OPAQUE on the top-level function that it calls, and remove it here."
             ]
           else []
       )
 where
  quoted = "'" <> GHC.getOccString (mbName mb) <> "'"
  constraint = if mbTrace mb then "HasCircuitContext" else "HasProbe"
  verb = if mbTrace mb then "not traced" else "not probed"

{- | The final type of every local binder, by name: a generalised binder's
(its poly 'GHC.Id' in an @AbsBinds@ export), and a binder that was not
generalised, of a function binding or in a pattern.
-}
binderTypes :: LHsBinds GhcTc -> Map.Map GHC.Name GHC.Type
binderTypes binds =
  Map.fromList $
    [(GHC.getName i, GHC.idType i) | ABE{abe_poly = i} <- SYB.listify isExport binds]
      <> [ (GHC.getName i, GHC.idType i)
         | FunBind{fun_id = L _ i} <- SYB.listify isFunBind binds
         ]
      <> [(GHC.getName i, GHC.idType i) | VarPat _ (L _ i) <- SYB.listify isVarPat binds]
 where
  isExport :: ABExport -> Bool
  isExport _ = True
  isFunBind :: HsBindLR GhcTc GhcTc -> Bool
  isFunBind FunBind{} = True
  isFunBind _ = False
  isVarPat :: Pat GhcTc -> Bool
  isVarPat VarPat{} = True
  isVarPat _ = False
