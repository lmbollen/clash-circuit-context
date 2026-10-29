{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Two shapes the renamer pass cannot read off the declaration group it is
handed, and that used to cost a scope or break the build. Both compile
without the plugin; this module is compiled with it, so building it is half
the test, and "Test.PluginDiagnostics" asserts the wires.
-}
module Test.DeclGroups (beforeSplice, etaReduced) where

import Clash.Explicit.Prelude
import Clash.Signal (withClock, withEnable, withReset)
import qualified Clash.Signal as Hidden (register)

import Clash.CircuitContext (HasCircuitContext)

{- | A constraint synonym ABOVE a top-level splice. The pass runs once per
declaration group and the splice ends one, so the group 'beforeSplice' is
renamed in does not hold this synonym: it is resolved from the type
environment, like an imported one.
-}
type SplitCtx dom = (KnownDomain dom, HasCircuitContext)

-- The splice that ends the group. @createDomain@ and @makeTopEntity@ do the
-- same thing in a real design.
$(pure [])

-- | Component via a synonym from an earlier declaration group.
beforeSplice :: (SplitCtx dom) => Signal dom Int -> Signal dom Int
beforeSplice inp = out
 where
  out = inp + 8
{-# OPAQUE beforeSplice #-}

{- | An UNSIGNED local whose inferred type is higher-rank: GHC gives
@withRst@ the type @(HiddenReset System => r) -> r@ from 'withReset'. A
recorder cannot be instantiated at that, so wrapping it broke the build; it
is a function, so not wrapping it costs no wire. @out@ still traces.
-}
etaReduced ::
  (HasCircuitContext) => Reset System -> Signal System Int -> Signal System Int
etaReduced rst inp = out
 where
  withRst = withReset rst
  out = withClock clockGen (withEnable enableGen (withRst (Hidden.register 0 inp)))
{-# OPAQUE etaReduced #-}
