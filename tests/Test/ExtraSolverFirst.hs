{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
-- One pragma: the plugins run in the order given, and a formatter sorts
-- separate pragmas.
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver
  -fplugin=GHC.TypeLits.Normalise
  -fplugin=GHC.TypeLits.KnownNat.Solver
  -fplugin=Clash.CircuitContext.Plugin
  -fconstraint-solver-iterations=2 #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The pointer of "Main", with @ghc-typelits-extra@'s solver enabled first, as
Clash projects often list it. Without this plugin the module compiles in
two rounds of GHC's plugin loop; it is held to two, so a change that makes
the oracle take a round of its own fails to compile here.
-}
module Test.ExtraSolverFirst (ptrExtra) where

import Clash.Explicit.Prelude

import Clash.CircuitContext (HasCircuitContext)

-- | A pointer one bit wider than its address: the width is @aw + 1@.
ptrExtra ::
  forall aw.
  (HasCircuitContext, KnownNat aw) =>
  Signal System Bool ->
  Signal System (Unsigned (aw + 1))
ptrExtra inc = p
 where
  p = register clockGen resetGen enableGen 0 next
  next = mux inc (p + 1) p
{-# OPAQUE ptrExtra #-}
