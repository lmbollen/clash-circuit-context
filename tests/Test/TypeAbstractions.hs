{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

A higher-rank component defined with a type-abstraction pattern. Only built
on GHC 9.10 and up, which is where @f \@a x = ...@ became legal; compiling it,
with no diagnostic naming it, is the test.
-}
module Test.TypeAbstractions (topRankNInvis) where

import Clash.Prelude

import Clash.CircuitContext (HasCircuitContext)

type LocalRst dom r = ((HiddenReset dom) => r) -> r

{- | @\@dom@ binds the forall, not an arrow: only @active@ consumes one, so
what remains is higher-rank and the equation is eta-expanded by one
argument. Counting @\@dom@ as an argument consumed the polytype's arrow too,
and the wrap broke the build.
-}
topRankNInvis ::
  forall dom r.
  (HasCircuitContext, KnownDomain dom) =>
  Signal dom Bool ->
  LocalRst dom r
topRankNInvis @dom active = withReset (unsafeFromActiveLow @dom active)
{-# OPAQUE topRankNInvis #-}
