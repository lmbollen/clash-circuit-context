{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

A local function that GHC generalises, of a width it does not know. GHC
quantifies over the constraint that records @zs@, and then checks the
inferred type: it solves the type's constraints from a copy of them. The
oracle must leave that wanted to the given that holds it. Deciding it
instead asks for @KnownNat (1 + m)@, which only @ghc-typelits-knownnat@
gives, and this suite does not load it. Compiling the module is the test;
"Test.GeneralisedLocalsAmbiguous" has the same shape after that check.
-}
module Test.GeneralisedLocals (widened) where

import Clash.Prelude

import Clash.CircuitContext (HasCircuitContext)

-- | The local @widen@ has the inferred type that is checked.
widened ::
  (HasCircuitContext) => Signal System (BitVector 4) -> Signal System (BitVector 5)
widened xs = widen xs
 where
  widen (ys :: Signal System (BitVector m)) = zs
   where
    zs = fmap ((0 :: BitVector 1) ++#) ys
{-# OPAQUE widened #-}
