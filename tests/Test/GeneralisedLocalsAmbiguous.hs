{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

"Test.GeneralisedLocals" without GHC's check of the inferred type
(@AllowAmbiguousTypes@ turns it off). The wanted that records @zs@ is then
in the residual implication of @widen@, where the constraint GHC quantified
over is its given. The oracle must leave it to that given here too.
-}
module Test.GeneralisedLocalsAmbiguous (widenedAmbiguous) where

import Clash.Prelude

import Clash.CircuitContext (HasCircuitContext)

-- | The local @widen@ has the residual implication.
widenedAmbiguous ::
  (HasCircuitContext) => Signal System (BitVector 4) -> Signal System (BitVector 5)
widenedAmbiguous xs = widen xs
 where
  widen (ys :: Signal System (BitVector m)) = zs
   where
    zs = fmap ((0 :: BitVector 1) ++#) ys
{-# OPAQUE widenedAmbiguous #-}
