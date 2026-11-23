{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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

Payloads of a width @n + 1@ that the oracle declines: one without a
@Waveform@ instance (recorded as raw bits, not described) and one without a
@BitPack@ instance (not recorded). The oracle decides a no as GHC rewrites
the constraint, which takes no round of GHC's plugin loop: the module
compiles in two rounds, with this plugin and without it, and it is held to
two, so a change that adds a round fails to compile here. In the solver
stage, a no took a round of its own.
-}
module Test.DeclinedWidths (
  Plain,
  foos,
  bars,
  subtracted,
) where

import Clash.Explicit.Prelude

import Clash.CircuitContext (HasCircuitContext)

-- | A payload with 'BitPack' and no @Waveform@ instance.
newtype Plain (n :: Nat) = Plain (BitVector n)
  deriving (Generic, NFDataX)

instance (KnownNat n) => BitPack (Plain n) where
  type BitSize (Plain n) = n
  pack (Plain b) = b
  unpack = Plain

-- | @foo@ is recorded as raw bits, not described.
foos ::
  (HasCircuitContext, KnownNat n) =>
  Signal System (BitVector (n + 1)) ->
  Signal System (Plain (n + 1))
foos inp = foo
 where
  foo = Plain <$> inp
{-# OPAQUE foos #-}

-- | A payload without 'BitPack'.
newtype Opaque (n :: Nat) = Opaque (BitVector n)

-- | @bar@ is not recorded.
bars ::
  (HasCircuitContext, KnownNat n) =>
  Signal System (BitVector (n + 1)) ->
  Signal System (BitVector (n + 1))
bars inp = (\(Opaque b) -> b) <$> bar
 where
  bar = Opaque <$> inp
{-# OPAQUE bars #-}

{- | A width with a subtraction that the oracle cannot show is defined: @addr@
is not recorded, and the module compiles, as it does without this plugin.
It did not: while the oracle's constraint for @addr@ waited for the solver
stage, @ghc-typelits-natnormalise@ normalised the width in it to @29 + (y -
w)@ and asked for @y <= 1 + w@ and @w <= y@ to justify that, which nothing
gives.
-}
subtracted ::
  forall w y.
  (HasCircuitContext, KnownNat w, KnownNat y, (w + 1) - y <= 30) =>
  SNat w ->
  SNat y ->
  Signal System (BitVector (30 - ((1 + w) - y)))
subtracted w _ = addr
 where
  addr = undefinedOf w
{-# OPAQUE subtracted #-}

-- | A signal of undefined values, of a type GHC need not know the width of.
undefinedOf :: proxy -> Signal System a
undefinedOf _ = pure (errorX "undefined")
{-# OPAQUE undefinedOf #-}
