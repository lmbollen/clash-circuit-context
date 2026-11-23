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
  -fconstraint-solver-iterations=3 #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Payloads of a width @n + 1@ that the oracle declines: one without a
@Waveform@ instance (recorded as raw bits, not described) and one without a
@BitPack@ instance (not recorded). The oracle decides a no in its solver
stage, which takes a round of GHC's plugin loop. Without this plugin the
module compiles in two rounds; with it, it takes three, and it is held to
that, so a change that adds a round fails to compile here.
-}
module Test.DeclinedWidths (
  Plain,
  foos,
  bars,
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
