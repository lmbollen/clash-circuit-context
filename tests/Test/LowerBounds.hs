{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
-- One pragma: the plugins run in the order given, and a formatter sorts
-- separate pragmas. The typelits plugins alone take five rounds here.
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver
  -fplugin=GHC.TypeLits.Normalise
  -fplugin=GHC.TypeLits.KnownNat.Solver
  -fplugin=Clash.CircuitContext.Plugin
  -fconstraint-solver-iterations=5 #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Widths whose side conditions hold for every value of their variables, from
a downstream design (bittide). The typelits plugins prove them in the real
compile, so the oracle must too, or the binding is lost: @1 <= 8 + w@
(@ghc-typelits-natnormalise@) and @1 <= Max 1 p@ (@ghc-typelits-extra@).
Beside them, side conditions that are true but that no plugin proves (@1 <=
Max 3 p@), and ones that are not true for every value (@1 <= n - 1@): the
oracle must not say yes to those, or the module does not compile. "Main"
checks what was recorded.
-}
module Test.LowerBounds (
  remote,
  remoteRestated,
  remoteWide,
  idle,
  idleThree,
  predecessor,
  sampled,
) where

import qualified Prelude as P

import Clash.Explicit.Prelude

import Clash.CircuitContext (HasCircuitContext)

import Test.SideConditions (idleWithoutExtra)

{- | A signal of any width, of undefined values. The bindings below apply it
to an argument, so they are not closed and the plugin records them.
-}
anyWidth :: (KnownNat n) => proxy -> Signal System (BitVector n)
anyWidth _ = pure (errorX "any")
{-# OPAQUE anyWidth #-}

-- | The same, for a type that GHC need not know the width of.
undefinedOf :: proxy -> Signal System a
undefinedOf _ = pure (errorX "undefined")
{-# OPAQUE undefinedOf #-}

{- | The address width of a bus behind a prefix for @w + 8@ busses (bittide's
CallistoSw). @KnownNat (30 - CLog 2 (8 + w))@ needs @1 <= 8 + w@, which holds
for every @w@, and @CLog 2 (8 + w) <= 30@, which the signature gives:
@addr@ is recorded.
-}
remote ::
  forall w.
  (HasCircuitContext, KnownNat w, CLog 2 (8 + w) <= 30) =>
  SNat w ->
  Signal System (BitVector (30 - CLog 2 (8 + w)))
remote w = addr
 where
  addr = anyWidth w
{-# OPAQUE remote #-}

{- | The same with @CLog 2 ((w + 7) + 1) <= 30@ in the signature, as
CallistoSw writes it (@PrefixWidth (otherWb + SwcccInternalBusses) <= 30@):
the same sums of products, in another order. @addr@ is recorded.
-}
remoteRestated ::
  forall w.
  (HasCircuitContext, KnownNat w, CLog 2 ((w + 7) + 1) <= 30) =>
  SNat w ->
  Signal System (BitVector (30 - CLog 2 (8 + w)))
remoteRestated w = addr
 where
  addr = anyWidth w
{-# OPAQUE remoteRestated #-}

-- | The wider of that address and another one: @wide@ is recorded.
remoteWide ::
  forall w wMu.
  (HasCircuitContext, KnownNat w, KnownNat wMu, CLog 2 ((w + 7) + 1) <= 30) =>
  SNat w ->
  SNat wMu ->
  Signal System (BitVector (Max (30 - CLog 2 (8 + w)) wMu))
remoteWide w _ = wide
 where
  wide = anyWidth w
{-# OPAQUE remoteWide #-}

{- | A count of idle cycles, at most a time divided by a period @p@ that may
be 0 (bittide's Si539xSpi). @KnownNat (Div t (Max 1 p))@ needs @1 <= Max 1
p@: @cycles@ is recorded.
-}
idle ::
  forall p.
  (HasCircuitContext, KnownNat p) =>
  SNat p ->
  Signal System (Index (Max 1 (Div (94999 + Max 1 p) (Max 1 p))))
idle p = cycles
 where
  cycles = maxBound <$ undefinedOf p
{-# OPAQUE idle #-}

{- | The same with @Max 3 p@. @1 <= Max 3 p@ holds for every @p@, but
@ghc-typelits-extra@ proves @a <= Max a b@ only: @cycles@ is not recorded.
-}
idleThree ::
  forall p.
  (HasCircuitContext, KnownNat p) =>
  SNat p ->
  Signal System (Index (Max 1 (Div 94999 (Max 3 p))))
idleThree p = cycles
 where
  cycles = undefinedOf p
{-# OPAQUE idleThree #-}

{- | @CLog 2 (n - 1)@ needs @1 <= n - 1@, which does not hold for @n = 1@:
@addr@ is not recorded.
-}
predecessor ::
  forall n.
  (HasCircuitContext, KnownNat n, 1 <= n) =>
  SNat n ->
  Signal System (BitVector (CLog 2 (n - 1)))
predecessor n = addr
 where
  addr = undefinedOf n
{-# OPAQUE predecessor #-}

{- | Two cycles of each, at widths the plugins compute here: "Main" is held
to fewer rounds than these take.
-}
sampled :: (HasCircuitContext) => Int
sampled =
  P.length (sampleN 2 (remote (SNat @3)))
    P.+ P.length (sampleN 2 (remoteRestated (SNat @3)))
    P.+ P.length (sampleN 2 (remoteWide (SNat @3) (SNat @5)))
    P.+ P.length (sampleN 2 (idle (SNat @10)))
    P.+ P.length (sampleN 2 (idleThree (SNat @2)))
    P.+ P.length (sampleN 2 (predecessor (SNat @2)))
    P.+ P.length (sampleN 2 (idleWithoutExtra (SNat @2)))
{-# OPAQUE sampled #-}
