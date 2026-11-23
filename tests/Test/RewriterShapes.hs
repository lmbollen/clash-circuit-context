{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

{- | Shapes from a downstream design (bittide) where the oracle must NOT
decide in GHC's rewriter. In each, a given holds the same flag as the
wanted: the constraint that GHC quantified over when it generalised a local
binding. GHC solves the wanted from that given, and the solver stage never
sees it. A flag that the rewriter decides no longer matches the given, so
the wanted then needs an instance: a yes that the oracle gives too readily
breaks the build (@addressed@), and a no drops a binding that the caller
records, and warns about it. So the rewriter decides no flag that a given
holds, and only decides a yes.

@check.sh@ checks that the plugin warns exactly once on each line marked
@-- warns@, and nowhere else: those are the warnings that the solver stage
gives. Any other warning is a negative that the rewriter decided early. In
these shapes it came from GHC's check of an inferred type, which solves the
type's own constraints from a copy of them; the binding itself was still
recorded, but the warning was false.
-}
module Test.RewriterShapes (
  countTo,
  stepped,
  ports,
  generalisedPorts,
  masks,
  addressed,
) where

import qualified Prelude as P

import Data.Tagged (Tagged (..))
import qualified GHC.TypeLits as TypeLits

import Clash.Explicit.Prelude

import Clash.CircuitContext

{- | A local function that GHC generalises, of a value: GHC solves the
wanted of @next@ from the given it quantified over. Asking for @Traceable
b@ instead finds nothing. No warning.
-}
countTo ::
  (HasCircuitContext) => Signal System (Maybe (Index 8)) -> Signal System (Index 8)
countTo = mealy clockGen resetGen enableGen goCount 0
 where
  goCount current0 clearAt = (next, current0)
   where
    next = satSucc SatWrap current1
    current1
      | Just clear <- clearAt, clear P.== current0 = 0
      | P.otherwise = current0
{-# OPAQUE countTo #-}

{- | The same at a signal type: @next@ is recorded at each call of @step@.
No warning.
-}
stepped :: (HasCircuitContext) => Signal System Bool -> Signal System (Unsigned 4)
stepped en = counter
 where
  counter = register clockGen resetGen enableGen 0 (step en counter)
  step e current0 = next
   where
    next = mux e (current0 + 1) current0
{-# OPAQUE stepped #-}

-- | A port type, as circuit-notation tags the signals of a circuit.
data Port a

{- | Two ports of one type, bound by one pattern, of a payload without
'BitPack'. Both share one wanted, so the oracle warns once, about @a@.
-}
ports ::
  forall dom a.
  (HasCircuitContext, KnownDomain dom) =>
  Signal dom a -> Signal dom a -> (Signal dom a, Signal dom a)
ports x0 y0 = (unTagged a, unTagged b)
 where
  (a, b) = (Tagged @(Port a) x0, Tagged @(Port a) y0) -- warns
{-# OPAQUE ports #-}

{- | The same ports, bound in a local function that GHC generalises. The
oracle warns once, at the call of @go@, where the payload is known.
-}
generalisedPorts ::
  forall dom a.
  (HasCircuitContext, KnownDomain dom) =>
  Signal dom a -> Signal dom a -> (Signal dom a, Signal dom a)
generalisedPorts x0 y0 = go x0 y0 -- warns
 where
  go x y = (unTagged a, unTagged b)
   where
    (a, b) = (Tagged @(Port Bool) x, Tagged @(Port Bool) y)
{-# OPAQUE generalisedPorts #-}

-- | A type whose 'BitPack' instance is a 'TypeLits.TypeError'.
data Mask (n :: Nat) = Mask (BitVector n)
  deriving (Generic, NFDataX)

instance
  (KnownNat n, TypeLits.TypeError ('TypeLits.Text "Mask has no BitPack instance")) =>
  BitPack (Mask n)
  where
  type BitSize (Mask n) = n
  pack = P.error "unreachable"
  unpack = P.error "unreachable"

{- | A mask made in a local function that GHC generalises. The oracle
cannot decide at the call of @toMask@, and says so once, there.
-}
masks ::
  forall n dom.
  (HasCircuitContext, KnownDomain dom, KnownNat n) =>
  Signal dom (Vec n Bool) -> Signal dom (BitVector n)
masks bits = (\(Mask bv) -> bv) <$> toMask bits -- warns
 where
  toMask vs = maskS
   where
    maskS = Mask . pack <$> vs
{-# OPAQUE masks #-}

-- | An address as wide as a table of @n@ entries needs.
addrOf :: (KnownNat n) => SNat n -> Signal System (BitVector (CLog 2 n))
addrOf _ = pure (errorX "address")
{-# OPAQUE addrOf #-}

{- | A local function that GHC generalises, of an address width that has a
side condition: @KnownNat (CLog 2 n)@ holds only where @1 <= n@ does. GHC
checks the inferred type of @table@ by solving its constraints from a copy
of them, and the oracle must leave the wanted that records @addr@ to that
copy. No warning.
-}
addressed :: (HasCircuitContext) => Signal System (BitVector (CLog 2 4))
addressed = table (SNat @4)
 where
  table s = addr
   where
    addr = addrOf s
{-# OPAQUE addressed #-}
