{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
-- One pragma: the plugins run in the order given, and a formatter sorts
-- separate pragmas.
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise
  -fplugin=GHC.TypeLits.KnownNat.Solver
  -fplugin=Clash.CircuitContext.Plugin #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Widths that @ghc-typelits-knownnat@ knows only under a side condition, or
not at all. The oracle says to record such a binding only when the
requirements of its @KnownNat@ hold. When it said yes regardless, GHC had to
find a @KnownNat@ that nothing gives, and a module that compiles without
the plugin failed with @Cannot satisfy: 1 <= n@. Compiling this module is
the test; "Main" checks what was recorded.
-}
module Test.SideConditions (
  addrOf,
  unchecked,
  checked,
  logarithm,
) where

import Clash.Prelude

import Clash.CircuitContext (HasCircuitContext)

-- | An address as wide as a table of @n@ entries needs.
addrOf :: (KnownNat n) => SNat n -> Signal System (BitVector (CLog 2 n))
addrOf _ = pure (errorX "address")
{-# OPAQUE addrOf #-}

{- | @KnownNat (CLog 2 n)@ needs @1 <= n@, which the signature does not
give: @addr@ is not recorded, and the plugin says the oracle could not
decide @1 <= n@.
-}
unchecked ::
  (HasCircuitContext, KnownNat n) => SNat n -> Signal System (BitVector (CLog 2 n))
unchecked s = addr
 where
  addr = addrOf s
{-# OPAQUE unchecked #-}

-- | The same with @1 <= n@ in the signature: @addr@ is recorded.
checked ::
  (HasCircuitContext, KnownNat n, 1 <= n) =>
  SNat n ->
  Signal System (BitVector (CLog 2 n))
checked s = addr
 where
  addr = addrOf s
{-# OPAQUE checked #-}

{- | @ghc-typelits-knownnat@ has no instance for 'Log2' at all, so @KnownNat
(Log2 n)@ holds for no @n@ it does not know: @bits@ is not recorded.
-}
logarithm ::
  (HasCircuitContext, KnownNat n) => SNat n -> Signal System (BitVector (Log2 n))
logarithm s = bits
 where
  bits = bitsOf s
{-# OPAQUE logarithm #-}

-- | As many bits as the logarithm of @n@.
bitsOf :: (KnownNat n) => SNat n -> Signal System (BitVector (Log2 n))
bitsOf _ = pure (errorX "bits")
{-# OPAQUE bitsOf #-}
