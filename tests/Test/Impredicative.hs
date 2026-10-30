{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

A higher-rank component no eta-expansion reaches: after its pattern, the
polytype sits under no arrow. That takes @ImpredicativeTypes@ to write, and
under it GHC's Quick Look instantiates the component wrap at the polytype
directly, so the equation is wrapped as written. "Test.PluginDiagnostics"
asserts @impredicativeOut.impredicative.out@, the wire of the component that
would otherwise have no scope.
-}
module Test.Impredicative (impredicativeOut) where

import Clash.Prelude

import Clash.CircuitContext (HasCircuitContext)

impredicative ::
  (HasCircuitContext) => Signal System Int -> (Signal System Int, Maybe (forall a. a -> a))
impredicative inp = (out, Just id)
 where
  out = inp + 1
{-# OPAQUE impredicative #-}

-- | A rank-1 caller, so the suite needs no @ImpredicativeTypes@ of its own.
impredicativeOut :: (HasCircuitContext) => Signal System Int -> Signal System Int
impredicativeOut inp = fst (impredicative inp)
{-# OPAQUE impredicativeOut #-}
