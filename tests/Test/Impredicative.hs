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

The wrap works for any body because a component is always signed: GHC checks
the wrap against the signature's result type, and Quick Look takes the
instantiation from that, not from the body. The shapes below pin that for
the bodies most likely to defeat it. Each compiles without the plugin, so if a
GHC release changes Quick Look and one stops compiling with it, this module
fails to build.
-}
module Test.Impredicative (
  impredicativeOut,
  impredGuarded,
  impredTwoEqs,
  impredIf,
  impredList,
  impredLambda,
  impredPointFree,
) where

import qualified Prelude as P

import Clash.Prelude
import Data.Functor.Identity (Identity (..))

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

-- | Guards, case-encoded inside the wrap.
impredGuarded :: (HasCircuitContext) => Bool -> Maybe (forall a. a -> a)
impredGuarded c
  | c = Just id
  | otherwise = Nothing
{-# OPAQUE impredGuarded #-}

-- | Two equations, each wrapped on its own.
impredTwoEqs :: (HasCircuitContext) => Bool -> Maybe (forall a. a -> a)
impredTwoEqs True = Just id
impredTwoEqs False = Nothing
{-# OPAQUE impredTwoEqs #-}

-- | A conditional, whose branches are checked against the result type.
impredIf :: (HasCircuitContext) => Bool -> Maybe (forall a. a -> a)
impredIf c = if c then Just id else Nothing
{-# OPAQUE impredIf #-}

-- | A list of polytypes, built by an ordinary higher-order function.
impredList :: (HasCircuitContext) => Int -> [forall a. a -> a]
impredList n = P.replicate n id
{-# OPAQUE impredList #-}

-- | A constructor around a lambda.
impredLambda :: (HasCircuitContext) => Int -> Identity (forall a. a -> a)
impredLambda _ = Identity (\x -> x)
{-# OPAQUE impredLambda #-}

{- | Point-free: no pattern consumes the arrow, and the polytype past it is
still under no arrow of its own, so the equation is wrapped as written, with
no eta-expansion.
-}
impredPointFree :: (HasCircuitContext) => Int -> Maybe (forall a. a -> a)
impredPointFree = const (Just id)
{-# OPAQUE impredPointFree #-}
