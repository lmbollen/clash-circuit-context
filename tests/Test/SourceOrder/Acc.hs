{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}

{- | The components that "Test.SourceOrder" and "Test.SourceOrder.Plain"
call: one that the plugin splits, and functions of the shapes it splits with
care or does not split.
-}
module Test.SourceOrder.Acc (
  acc,
  accBy,
  accByImplicit,
  (<+>),
  Stage (..),
  annotated,
) where

import Clash.Explicit.Prelude

import Clash.CircuitContext

{- | A counter that adds @step@ each cycle after the reset: a plugin-made
component.
-}
acc :: (HasCircuitContext) => Unsigned 8 -> Signal System (Unsigned 8)
acc step = total
 where
  total = register clockGen resetGen enableGen 0 (total + pure step)
{-# OPAQUE acc #-}

{- | 'acc' with a step that only a type application gives: the type of
@accBy@ is ambiguous without one. The wrapper the plugin makes calls the
worker with the same type application.
-}
accBy :: forall n. (HasCircuitContext, KnownNat n) => Signal System (Unsigned 8)
accBy = acc (natToNum @n)
{-# OPAQUE accBy #-}

{- | A type that is ambiguous without an explicit @forall@, whose body does
not need its variable.
-}
accByImplicit :: (HasCircuitContext, KnownNat n) => Signal System (Unsigned 8)
accByImplicit = acc 1
{-# OPAQUE accByImplicit #-}

-- | 'accBy' as an operator, defined infix.
(<+>) ::
  forall n.
  (HasCircuitContext, KnownNat n) =>
  Unsigned 8 ->
  Unsigned 8 ->
  Signal System (Unsigned 8)
a <+> b = acc (a + b + natToNum @n)
{-# OPAQUE (<+>) #-}

-- | A class method: the plugin does not split it.
class Stage a where
  stage :: (HasCircuitContext) => a -> Signal System (Unsigned 8)

instance Stage Bool where
  stage up = acc (if up then 1 else 0)

-- | A function with an annotation: the plugin does not split it.
annotated :: (HasCircuitContext) => Unsigned 8 -> Signal System (Unsigned 8)
annotated = acc
{-# ANN annotated "HLint: ignore Eta reduce" #-}
