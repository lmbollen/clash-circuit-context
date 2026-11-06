{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}

-- | The component that "Test.SourceOrder" and "Test.SourceOrder.Plain" call.
module Test.SourceOrder.Acc (acc) where

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
