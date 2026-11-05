{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}

{- | A module WITHOUT the plugin that calls an instrumented function: its
calls are on the call path all the same, because GHC adds their sites to
the call stack that 'HasCircuitContext' asks for.
-}
module Test.SourceOrder.Plain (plainCalls) where

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Test.SourceOrder.Acc (acc)

-- | Two calls of 'acc' in a scope written by hand.
plainCalls ::
  (HasCircuitContext) => (Signal System (Unsigned 8), Signal System (Unsigned 8))
plainCalls = component "plainCalls" (acc 1, acc 100)
