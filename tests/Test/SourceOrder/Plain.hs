{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}

{- | A module WITHOUT the plugin that calls an instrumented function: its
calls are on the call path all the same, because GHC adds their sites to
the call stack that 'HasCircuitContext' asks for.
-}
module Test.SourceOrder.Plain (plainCalls, plainRoots) where

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Test.SourceOrder.Acc (Stage (..), acc, accBy, accByImplicit, annotated, (<+>))

-- | Two calls of 'acc' in a scope written by hand.
plainCalls ::
  (HasCircuitContext) => (Signal System (Unsigned 8), Signal System (Unsigned 8))
plainCalls = component "plainCalls" (acc 1, acc 100)

{- | Calls from code that does not ask for 'HasCircuitContext', as a test
suite makes them: 'withoutCircuitContext' gives it @CircuitContextOnly@ only.
Of a function that the plugin splits, of ones whose type is ambiguous
without a type application, of a class method and of a function with an
annotation, which the plugin does not split.
-}
plainRoots :: [[Unsigned 8]]
plainRoots =
  [ sampleN 3 (withoutCircuitContext (acc 1))
  , sampleN 3 (withoutCircuitContext (accBy @2))
  , sampleN 3 (withoutCircuitContext (accByImplicit @2))
  , sampleN 3 (withoutCircuitContext ((<+>) @0 1 2))
  , sampleN 3 (withoutCircuitContext (stage True))
  , sampleN 3 (withoutCircuitContext (annotated 4))
  ]
