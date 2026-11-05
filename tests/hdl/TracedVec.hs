{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}

{- | A traced vector binding: HDL generation without optimisation stopped at
"Hit specialization limit" when the elements' context was changed.
-}
module TracedVec where

import Clash.Explicit.Prelude

import Clash.CircuitContext

top :: (HasCircuitContext) => Signal System (Unsigned 8) -> Signal System (Unsigned 8)
top inp = sum ws
 where
  ws = map (+ 1) (inp :> inp :> Nil)
{-# OPAQUE top #-}

topEntity :: Signal System (Unsigned 8) -> Signal System (Unsigned 8)
topEntity inp = withoutCircuitContext (top inp)
