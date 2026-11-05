{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}

{- | Calls of one OPAQUE component in several shapes: Clash must make one
module of it, @acc@, whatever the calls.
-}
module Calls where

import Clash.Explicit.Prelude

import Clash.CircuitContext

acc ::
  (HasCircuitContext) =>
  Clock System ->
  Reset System ->
  Signal System (Unsigned 8) ->
  Signal System (Unsigned 8)
acc clk rst step = total
 where
  total = register clk rst enableGen 0 (total + step)
{-# OPAQUE acc #-}

viaHelper ::
  (HasCircuitContext) =>
  Clock System ->
  Reset System ->
  Signal System (Unsigned 8) ->
  Signal System (Unsigned 8)
viaHelper clk rst step = acc clk rst step
{-# ANN viaHelper NoCircuitScope #-}

top ::
  (HasCircuitContext) =>
  Clock System ->
  Reset System ->
  Signal System (Unsigned 8) ->
  Signal System (Unsigned 8)
top clk rst inp = first + second + third + fourth + sum (map (acc clk rst) (inp :> (inp + 2) :> Nil))
 where
  first = acc clk rst inp
  second = acc clk rst (inp + 1)
  third = viaHelper clk rst (inp + 3)
  fourth = id $ acc clk rst (inp + 4)
{-# OPAQUE top #-}

topEntity ::
  Clock System -> Reset System -> Signal System (Unsigned 8) -> Signal System (Unsigned 8)
topEntity clk rst inp = withoutCircuitContext (top clk rst inp)
