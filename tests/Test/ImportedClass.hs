{-# LANGUAGE ImplicitParams #-}

{- | A class whose method asks for @HasCircuitContext@, for
"Test.MethodBodies" to give instances of from ANOTHER module: there the
plugin reads the method's type from the interface.
-}
module Test.ImportedClass (Remote (..)) where

import Clash.Prelude

import Clash.CircuitContext (HasCircuitContext)

class Remote a where
  remote :: (HasCircuitContext) => a -> Signal System Int -> Signal System Int
