{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}

{- | Class methods with @HasCircuitContext@ or @HasProbe@ (PROBES-13,
HDL-11). The plugin does not instrument the body of an instance method or a
class default, so a local binding there that it would trace (or probe) in a
top-level function is lost, and an @OPAQUE@ there does not make a component.
The plugin says so at that body, and nowhere else: not at the class
signature, which needs the constraint so that instances can call
instrumented code, and not at a body that only calls a top-level function,
which is the way to instrument a method.

@check.sh@ checks that the plugin warns about a method body on each line
marked @-- warns@ (the first line of the body), and nowhere else.
-}
module Test.MethodBodies where

import qualified Clash.Explicit.Prelude as E
import Clash.Prelude

import Clash.CircuitContext
import Test.ImportedClass (Remote (..))

type Sig = Signal System Int

-- | A component: what a method calls to be instrumented.
counter :: (HasCircuitContext) => Sig -> Sig
counter inp = acc
 where
  acc = E.register clockGen resetGen enableGen 0 (acc + inp)
{-# OPAQUE counter #-}

class Thing a where
  thing :: (HasCircuitContext) => a -> Sig -> Sig

-- | Calls a component: nothing is lost, so no warning.
instance Thing Bool where
  thing _ = counter

-- | A body with a local signal: lost.
instance Thing Int where
  thing k inp = doubled -- warns
   where
    doubled = inp * fromIntegral k

{- | Local bindings that would not be traced in a top-level function either:
a plain value, a function, a name with @_@. No warning.
-}
instance Thing () where
  thing _ inp = helper inp + _unused
   where
    two = 2 :: Int
    helper x = x * fromIntegral two
    _unused = 0

-- | A closed signal that GHC does not generalise: lost.
instance Thing Char where
  thing _ inp = inp + fmap fromEnum pulse -- warns
   where
    pulse = E.riseEvery clockGen resetGen enableGen d4

-- | An instance signature with the constraint, and a call: no warning.
instance Thing Integer where
  thing :: (HasCircuitContext) => Integer -> Sig -> Sig
  thing _ = counter

-- | OPAQUE on a method that calls a component: not a component itself.
instance Thing Word where
  thing _ = counter -- warns
  {-# OPAQUE thing #-}

-- | A class default with a local signal: lost. Its instance is quiet.
class Defaulted a where
  defaulted :: (HasCircuitContext) => a -> Sig -> Sig
  defaulted _ inp = tripled -- warns
   where
    tripled = inp * 3

instance Defaulted Bool

-- | A class default that calls a component: no warning.
class Forwarded a where
  forwarded :: (HasCircuitContext) => a -> Sig -> Sig
  forwarded _ = counter

instance Forwarded Bool

-- | A method with HasProbe: a probed step function's bindings are lost.
class Stepper a where
  stepWith :: (HasProbe) => a -> Int -> Int -> (Int, Int)

instance Stepper Bool where
  stepWith _ s i = (next, s) -- warns
   where
    next = s + i

-- | A probed method that probes by hand: no binding to lose.
instance Stepper Int where
  stepWith k s i = (probe "next" (s + i * k), s)

-- | Instances of a class from another module.
data Here = Here

data There = There

instance Remote Here where
  remote _ inp = halved -- warns
   where
    halved = inp - 2

instance Remote There where
  remote _ = counter

-- | A class without the constraints: never reported.
class Plain a where
  plain :: a -> Sig -> Sig

instance Plain Bool where
  plain _ inp = doubled
   where
    doubled = inp * 2

-- | Calls of the methods, so that the instances are used.
uses :: (HasCircuitContext) => Sig -> (Sig, Sig, Sig, Sig)
uses inp = (thing True inp, thing (3 :: Int) inp, defaulted True inp, remote Here inp)
{-# OPAQUE uses #-}
