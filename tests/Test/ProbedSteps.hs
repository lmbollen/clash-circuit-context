{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

{- | Step functions given to 'mealyProbed' and its friends, with and without
@HasProbe@ (PROBES-7). A step function whose type has no @HasProbe@ probes
nothing: the plugin does not probe its local bindings, and it cannot call
'probe'. That used to be silent.

@check.sh@ checks that the plugin says "probes nothing" on each line marked
@-- warns@ (the line of the step function), and nowhere else. Every other
call here probes, or asks for probes with @HasProbe@ in the signature of its
step function, and a warning about it is a false one: a step function that
is partly applied, composed, imported, a class method, a record field, behind
a constraint synonym, passed through a wrapper of the designer's, or one that
calls 'probe' by hand.
-}
module Test.ProbedSteps where

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Test.ImportedSteps (importedProbedStep, importedStep)

type Clk = Clock System
type Rst = Reset System
type Sig = Signal System Int

--------------------------------------------------------------------------------
-- Step functions

-- | With @HasProbe@: the plugin probes @acc@.
probedStep :: (HasProbe) => Int -> Int -> (Int, Int)
probedStep s i = (acc, s)
 where
  acc = s + i

-- | With @HasProbe@ and nothing to probe. It asks for probes, so it is quiet.
emptyProbedStep :: (HasProbe) => Int -> Int -> (Int, Int)
emptyProbedStep s i = (s + i, s)

-- | Without @HasProbe@: @acc@ is not probed.
plainStep :: Int -> Int -> (Int, Int)
plainStep s i = (acc, s)
 where
  acc = s + i

-- | With @HasProbe@, on a pair: for 'curry'.
probedPairStep :: (HasProbe) => (Int, Int) -> (Int, Int)
probedPairStep (s, i) = (acc, s)
 where
  acc = s + i

-- | With @HasProbe@ and a parameter before the state: partly applied.
scaledStep :: (HasProbe) => Int -> Int -> Int -> (Int, Int)
scaledStep k s i = (acc, s)
 where
  acc = k * s + i

-- | @HasProbe@ behind a constraint synonym.
type StepCtx a = (HasProbe, Num a)

synonymStep :: (StepCtx a) => a -> a -> (a, a)
synonymStep s i = (acc, s)
 where
  acc = s + i

-- | A class method with @HasProbe@.
class Stepper a where
  stepWith :: (HasProbe) => a -> Int -> Int -> (Int, Int)

instance Stepper Bool where
  stepWith b s i = if b then probedStep s i else (s, i)

-- | A record field of the type @(HasProbe) => …@.
newtype Machine = Machine {machineStep :: (HasProbe) => Int -> Int -> (Int, Int)}

{- | A combinator without @HasProbe@ around a step function (a design forces
its state this way).
-}
strictState :: (s -> i -> (s, o)) -> s -> i -> (s, o)
strictState f s i = case f s i of
  (s', o) -> s' `seq` (s', o)

--------------------------------------------------------------------------------
-- A step function that probes nothing: one warning each

-- | A local step function without a signature (the reviewer's @forgot@).
forgot :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
forgot clk rst inp = out
 where
  out = mealyProbed clk rst enableGen step 0 inp -- warns
  step s i = (acc, s)
   where
    acc = s + i
{-# OPAQUE forgot #-}

-- | A top-level step function without @HasProbe@.
forgotTop :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
forgotTop clk rst = mealyProbed clk rst enableGen plainStep 0 -- warns
{-# OPAQUE forgotTop #-}

-- | An imported step function without @HasProbe@.
forgotImported :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
forgotImported clk rst = mealyProbed clk rst enableGen importedStep 0 -- warns
{-# OPAQUE forgotImported #-}

-- | A lambda that does not probe.
forgotLambda :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
forgotLambda clk rst = mealyProbed clk rst enableGen (\s i -> (s + i, s)) 0 -- warns
{-# OPAQUE forgotLambda #-}

-- | An operator, to 'mooreProbed'.
forgotMoore :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
forgotMoore clk rst = mooreProbed clk rst enableGen (+) id 0 -- warns
{-# OPAQUE forgotMoore #-}

-- | The bundled forms.
forgotBundled :: (HasCircuitContext) => Clk -> Rst -> Sig -> (Sig, Sig)
forgotBundled clk rst inp = (a, b)
 where
  a = mealyBProbed clk rst enableGen plainStep 0 inp -- warns
  b = mooreBProbed clk rst enableGen (+) id 0 inp -- warns
{-# OPAQUE forgotBundled #-}

-- | A function to 'probeFmap' that does not probe.
forgotFmap :: (HasCircuitContext) => Clk -> Sig -> Sig
forgotFmap clk = probeFmap clk (fst . plainStep 1) -- warns
{-# OPAQUE forgotFmap #-}

-- | Through @$@.
forgotDollar :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
forgotDollar clk rst = (mealyProbed clk rst enableGen $ plainStep) 0 -- warns
{-# OPAQUE forgotDollar #-}

-- | A combinator without @HasProbe@ around a step function without it.
forgotStrict :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
forgotStrict clk rst = mealyProbed clk rst enableGen (strictState plainStep) 0 -- warns
{-# OPAQUE forgotStrict #-}

--------------------------------------------------------------------------------
-- A step function that probes, or asks for probes: no warning

probed :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
probed clk rst = mealyProbed clk rst enableGen probedStep 0
{-# OPAQUE probed #-}

-- | Nothing to probe, but the signature asks for probes.
probedEmpty :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
probedEmpty clk rst = mealyProbed clk rst enableGen emptyProbedStep 0
{-# OPAQUE probedEmpty #-}

-- | A local step function with a signature.
probedLocal :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
probedLocal clk rst = mealyProbed clk rst enableGen step 0
 where
  step :: (HasProbe) => Int -> Int -> (Int, Int)
  step s i = (acc, s)
   where
    acc = s + i
{-# OPAQUE probedLocal #-}

-- | A lambda that calls 'probe' by hand.
probedByHand :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
probedByHand clk rst = mealyProbed clk rst enableGen (\s i -> (probe "sum" (s + i), s)) 0
{-# OPAQUE probedByHand #-}

{- | A local step function without a signature that calls 'probe' by hand:
GHC generalises it over @?probe@.
-}
probedByHandLocal :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
probedByHandLocal clk rst = mealyProbed clk rst enableGen step 0
 where
  step s i = (probe "sum" (s + i), s)
{-# OPAQUE probedByHandLocal #-}

probedPartly :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
probedPartly clk rst = mealyProbed clk rst enableGen (scaledStep 3) 0
{-# OPAQUE probedPartly #-}

probedComposed :: (HasCircuitContext) => Clk -> Rst -> Sig -> (Sig, Sig, Sig)
probedComposed clk rst inp = (a, b, c)
 where
  a = mealyProbed clk rst enableGen (\s -> probedStep s . (+ 1)) 0 inp
  b = mealyProbed clk rst enableGen (flip probedStep) 0 inp
  c = mealyProbed clk rst enableGen (curry probedPairStep) 0 inp
{-# OPAQUE probedComposed #-}

probedImported :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
probedImported clk rst = mealyProbed clk rst enableGen importedProbedStep 0
{-# OPAQUE probedImported #-}

probedSynonym :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
probedSynonym clk rst = mealyProbed clk rst enableGen synonymStep 0
{-# OPAQUE probedSynonym #-}

probedMethod :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
probedMethod clk rst = mealyProbed clk rst enableGen (stepWith True) 0
{-# OPAQUE probedMethod #-}

probedField :: (HasCircuitContext) => Machine -> Clk -> Rst -> Sig -> Sig
probedField m clk rst = mealyProbed clk rst enableGen (machineStep m) 0
{-# OPAQUE probedField #-}

probedIf :: (HasCircuitContext) => Bool -> Clk -> Rst -> Sig -> Sig
probedIf b clk rst = mealyProbed clk rst enableGen (if b then probedStep else flip probedStep) 0
{-# OPAQUE probedIf #-}

probedDollar :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
probedDollar clk rst = (mealyProbed clk rst enableGen $ probedStep) 0
{-# OPAQUE probedDollar #-}

-- | A combinator without @HasProbe@ around a step function with it.
probedStrict :: (HasCircuitContext) => Clk -> Rst -> Sig -> Sig
probedStrict clk rst = mealyProbed clk rst enableGen (strictState probedStep) 0
{-# OPAQUE probedStrict #-}

probedBundled :: (HasCircuitContext) => Clk -> Rst -> Sig -> (Sig, Sig)
probedBundled clk rst inp = (a, b)
 where
  a = mealyBProbed clk rst enableGen probedStep 0 inp
  b = mooreBProbed clk rst enableGen (\s i -> fst (probedStep s i)) id 0 inp
{-# OPAQUE probedBundled #-}

probedFmap :: (HasCircuitContext) => Clk -> Sig -> Sig
probedFmap clk = probeFmap clk (\x -> probe "next" (x + 1))
{-# OPAQUE probedFmap #-}

{- | A wrapper of the designer's around 'mealyProbed' that passes its own
@HasProbe@ parameter on, directly and in a lambda.
-}
myMealy ::
  (HasCircuitContext) =>
  Clk -> Rst -> ((HasProbe) => Int -> Int -> (Int, Int)) -> Sig -> Sig
myMealy clk rst f = mealyProbed clk rst enableGen f 0

myMealyLambda ::
  (HasCircuitContext) =>
  Clk -> Rst -> ((HasProbe) => Int -> Int -> (Int, Int)) -> Sig -> Sig
myMealyLambda clk rst f = mealyProbed clk rst enableGen (\s i -> f s i) 0

viaWrapper :: (HasCircuitContext) => Clk -> Rst -> Sig -> (Sig, Sig)
viaWrapper clk rst inp = (myMealy clk rst probedStep inp, myMealyLambda clk rst probedStep inp)
{-# OPAQUE viaWrapper #-}
