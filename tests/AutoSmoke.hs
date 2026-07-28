{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

{- | Phase 3 golden test: the smoke design written NATURALLY — no manual
'component' \/ 'traceSignalC' \/ 'probe' calls anywhere. The plugin
instruments everything:

* @acc@: OPAQUE + 'HasCircuitContext' ⇒ auto-wrapped in @component "acc"@; its
  where-binding @total@ auto-traced under the pushed scope (where→let
  move); its step function's 'HasProbe' signature switches @next@ to
  auto-probing (innermost signature wins).
* @top@: OPAQUE + 'HasCircuitContext' ⇒ @component "top"@; @out@ auto-traced; two
  @acc@ instances disambiguate downstream; @_hidden@ opts out by name.
* @guarded@: guarded body ending in @otherwise@ ⇒ still a component (the
  plugin case-encodes the guards inside the wrap).
* @partialGuard@: guards without a final otherwise on the LAST (only)
  equation ⇒ still a component: with no later equation to fall through to,
  guard failure was already bottom.
* @fallthrough@: non-exhaustive guards on a NON-final equation ⇒ that
  equation's wrap is skipped with a warning (case-encoding would turn the
  fall-through into a crash); its final equation is wrapped independently.
  Skipped-equation bindings trace under the CALLER's scope, since the
  @?circuitContext@ dictionary flows from the call site inside @top@.
* @pair@ (in @top@): a record of signals whose 'Traceable' instance is
  derived generically ('Generic' + an empty instance) ⇒ the binding
  auto-traces field-wise, each field a @pair.\<field\>@ sub-scope wire.
* @(accA, accB)@ (in @top@): a tuple PATTERN binding ⇒ each binder is
  renamed fresh in the pattern and traced via a sibling
  @accA = autoTrace "accA" accA'@ binding.
* @polyHelper@: F9 regression — its CLOSED local @quiet@ (a polymorphic
  helper used at two types) must be left unwrapped, or the module does not
  even compile; its open locals still trace.
-}
module Main where

import qualified Prelude as P

import Control.Exception (evaluate)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)

import Clash.Explicit.Prelude

import Clash.CircuitContext

-- | Accumulator: component "acc"; probes its pre-registration sum.
acc :: (HasCircuitContext) => Signal System Int -> Signal System Int
acc inp = total
 where
  total = mealyProbed clockGen resetGen enableGen step 0 inp
  step :: (HasProbe) => Int -> Int -> (Int, Int)
  step s i = (next, s)
   where
    next = prev + 1 + i
    prev = s - 1
{-# OPAQUE acc #-}

{- | Guarded body ending in @otherwise@: still a component — the plugin
case-encodes the guards inside the wrap.
-}
guarded :: (HasCircuitContext) => Bool -> Signal System Int -> Signal System Int
guarded b inp
  | b = pos
  | otherwise = neg
 where
  pos = adder inp 1
  neg = inp - 1
{-# OPAQUE guarded #-}

adder = (+)
{-# OPAQUE adder #-}

{- | Partial guards, but the last (only) equation: still safely wrapped —
guard failure was bottom before and after.
-}
partialGuard :: (HasCircuitContext) => Int -> Signal System Int -> Signal System Int
partialGuard k inp
  | k P.> 0 = out2
 where
  out2 = inp + 3
{-# OPAQUE partialGuard #-}

{- | Non-exhaustive guards on a non-final equation: that equation's wrap is
skipped (warning); the last equation still becomes a component.
-}
fallthrough :: (HasCircuitContext) => Int -> Signal System Int -> Signal System Int
fallthrough k inp
  | k P.> 9000 = out3
 where
  out3 = inp + 100
fallthrough _ inp = inp + 4
{-# OPAQUE fallthrough #-}

{- | F9 regression: @quiet@ is a CLOSED binding (no free local variables) —
GHC generalizes it, and it is used at two different payload types below.
The plugin must leave it alone (see @closedBind@ in the renamer): wrapping
it in @autoTrace@ would destroy the generalization (the @CanTrace@ family
application in the injected constraint is not quantifiable), monomorphize
it at its first use, and turn the second use into a type error — exactly
the failure bittide's @timeWb@ hit through its @noWrite = pure Nothing@.
Compiling at all is the main assertion; the open locals still trace.
-}
polyHelper :: (HasCircuitContext) => Signal System Int -> Signal System Int
polyHelper inp = out4
 where
  out4 = inp + (maybe 0 fromEnum <$> asBool) + (fromMaybe 0 <$> asInt)
  asBool = quiet :: Signal System (Maybe Bool)
  asInt = quiet :: Signal System (Maybe Int)
  quiet = pure Nothing
{-# OPAQUE polyHelper #-}

{- | A record of signals: 'Traceable' derived generically ('Generic' plus an
empty instance). A binding of this type auto-traces field-wise.
-}
data Pair dom = Pair
  { pFst :: Signal dom Int
  , pSnd :: Signal dom Int
  }
  deriving (Generic)

instance (KnownDomain dom) => Traceable (Pair dom)

{- | 'probeFmap' coverage: a COMBINATIONAL function lifted over a signal, whose
internal @where@ bindings the plugin auto-probes (it carries 'HasProbe'). This
is the @route \<$\> …@ idiom — no mealy, no 'Signal'-typed locals — so neither
'traceSignalC' nor a bare 'mealyProbed' would reach @doubled@\/@negated@.
-}
probeFmapDemo :: (HasCircuitContext) => Signal System Int -> Signal System Int
probeFmapDemo = probeFmap clockGen step
 where
  step :: (HasProbe) => Int -> Int
  step x = doubled + negated
   where
    doubled = x + x
    negated = negate x
{-# OPAQUE probeFmapDemo #-}

{- | 'mealyBProbed' coverage: the bundled-I/O mealy idiom (@mealyB@), whose step
internals the plugin auto-probes.
-}
mealyBDemo :: (HasCircuitContext) => Signal System Int -> Signal System Int
mealyBDemo inp = out
 where
  (out, _dbg) = mealyBProbed clockGen resetGen enableGen stepB (0 :: Int) (inp, pure 1)
  stepB :: (HasProbe) => Int -> (Int, Int) -> (Int, (Int, Int))
  stepB s (i, k) = (s', (s, s'))
   where
    s' = s + i + k
{-# OPAQUE mealyBDemo #-}

top :: (HasCircuitContext) => Signal System Int -> Signal System Int
top inp = out
 where
  out =
    accA
      + accB
      + guarded True inp
      + partialGuard 1 inp
      + fallthrough 1 inp
      + polyHelper inp
  (accA, accB) = (pFst pair, pSnd pair)
  pair = Pair (acc inp) (acc (inp + 1))
  _hidden = acc (inp + 2) -- opt-out; also never forced
{-# OPAQUE top #-}

check :: String -> Bool -> IO ()
check what ok
  | ok = putStrLn ("ok: " <> what)
  | otherwise = putStrLn ("FAIL: " <> what) >> exitFailure

main :: IO ()
main = do
  (samples, traces, probes) <- withCircuitContext $ do
    let xs = sampleN 8 (top (fromList [1, 1 ..]))
    _ <- evaluate (deepseqX xs xs)
    pure xs
  putStrLn ("samples: " <> show samples)
  putStrLn ("traces:  " <> show (Map.keys traces))
  putStrLn ("probes:  " <> show (Map.keys probes))
  let
    tks = Map.keys traces
    pks = Map.keys probes
    hasT nm = P.any (nm `isInfixOf`) tks
    countT nm = P.length (P.filter (nm `isInfixOf`) tks)
    countP nm = P.length (P.filter (nm `isInfixOf`) pks)
  -- acc(1s) = [0,0,1..6]; acc(2s) = [0,0,2..12]; guarded = 2s; partial = 4s;
  -- fallthrough (k=1 takes the second equation) = 5s; polyHelper = 1s
  check "samples" (samples P.== [12, 12, 15, 18, 21, 24, 27, 30])
  check "top.out traced" (hasT "top@" P.&& hasT ".out@")
  check
    "record binding traced field-wise (generic Traceable)"
    (hasT ".pair.pFst@" P.&& hasT ".pair.pSnd@")
  check
    "tuple pattern binders traced (sibling autoTrace binds)"
    (hasT ".accA@" P.&& hasT ".accB@")
  check "two acc component instances" (countT ".total@" P.== 2)
  check "step probes per instance" (countP ".next@" P.== 2 P.&& countP ".prev@" P.== 2)
  check
    "guarded is a component containing pos"
    (P.any (\k -> "guarded@" `isInfixOf` k P.&& ".pos@" `isInfixOf` k) tks)
  check
    "partialGuard is a component containing out2 (last-equation rule)"
    (P.any (\k -> "partialGuard@" `isInfixOf` k P.&& ".out2@" `isInfixOf` k) tks)
  check "opt-out honored" (P.not (hasT "hidden"))
  check
    "polyHelper open locals traced at both use types"
    (hasT ".asBool@" P.&& hasT ".asInt@" P.&& hasT ".out4@")
  check "closed polymorphic local skipped (F9)" (P.not (hasT "quiet"))
  -- probeFmap / mealyBProbed coverage (separate runs; not in the golden VCD).
  (_, _, pfProbes) <- withCircuitContext $ do
    let xs = sampleN 4 (probeFmapDemo (fromList [3, 3 ..]))
    _ <- evaluate (deepseqX xs xs)
    pure xs
  let pfKeys = Map.keys pfProbes
  check
    "probeFmap probes combinational internals"
    (P.any (".doubled@" `isInfixOf`) pfKeys P.&& P.any (".negated@" `isInfixOf`) pfKeys)
  (_, _, mbProbes) <- withCircuitContext $ do
    let xs = sampleN 4 (mealyBDemo (fromList [1, 1 ..]))
    _ <- evaluate (deepseqX xs xs)
    pure xs
  check
    "mealyBProbed probes step internals"
    (P.any (".s'@" `isInfixOf`) (Map.keys mbProbes))
  vcd <- dumpVCDC (0, 8) traces probes
  case vcd of
    Left err -> putStrLn ("VCD error: " <> err) >> exitFailure
    Right txt -> do
      TIO.writeFile "auto-smoke.vcd" txt
      putStrLn "auto-smoke passed"
