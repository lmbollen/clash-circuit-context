{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

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

import Control.Exception (SomeException, evaluate, throwIO, try)
import Data.List (isInfixOf, sort)
import Data.Maybe (fromMaybe)
import System.Exit (exitFailure)

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.CircuitContext.Recording
import Clash.Signal (HiddenReset, withReset)

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

{- | 'partialGuard', defined infix: the guards' local equation is printed
prefix, as it has no patterns to put the name between.
-}
infixPartial :: (HasCircuitContext) => Int -> Signal System Int -> Signal System Int
k `infixPartial` inp
  | k P.> 0 = out5
 where
  out5 = inp + 5
{-# OPAQUE infixPartial #-}

{- | 'partialGuard' with a higher-rank right-hand side: the plugin
eta-expands the equation (one extra argument for the constrained @r@), so
its guards must take that argument too. Nothing reads @rst@, so it is not
recorded; the samples show the expansion works.
-}
partialRank ::
  (HasCircuitContext, KnownDomain dom) => Int -> Reset dom -> ((HiddenReset dom) => r) -> r
partialRank k reset
  | k P.> 0 = withReset rst
 where
  rst = reset
{-# OPAQUE partialRank #-}

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

{- | Run an action under a recording context, rethrowing what it throws, and
return its result with the recording.
-}
recorded :: String -> ((HasCircuitContext) => IO a) -> IO (a, Recording)
recorded spill k = do
  (r, rec) <- withRecording spill k
  either throwIO (\a -> pure (a, rec)) r

-- | The keys of the recorded signals of one kind, in order.
keysOf :: Kind -> Recording -> [String]
keysOf kind rec = sort [signalKey s | s <- recordedSignals rec, signalKind s P.== kind]

main :: IO ()
main = do
  (samples, rec) <- recorded "auto-instrumentation.spill" $ do
    let xs = sampleN 8 (top (fromList [1, 1 ..]))
    evaluate (deepseqX xs xs)
  putStrLn ("samples: " <> show samples)
  putStrLn ("traces:  " <> show (keysOf TraceKind rec))
  putStrLn ("probes:  " <> show (keysOf ProbeKind rec))
  let
    tks = keysOf TraceKind rec
    pks = keysOf ProbeKind rec
    hasT nm = P.any (nm `isInfixOf`) tks
    countT nm = P.length (P.filter (nm `isInfixOf`) tks)
    countP nm = P.length (P.filter (nm `isInfixOf`) pks)
  -- acc(1s) = [0,0,1..6]; acc(2s) = [0,0,2..12]; guarded = 2s; partial = 4s;
  -- fallthrough (k=1 takes the second equation) = 5s; polyHelper = 1s
  check "samples" (samples P.== [12, 12, 15, 18, 21, 24, 27, 30])
  check "top.out traced" (hasT "top@" P.&& hasT ".out@")
  check
    "record binding traced field-wise (generic Traceable)"
    (hasT ".pair_pFst@" P.&& hasT ".pair_pSnd@")
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
  -- A last equation whose guards can all fail is still wrapped (failure was
  -- bottom before and after). It must stay the SAME bottom: the failure names
  -- the function, not the plugin's encoding of its guards (#13).
  let failsInFunction nm act = do
        failed <- try (evaluate act) :: IO (Either SomeException (Signal System Int))
        check
          (nm <> "'s guard failure names the function, not the encoding")
          ( either
              (isInfixOf ("Non-exhaustive patterns in function " <> nm) . show)
              (P.const P.False)
              failed
          )
  failsInFunction "partialGuard" (withoutCircuitContext (partialGuard 0 (pure 1)))
  failsInFunction "infixPartial" (withoutCircuitContext (0 `infixPartial` pure 1))
  failsInFunction
    "partialRank"
    (withoutCircuitContext (partialRank 0 (resetGen @System) (pure 1)))
  (ipSamples, ipRec) <- recorded "auto-instrumentation-ip.spill" $ do
    let xs =
          sampleN 4 (1 `infixPartial` fromList [1, 1 ..])
            P.<> sampleN 4 (partialRank 1 (resetGen @System) (fromList [1, 1 ..]))
    evaluate (deepseqX xs xs)
  discardRecording ipRec
  check "infixPartial and partialRank sample" (ipSamples P.== [6, 6, 6, 6, 1, 1, 1, 1])
  check
    "infixPartial is a component containing out5 (last-equation rule)"
    ( P.any
        (\k -> "infixPartial@" `isInfixOf` k P.&& ".out5@" `isInfixOf` k)
        (keysOf TraceKind ipRec)
    )
  check
    "polyHelper open locals traced at both use types"
    (hasT ".asBool@" P.&& hasT ".asInt@" P.&& hasT ".out4@")
  check "closed polymorphic local skipped (F9)" (P.not (hasT "quiet"))
  -- probeFmap / mealyBProbed coverage (separate runs; not in the golden VCD).
  (_, pfRec) <- recorded "auto-instrumentation-pf.spill" $ do
    let xs = sampleN 4 (probeFmapDemo (fromList [3, 3 ..]))
    evaluate (deepseqX xs xs)
  let pfKeys = keysOf ProbeKind pfRec
  discardRecording pfRec
  check
    "probeFmap probes combinational internals"
    (P.any (".doubled@" `isInfixOf`) pfKeys P.&& P.any (".negated@" `isInfixOf`) pfKeys)
  (_, mbRec) <- recorded "auto-instrumentation-mb.spill" $ do
    let xs = sampleN 4 (mealyBDemo (fromList [1, 1 ..]))
    evaluate (deepseqX xs xs)
  discardRecording mbRec
  check
    "mealyBProbed probes step internals"
    (P.any (".s'@" `isInfixOf`) (keysOf ProbeKind mbRec))
  -- This module never mentions clash-shockwaves, so its Waveform instances
  -- reach the oracle only through the plugin's force-load of their home
  -- interface. Without it every wire still traces, as raw bits, which the
  -- golden VCD cannot tell apart; only the descriptor can.
  check
    "an Int wire is described without importing clash-shockwaves"
    ( P.any
        (\s -> ".out@" `isInfixOf` signalKey s P.&& signalDescribed s)
        [s | s <- recordedSignals rec, signalKind s P.== TraceKind]
    )
  vcd <- writeVCD (0, 8) rec "auto-instrumentation.vcd"
  discardRecording rec
  case vcd of
    Left err -> putStrLn ("VCD error: " <> err) >> exitFailure
    Right () -> putStrLn "auto-instrumentation passed"
