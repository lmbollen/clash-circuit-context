{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}

{- | The names in a waveform come from the design's source, never from the
order in which the simulation reaches its parts. The generator picks that
order.

The instances of a plugin-made component all share one @component@ call,
the one the plugin writes inside the component. They are numbered by the
calls of the component: the plugin splits each function that needs a
context into a wrapper and a worker, and the wrapper adds the site of its
call, which GHC passes in the call stack, to the context's call path.
-}
module Test.SourceOrder (tests) where

import qualified Prelude as P

import Control.Exception (evaluate, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import GHC.Stack (HasCallStack, callStack, getCallStack, withFrozenCallStack)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((-<.>))
import System.IO (hClose, openTempFile)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedgehog (Property, PropertyT, annotate, failure, forAll, property, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen

import Clash.Explicit.Prelude
import Clash.Signal (HiddenReset, withReset)

import Clash.CircuitContext
import Clash.CircuitContext.Recording (
  discardRecording,
  recordedSignals,
  signalKey,
  writeVCD,
 )
import Test.SourceOrder.Acc (acc)
import Test.SourceOrder.Plain (plainCalls)
import Test.Vcd (asInts, decodeVCDPaths, model)

--------------------------------------------------------------------------------
-- The designs

type Pair = (Signal System (Unsigned 8), Signal System (Unsigned 8))

-- | Two plain calls of 'acc', in two places.
twoCalls :: (HasCircuitContext) => Pair
twoCalls = (first, second)
 where
  first = acc 1
  second = acc 100
{-# OPAQUE twoCalls #-}

-- | A function whose next argument after the reset is constraint-rank.
underReset :: (HasCircuitContext) => Reset System -> ((HiddenReset System) => r) -> r
underReset r k = withReset r k
{-# OPAQUE underReset #-}

-- | Calls through @$@ and @&@ of a function with a constraint-rank argument.
dollarCalls :: (HasCircuitContext) => Pair
dollarCalls = (first, second)
 where
  first = underReset resetGen $ acc 1
  second = acc 100 & underReset resetGen
{-# OPAQUE dollarCalls #-}

-- | A function whose second argument is higher-rank.
withPoly ::
  (HasCircuitContext) => Unsigned 8 -> (forall b. b -> b) -> Signal System (Unsigned 8)
withPoly s f = f (acc s)
{-# OPAQUE withPoly #-}

-- | 'withPoly' through @$@, with a lambda as its higher-rank argument.
polyDollar :: (HasCircuitContext) => Pair
polyDollar = (withPoly 1 $ \x -> x, withPoly 100 $ \x -> x)
{-# OPAQUE polyDollar #-}

-- | A component that adds its two inputs into an accumulator.
addAcc ::
  (HasCircuitContext) =>
  Signal System (Unsigned 8) ->
  Signal System (Unsigned 8) ->
  Signal System (Unsigned 8)
addAcc a b = total
 where
  total = register clockGen resetGen enableGen 0 (total + a + b)
{-# OPAQUE addAcc #-}

-- | Two calls of 'addAcc' as an operator.
operatorCalls :: (HasCircuitContext) => Pair
operatorCalls = (pure 1 `addAcc` pure 0, pure 100 `addAcc` pure 0)
{-# OPAQUE operatorCalls #-}

-- | Apply a function: what a partial application is passed to.
applyTo :: (a -> b) -> a -> b
applyTo f x = f x

-- | Two partial applications of 'addAcc' (and 'acc' unapplied).
partialCalls :: (HasCircuitContext) => Pair
partialCalls = (applyTo (addAcc (pure 1)) (pure 0), applyTo (addAcc (pure 100)) (pure 0))
{-# OPAQUE partialCalls #-}

-- | A helper with no scope of its own: its call of 'acc' is in one place.
viaHelper :: (HasCircuitContext) => Unsigned 8 -> Signal System (Unsigned 8)
viaHelper step = acc step
{-# ANN viaHelper NoCircuitScope #-}

-- | Two calls of the helper: the calls of the helper order the instances.
twoHelperCalls :: (HasCircuitContext) => Pair
twoHelperCalls = (first, second)
 where
  first = viaHelper 1
  second = viaHelper 100
{-# OPAQUE twoHelperCalls #-}

-- | A local function with a signature, called twice.
localCalls :: (HasCircuitContext) => Pair
localCalls = (first, second)
 where
  first = go 1
  second = go 100
  go :: (HasCircuitContext) => Unsigned 8 -> Signal System (Unsigned 8)
  go step = acc step
{-# OPAQUE localCalls #-}

-- | A helper with two traces, @count@ and @other@, in its caller's scope.
twoTraces :: (HasCircuitContext) => Unsigned 8 -> Signal System (Unsigned 8)
twoTraces step = other
 where
  count = register clockGen resetGen enableGen 0 (count + pure step)
  other = count + 1
{-# ANN twoTraces NoCircuitScope #-}

-- | Two calls of 'twoTraces'.
twoTraceCalls :: (HasCircuitContext) => Pair
twoTraceCalls = (first, second)
 where
  first = twoTraces 1
  second = twoTraces 100
{-# OPAQUE twoTraceCalls #-}

-- | A recursive helper without a scope: one 'acc' per level.
chain :: (HasCircuitContext) => Int -> Int -> [Signal System (Unsigned 8)]
chain d n
  | d P.== n = []
  | P.otherwise = acc (P.fromIntegral d + 1) : chain (d + 1) n
{-# ANN chain NoCircuitScope #-}

-- | Four levels of 'chain'.
chained :: (HasCircuitContext) => [Signal System (Unsigned 8)]
chained = chain 0 4
{-# OPAQUE chained #-}

-- | Thirty levels of 'chain'.
deepChain :: (HasCircuitContext) => [Signal System (Unsigned 8)]
deepChain = chain 0 30
{-# OPAQUE deepChain #-}

-- | A synonym of the designer's that holds 'HasCircuitContext'.
type AccCtx dom = (HasCircuitContext, KnownDomain dom)

-- | 'acc', asking for its context through 'AccCtx'.
accSyn :: (AccCtx System) => Unsigned 8 -> Signal System (Unsigned 8)
accSyn step = total
 where
  total = register clockGen resetGen enableGen 0 (total + pure step)
{-# OPAQUE accSyn #-}

{- | Two calls of 'accSyn' in one expression. GHC 9.8 gives such calls one
call stack when the callee's type asks for a constraint tuple, so the plugin
writes the synonym out in the wrapper's signature.
-}
synonymCalls :: (HasCircuitContext) => Pair
synonymCalls = (accSyn 1, accSyn 100)
{-# OPAQUE synonymCalls #-}

-- | Two calls of 'acc' under a frozen 'HasCallStack': the call sites are not it.
frozenCalls :: (HasCircuitContext) => Pair
frozenCalls = withFrozenCallStack (acc 1, acc 100)
{-# OPAQUE frozenCalls #-}

-- | The functions on the designer's own call stack.
whereIs :: (HasCallStack, HasCircuitContext) => Int -> [String]
whereIs _ = P.map P.fst (getCallStack callStack)

-- | A class whose instance makes two instances of 'acc' in one method.
class Pairs a where
  pairs :: (HasCircuitContext) => a -> Pair

instance Pairs Bool where
  pairs _ = (acc 1, acc 100)

-- | The pair that one call of 'pairs' makes.
methodBody :: (HasCircuitContext) => Pair
methodBody = pairs True
{-# OPAQUE methodBody #-}

{- | A record of signals whose fields are not in name order, so that
declaration order and name order differ.
-}
data Lane = Lane
  { laneB :: Signal System (Unsigned 8)
  , laneA :: Signal System (Unsigned 8)
  }
  deriving (Generic)

instance Traceable Lane

{- | A vector of records, recorded as the binding @lanes@. It uses the
argument: a closed local binding is not recorded.
-}
vecOfRecords :: (HasCircuitContext) => Signal System (Unsigned 8) -> Vec 3 Lane
vecOfRecords inp = lanes
 where
  lanes = map (\i -> Lane (inp + pure i) (inp + pure (i + 10))) (0 :> 1 :> 2 :> Nil)
{-# OPAQUE vecOfRecords #-}

{- | Record an action and return its VCD of @[0, n)@. Every file has a
fresh name, as the properties run in parallel.
-}
recorded :: Int -> ((HasCircuitContext) => IO a) -> PropertyT IO String
recorded n k = liftIO $ do
  createDirectoryIfMissing True dir
  (path, h) <- openTempFile dir "source-order.vcd"
  hClose h
  (r, rec) <- withRecordingFrom 0 (Just (path -<.> "spill")) k
  _ <- P.either throwIO pure r
  written <- writeVCD (0, P.fromIntegral n) rec path
  discardRecording rec
  vcd <- P.either (P.const (pure "")) (P.const (P.readFile path)) written
  P.length vcd `P.seq` P.mapM_ removePathForcibly [path, path -<.> "json"]
  pure vcd
 where
  dir = "waveforms/test-source-order"

{- | Sample both outputs of a pair, in the order the generator picks, and
return the waves of @prefix_0.suffix@ and @prefix_1.suffix@.
-}
instancesOf ::
  String ->
  String ->
  ((HasCircuitContext) => Pair) ->
  PropertyT IO ([Maybe Integer], [Maybe Integer])
instancesOf prefix suffix pair = do
  secondFirst <- forAll Gen.bool
  vcd <- recorded 3 $ do
    let (a, b) = pair
        force x = evaluate (sampleN 3 x `deepseqX` ())
    if secondFirst then force b *> force a else force a *> force b
  let waves = decodeVCDPaths 3 vcd
      waveAt p =
        maybe
          (annotate (p P.<> " missing in " P.<> P.show (Map.keys waves)) *> failure)
          pure
          (Map.lookup p waves)
  w0 <- waveAt (prefix P.<> "_0" P.<> suffix)
  w1 <- waveAt (prefix P.<> "_1" P.<> suffix)
  pure (asInts w0, asInts w1)

-- | A pair of instances, numbered by their calls.
numbered :: String -> String -> ((HasCircuitContext) => Pair) -> Property
numbered prefix suffix pair = property $ do
  waves <- instancesOf prefix suffix pair
  waves === (model [0, 0, 1], model [0, 0, 100])

-- | A VCD's declarations: scopes, wires and their ends.
declarations :: String -> [String]
declarations vcd =
  [ d
  | l <- P.lines vcd
  , d <- case P.words l of
      ["$scope", "module", nm, "$end"] -> ["scope " P.<> nm]
      ["$var", "wire", _, _, nm, "$end"] -> [nm]
      ["$upscope", "$end"] -> ["up"]
      _ -> []
  ]

{- | The signals that the calls of one helper record into one scope come
call by call: all those of the first call, then all those of the second.
-}
prop_helperTraceOrder :: Property
prop_helperTraceOrder = property $ do
  secondFirst <- forAll Gen.bool
  vcd <- recorded 3 $ do
    let (a, b) = twoTraceCalls
        force x = evaluate (sampleN 3 x `deepseqX` ())
    if secondFirst then force b *> force a else force a *> force b
  declarations vcd
    === [ "scope twoTraceCalls"
        , "count_0"
        , "other_0"
        , "count_1"
        , "other_1"
        , "up"
        ]

{- | A function's own 'HasCallStack' keeps its caller's frame: the call
sites that the plugin needs are another implicit parameter, and the
wrapper does not touch this one.
-}
prop_userCallStack :: Property
prop_userCallStack = property $ do
  let frames = withoutCircuitContext (whereIs 0)
  annotate (P.show frames)
  Hedgehog.assert ("whereIs" `P.elem` frames)

{- | The levels of a recursive helper are numbered by depth, whatever the
order in which the simulation reaches them.
-}
prop_recursiveHelper :: Property
prop_recursiveHelper = property $ do
  order <- forAll (Gen.shuffle [0 .. 3 :: Int])
  vcd <-
    recorded 3
      $ P.mapM_ (\i -> evaluate (sampleN 3 (chained P.!! i) `deepseqX` ())) order
  let waves = decodeVCDPaths 3 vcd
  [ fmap asInts (Map.lookup ("chained.acc_" P.<> P.show i P.<> ".total") waves)
    | i <- [0 .. 3 :: Int]
    ]
    === [Just (model [0, 0, s]) | s <- [1 .. 4]]

{- | The key of a level of a recursive helper does not grow with its depth:
the recursive call is one site with a count, not one site per level.
-}
prop_recursiveKeys :: Property
prop_recursiveKeys = property $ do
  longest <- liftIO $ do
    createDirectoryIfMissing True "waveforms/test-source-order"
    (path, h) <- openTempFile "waveforms/test-source-order" "keys.spill"
    hClose h
    (r, rec) <-
      withRecordingFrom 0 (Just path)
        $ evaluate (P.map (sampleN 3) deepChain `deepseqX` ())
    _ <- P.either throwIO pure r
    let n = P.maximum (P.map (P.length . signalKey) (recordedSignals rec))
    n `P.seq` discardRecording rec
    pure n
  annotate (P.show longest)
  Hedgehog.assert (longest P.< 400)

{- | A vector of records is listed field by field, each field's elements in
index order, and the fields in declaration order (the elements of a vector
carry no position: see the 'Vec' instance of 'Traceable'). The generator picks the order in which the
simulation reaches the fields.
-}
prop_vecOfRecords :: Property
prop_vecOfRecords = property $ do
  order <- forAll (Gen.shuffle [(e, f) | e <- [0 .. 2], f <- [False, True]])
  vcd <- recorded 2 $ do
    let design = vecOfRecords (pure 1)
        field (e, f) = (if f then laneA else laneB) (design !! (e :: Int))
    P.mapM_ (\ef -> evaluate (sampleN 2 (field ef) `deepseqX` ())) order
  declarations vcd
    === [ "scope vecOfRecords"
        , "lanes_0_laneB"
        , "lanes_1_laneB"
        , "lanes_2_laneB"
        , "lanes_0_laneA"
        , "lanes_1_laneA"
        , "lanes_2_laneA"
        , "up"
        ]

tests :: TestTree
tests =
  testGroup
    "Test.SourceOrder"
    [ testProperty
        "plain calls number the instances"
        (numbered "twoCalls.acc" ".total" twoCalls)
    , testProperty
        "calls through $ and & with a constraint-rank argument"
        (numbered "dollarCalls.acc" ".total" dollarCalls)
    , testProperty
        "calls through $ with a higher-rank argument"
        (numbered "polyDollar.withPoly" ".acc.total" polyDollar)
    , testProperty
        "calls of an operator"
        (numbered "operatorCalls.addAcc" ".total" operatorCalls)
    , testProperty
        "partial applications"
        (numbered "partialCalls.addAcc" ".total" partialCalls)
    , testProperty
        "the calls of a helper number the instances it makes"
        (numbered "twoHelperCalls.acc" ".total" twoHelperCalls)
    , testProperty
        "the calls of a local function with a signature"
        (numbered "localCalls.acc" ".total" localCalls)
    , testProperty
        "the calls of a module without the plugin"
        (numbered "plainCalls.acc" ".total" plainCalls)
    , testProperty
        "the calls in an instance method body"
        (numbered "methodBody.acc" ".total" methodBody)
    , testProperty
        "the calls of a helper number the traces it records"
        (numbered "twoTraceCalls.count" "" twoTraceCalls)
    , testProperty
        "calls of a function whose context is behind a synonym"
        (numbered "synonymCalls.accSyn" ".total" synonymCalls)
    , testProperty
        "calls under a frozen HasCallStack"
        (numbered "frozenCalls.acc" ".total" frozenCalls)
    , testProperty "a function's own HasCallStack keeps its caller" prop_userCallStack
    , testProperty "a helper's signals come call by call" prop_helperTraceOrder
    , testProperty "the levels of a recursive helper are numbered by depth" prop_recursiveHelper
    , testProperty "the keys of a recursive helper stay short" prop_recursiveKeys
    , testProperty
        "a vector of records is listed field by field"
        prop_vecOfRecords
    ]
