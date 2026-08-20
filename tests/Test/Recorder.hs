{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

{- | Recorder behaviour under GENERATED stimuli, each property holding the
dumped VCD against the values the simulation computed.

The recorder's interesting behaviour is data-dependent — runs compress by
value, the last forced cycle lives in the packed tail, aliasing depends on
histories colliding, the window drops by position — which is what generated
inputs exercise and hand-picked smoke cycles do not. Shrinking then walks
every one of these to its edge: one cycle, divergence at the final
uncommitted cycle, a window of one.
-}
module Test.Recorder (tests) where

import qualified Prelude as P

import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedgehog (Property, PropertyT, annotate, evalEither, failure, forAll, property, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Explicit.Prelude

import Clash.CircuitContext (
  dumpVCDC,
  recordedCycles,
  traceSignalC,
  withCircuitContextWindowM,
 )
import Test.Vcd (asInts, bitsToInteger, decodeVCD, isZ, model)

-- | Generated stimulus, padded so the signal is total however far a
-- consumer looks.
stim :: [Unsigned 8] -> Signal System (Unsigned 8)
stim xs = fromList (xs P.<> P.repeat 0)

genStim :: Hedgehog.Gen [Unsigned 8]
genStim = Gen.list (Range.linear 1 32) (Gen.integral Range.linearBounded)

-- | The decoded wave of one signal, or a failing property if it is missing.
waveOf :: (Monad m) => Int -> String -> Text.Text -> PropertyT m [String]
waveOf n nm vcd =
  case Map.lookup nm (decodeVCD n (Text.unpack vcd)) of
    Nothing -> annotate ("signal " P.<> nm P.<> " missing from VCD") *> failure
    Just w -> pure w

{- | Every recorded cycle is the value the simulation computed — through
registration, the change-compressed runs, the packed-tail drain and the VCD
render. Shrinking drives @xs@ toward a single cycle, which lives entirely in
the drained tail.
-}
prop_dumpIsFaithful :: Property
prop_dumpIsFaithful = property $ do
  xs <- forAll genStim
  let n = P.length xs
  (out, traces, probes) <- withCircuitContextWindowM P.maxBound $ do
    let ys = sampleN n (traceSignalC "sig" (stim xs))
    liftIO (evaluate (ys `deepseqX` ys))
  out === xs -- tracing is observationally identity
  vcd <- evalEither =<< liftIO (dumpVCDC (0, n) traces probes)
  wave <- waveOf n "sig" vcd
  asInts wave === model xs

{- | 'recordedCycles' is the number of cells the consumer forced — for ANY
prefix, including the one-cell run whose only cycle is still uncommitted in
the recorder's packed tail. Shrinking drives @k@ to 1.
-}
prop_recordedCyclesCountsForcing :: Property
prop_recordedCyclesCountsForcing = property $ do
  xs <- forAll genStim
  k <- forAll (Gen.int (Range.linear 1 (P.length xs)))
  (_, traces, probes) <- withCircuitContextWindowM P.maxBound $ do
    let ys = sampleN (P.length xs) (traceSignalC "sig" (stim xs))
    -- Force the spine of exactly k cells, nothing more.
    liftIO (evaluate (P.length (P.take k ys)))
  recordedCycles traces probes === k

{- | Two traced signals never borrow each other's values, however similar
their histories — the dump may alias identical ones to a shared identifier
(an encoding choice), but each NAME must still decode to its own history.
The generator biases toward the hard case: identical except for one
mutated cycle, which shrinking walks to the final, still-uncommitted one.
-}
prop_aliasingNeverChangesValues :: Property
prop_aliasingNeverChangesValues = property $ do
  xs <- forAll genStim
  let n = P.length xs
  mutateAt <- forAll (Gen.maybe (Gen.int (Range.linear 0 (n P.- 1))))
  let mutate j i v = if i P.== j then v + 1 else v
      ys = maybe xs (\j -> P.zipWith (mutate j) [0 :: Int ..] xs) mutateAt
  (_, traces, probes) <- withCircuitContextWindowM P.maxBound $ do
    let sa = sampleN n (traceSignalC "a" (stim xs))
        sb = sampleN n (traceSignalC "b" (stim ys))
    liftIO (evaluate ((sa, sb) `deepseqX` (sa, sb)))
  vcd <- evalEither =<< liftIO (dumpVCDC (0, n) traces probes)
  waveA <- waveOf n "a" vcd
  waveB <- waveOf n "b" vcd
  asInts waveA === model xs
  asInts waveB === model ys

{- | The trailing capture window keeps (at least) the last @w@ cycles, and a
window-dropped cycle renders @z@, never a wrong value: every non-@z@ cycle
still matches the model.
-}
prop_windowKeepsTheTailTruthfully :: Property
prop_windowKeepsTheTailTruthfully = property $ do
  xs <- forAll genStim
  let n = P.length xs
  w <- forAll (Gen.int (Range.linear 1 (n P.+ 2)))
  (_, traces, probes) <- withCircuitContextWindowM w $ do
    let ys = sampleN n (traceSignalC "sig" (stim xs))
    liftIO (evaluate (ys `deepseqX` ys))
  recordedCycles traces probes === n
  vcd <- evalEither =<< liftIO (dumpVCDC (0, n) traces probes)
  wave <- waveOf n "sig" vcd
  -- the last min(w, n) cycles are retained …
  Hedgehog.assert (P.all (P.not . isZ) (P.drop (P.max 0 (n P.- w)) wave))
  -- … and nothing retained is ever wrong.
  P.sequence_
    [ bitsToInteger bits === Just (toInteger v)
    | (v, bits) <- P.zip xs wave
    , P.not (isZ bits)
    ]

tests :: TestTree
tests =
  testGroup
    "Test.Recorder"
    [ testProperty "every dumped cycle is the simulated value" prop_dumpIsFaithful
    , testProperty "recordedCycles counts forced cells, down to one" prop_recordedCyclesCountsForcing
    , testProperty "aliasing never changes a signal's decoded values" prop_aliasingNeverChangesValues
    , testProperty "a capture window keeps the tail, truthfully" prop_windowKeepsTheTailTruthfully
    ]
