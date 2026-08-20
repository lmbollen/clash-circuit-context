{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{- | Property tests of the recorder, doubling as the worked example of the
hedgehog infrastructure ("Clash.CircuitContext.Waveform.Hedgehog").

Two halves, and the pairing is the point:

* /recorder behaviour/ — hedgehog properties over generated stimuli, each
  asserting that what the recorder stored (via 'dumpVCDC') is exactly what
  the simulation computed. Runs, packed-tail draining, aliasing and the
  capture window are all data-dependent, which is precisely what generated
  inputs exercise and hand-written smoke cycles do not: shrinking drives
  every one of these toward its edge (one cycle, divergence at the last
  cycle, window of one).

* /showcase, then read the files/ — properties written the way a downstream
  suite writes them ('withWaveformCase' keeping the largest passing case;
  'withWaveformOnCounterexample' leaving a failing property's SHRUNK case),
  followed — tasty's 'sequentialTestGroup' guarantees the order — by tests
  that decode the files those properties wrote and check they show the run
  they claim to. "The waveform exists" is not the acceptance criterion; the
  failing cycle being IN it is.

The VCD decoder below is deliberately name-based: several @$var@
declarations may share one identifier code (the alias-dedup encoding), and
decoding each NAME independently is exactly what makes an aliasing bug
visible to a property.
-}
module Main where

import qualified Prelude as P

import Control.Exception (evaluate)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((-<.>))

import Test.Tasty (DependencyType (AllSucceed), TestTree, defaultMain, sequentialTestGroup, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Hedgehog (Property, PropertyT, annotate, evalEither, failure, forAll, property, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Explicit.Prelude

import Clash.CircuitContext (
  HasCircuitContext,
  dumpVCDC,
  recordedCycles,
  traceSignalC,
  withCircuitContextWindowM,
 )
import Clash.CircuitContext.Waveform (
  WaveformSlot,
  newWaveformSlot,
  waveformSlotPath,
 )
import Clash.CircuitContext.Waveform.Hedgehog (
  recordLargestCase,
  withWaveformCase,
  withWaveformOnCounterexample,
 )

--------------------------------------------------------------------------------
-- Designs under test
--------------------------------------------------------------------------------

-- | Generated stimulus, padded so the signal is total however far a
-- consumer looks.
stim :: [Unsigned 8] -> Signal System (Unsigned 8)
stim xs = fromList (xs P.<> P.repeat 0)

genStim :: Hedgehog.Gen [Unsigned 8]
genStim = Gen.list (Range.linear 1 32) (Gen.integral Range.linearBounded)

-- | A running sum with real state; its model is 'accModel'.
accDut ::
  (HasCircuitContext) =>
  Signal System (Unsigned 8) ->
  Signal System (Unsigned 8)
accDut inp = out
 where
  out = traceSignalC "acc" (inp + prev)
  prev = register clockGen resetGen enableGen 0 out

{- | What 'accDut' computes. NOT @scanl1 (+)@: 'resetGen' asserts during
cycle 0, so the register suppresses the load at the edge OUT of that cycle —
it still outputs its init at cycle 1, and @out 0@ never reaches the state.
The register therefore contributes @0, 0, out 1, out 2, …@.
-}
accModel :: [Unsigned 8] -> [Unsigned 8]
accModel xs = out
 where
  out = P.zipWith (+) xs (0 : 0 : P.drop 1 out)

-- | Deliberately wrong: adds one to every sample. The showcase property
-- asserts it equals its input, fails, and leaves the counterexample.
offByOne ::
  (HasCircuitContext) =>
  Signal System (Unsigned 8) ->
  Signal System (Unsigned 8)
offByOne inp = traceSignalC "wrong" (inp + 1)

--------------------------------------------------------------------------------
-- A small VCD decoder
--------------------------------------------------------------------------------

{- | name → one bit string per cycle over @[0, n)@, values carried forward
between change lines. Names are resolved through their identifier codes, so
two names sharing a code (an alias declaration) each get a full wave — and a
wrongly-aliased signal therefore decodes to the WRONG values, where a
code-based decoder would hide it.
-}
decodeVCD :: Int -> String -> Map.Map String [String]
decodeVCD n vcd = Map.fromList [(nm, waveFor code) | (nm, code) <- vars]
 where
  ls = P.lines vcd
  vars =
    [ (nm, code)
    | l <- ls
    , ["$var", "wire", _w, code, nm, "$end"] <- [P.words l]
    ]
  events = go (0 :: Int) ls
  go t (l : rest) = case l of
    '#' : num -> go (P.read num) rest
    'b' : _
      | [bits, code] <- P.words l -> (t, code, P.drop 1 bits) : go t rest
    _ -> go t rest
  go _ [] = []
  waveFor code =
    [ P.last ("z" : [bits | (t, c, bits) <- events, c P.== code, t P.<= cyc])
    | cyc <- [0 .. n P.- 1]
    ]

-- | 'Nothing' for a value with undefined (@x@) or never-sampled (@z@) bits.
bitsToInteger :: String -> Maybe Integer
bitsToInteger = P.foldl step (Just 0)
 where
  step acc ch = do
    v <- acc
    d <- case ch of
      '0' -> Just 0
      '1' -> Just 1
      _ -> Nothing
    pure (v P.* 2 P.+ d)

-- | A cycle the recorder never retained (window-dropped or never forced).
isZ :: String -> Bool
isZ = P.all (P.== 'z')

-- | The decoded wave of one signal, as defined 'Integer's where possible.
waveOf :: (Monad m) => Int -> String -> Text.Text -> PropertyT m [String]
waveOf n nm vcd =
  case Map.lookup nm (decodeVCD n (Text.unpack vcd)) of
    Nothing -> annotate ("signal " P.<> nm P.<> " missing from VCD") *> failure
    Just w -> pure w

asInts :: [String] -> [Maybe Integer]
asInts = P.map bitsToInteger

model :: [Unsigned 8] -> [Maybe Integer]
model = P.map (Just . toInteger)

--------------------------------------------------------------------------------
-- Recorder behaviour, under generated stimuli
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Showcase: the combinators as a downstream suite uses them
--------------------------------------------------------------------------------

{- | The artifact pattern: a PASSING property that keeps its largest case.
The flag is decided before the simulation ('recordLargestCase' fires on the
size-99 case of a default 100-test property), so the 99 losing cases never
record. The kept case's model is stashed so the inspection test can hold the
file against it.
-}
prop_accumulatesAndKeepsLargest ::
  IORef Bool -> IORef (Maybe [Unsigned 8]) -> WaveformSlot -> Property
prop_accumulatesAndKeepsLargest fired expected slot = property $ do
  xs <- forAll genStim
  keep <- recordLargestCase fired
  let n = P.length xs
  withWaveformCase keep slot n (sampleN n (accDut (stim xs))) $ \out -> do
    let acc = accModel xs
    when keep (liftIO (writeIORef expected (Just acc)))
    out === acc

{- | The failure pattern: assertions live in the consumer, so when they fail
the case is re-run recording and the SHRUNK counterexample survives in the
slot. Passing cases cost nothing.
-}
prop_offByOneIsCaught :: WaveformSlot -> Property
prop_offByOneIsCaught slot = property $ do
  xs <- forAll (Gen.list (Range.linear 1 8) (Gen.integral Range.linearBounded))
  let n = P.length xs
  withWaveformOnCounterexample slot n (sampleN n (offByOne (stim xs))) $ \out ->
    out === xs

--------------------------------------------------------------------------------

tests ::
  IORef Bool ->
  IORef (Maybe [Unsigned 8]) ->
  WaveformSlot ->
  WaveformSlot ->
  TestTree
tests fired expected accSlot ceSlot =
  testGroup
    "waveform-props"
    [ testGroup
        "recorder behaviour"
        [ testProperty "every dumped cycle is the simulated value" prop_dumpIsFaithful
        , testProperty "recordedCycles counts forced cells, down to one" prop_recordedCyclesCountsForcing
        , testProperty "aliasing never changes a signal's decoded values" prop_aliasingNeverChangesValues
        , testProperty "a capture window keeps the tail, truthfully" prop_windowKeepsTheTailTruthfully
        ]
    , -- The writers run before the readers — that ORDER is the contract this
      -- group encodes, so it is sequential where everything else may run in
      -- parallel.
      sequentialTestGroup
        "showcase, then read the files it wrote"
        AllSucceed
        [ testProperty
            "accumulator matches its model (largest case kept)"
            (prop_accumulatesAndKeepsLargest fired expected accSlot)
        , testCase "an off-by-one DUT fails, leaving its counterexample" $ do
            ok <- Hedgehog.check (prop_offByOneIsCaught ceSlot)
            assertBool "the deliberately wrong property must fail" (P.not ok)
        , testCase "the kept waveform holds the accumulator's own run" $ do
            acc <-
              readIORef expected
                >>= P.maybe (assertFailure "largest case never fired") pure
            let n = P.length acc
            vcd <- P.readFile (waveformSlotPath accSlot)
            wave <-
              P.maybe (assertFailure "acc missing from VCD") pure $
                Map.lookup "acc" (decodeVCD n vcd)
            asInts wave @?= model acc
        , testCase "the counterexample waveform is the minimal shrunk case" $ do
            -- Any failing list shrinks to [0]; the off-by-one DUT then
            -- computes 1. One cycle, one wrong value — and that cycle lives
            -- entirely in the recorder's uncommitted tail, so this also
            -- proves a one-cycle counterexample survives capture.
            vcd <- P.readFile (waveformSlotPath ceSlot)
            P.length [() | '#' : _ <- P.lines vcd] @?= 1
            wave <-
              P.maybe (assertFailure "wrong missing from VCD") pure $
                Map.lookup "wrong" (decodeVCD 1 vcd)
            asInts wave @?= [Just 1]
        ]
    ]

main :: IO ()
main = do
  fired <- newIORef False
  expected <- newIORef Nothing
  accSlot <- newWaveformSlot "props-accumulator"
  ceSlot <- newWaveformSlot "props-counterexample"
  -- The reader tests must see THIS run's files, not a previous run's.
  P.mapM_ scrub [accSlot, ceSlot]
  defaultMain (tests fired expected accSlot ceSlot)
 where
  scrub slot = do
    let vcd = waveformSlotPath slot
    P.mapM_
      (\p -> doesFileExist p >>= \e -> when e (removeFile p))
      [vcd, vcd -<.> "json"]
