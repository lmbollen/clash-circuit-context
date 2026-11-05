{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- | Recorder behaviour under GENERATED stimuli, each property holding the
written VCD against the values the simulation computed.

The recorder's interesting behaviour is data-dependent — records compress by
value, the last forced cycle is recorded only when the run ends, aliasing
depends on histories colliding, a window starts at a position — which is
what generated inputs exercise and hand-picked smoke cycles do not.
Shrinking then walks every one of these to its edge: one cycle, divergence
at the final cycle, a window that starts at its end.
-}
module Test.Recorder (tests) where

import qualified Prelude as P

import Control.Exception (evaluate, throwIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((-<.>))
import System.IO (hClose, openTempFile)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedgehog (Property, PropertyT, annotate, failure, forAll, property, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Explicit.Prelude

import Clash.CircuitContext (
  HasCircuitContext,
  component,
  traceSignalC,
  withRecordingFrom,
 )
import Clash.CircuitContext.Recording (discardRecording, recordingEnd, writeVCD)
import Test.Vcd (asInts, decodeVCD, decodeVCDPaths, model)

-- | A domain at half the speed of 'System'.
createDomain vSystem{vName = "Slow", vPeriod = 20000}

{- | Generated stimulus, padded so the signal is total however far a
consumer looks.
-}
stim :: [Unsigned 8] -> Signal dom (Unsigned 8)
stim xs = fromList (xs P.<> P.repeat 0)

genStim :: Hedgehog.Gen [Unsigned 8]
genStim = Gen.list (Range.linear 1 32) (Gen.integral Range.linearBounded)

{- | Record an action from cycle @start@ of 'System' on, and write the VCD of
@[start, stop)@. Returns what the action returned, 'recordingEnd', and the
VCD (empty if none was written). Every file goes in a fresh name, as the
properties run in parallel.
-}
recorded ::
  Int -> Int -> ((HasCircuitContext) => IO a) -> PropertyT IO (a, Int, String)
recorded start stop k = liftIO $ do
  createDirectoryIfMissing True dir
  (path, h) <- openTempFile dir "recorder.vcd"
  hClose h
  (r, rec) <- withRecordingFrom (start P.* period) (Just (path -<.> "spill")) k
  a <- P.either throwIO pure r
  written <- writeVCD (P.fromIntegral start, P.fromIntegral stop) rec path
  discardRecording rec
  vcd <- P.either (P.const (pure Text.empty)) (P.const (TextIO.readFile path)) written
  P.mapM_ removePathForcibly [path, path -<.> "json"]
  pure (a, P.fromIntegral (recordingEnd rec), Text.unpack vcd)
 where
  dir = "waveforms/test-recorder"
  period = snatToNum (clockPeriod @System)

-- | The decoded wave of one signal, or a failing property if it is missing.
waveOf :: (Monad m) => Int -> String -> String -> PropertyT m [String]
waveOf n nm vcd =
  case Map.lookup nm (decodeVCD n vcd) of
    Nothing -> annotate ("signal " P.<> nm P.<> " missing from VCD") *> failure
    Just w -> pure w

{- | Every recorded cycle is the value the simulation computed — through
registration, the spill file's records, the cycle finished when the run
ends, and the VCD writer. Shrinking drives @xs@ toward a single cycle, which
is recorded only when the run ends.
-}
prop_dumpIsFaithful :: Property
prop_dumpIsFaithful = property $ do
  xs <- forAll genStim
  let n = P.length xs
  (out, _, vcd) <- recorded 0 n $ do
    let ys = sampleN n (traceSignalC "sig" (stim @System xs))
    evaluate (ys `deepseqX` ys)
  out === xs -- tracing is observationally identity
  wave <- waveOf n "sig" vcd
  asInts wave === model xs

{- | 'recordingEnd' is the number of cells the consumer forced — for ANY
prefix, including the one-cell run whose only cycle the tap has not
recorded yet. Shrinking drives @k@ to 1.
-}
prop_recordingEndCountsForcing :: Property
prop_recordingEndCountsForcing = property $ do
  xs <- forAll genStim
  k <- forAll (Gen.int (Range.linear 1 (P.length xs)))
  (_, end, _) <- recorded 0 (P.length xs) $ do
    let ys = sampleN (P.length xs) (traceSignalC "sig" (stim @System xs))
    -- Force the spine of exactly k cells, nothing more.
    evaluate (P.length (P.take k ys))
  end === k

{- | Two traced signals never borrow each other's values, however similar
their histories — the writer may alias identical ones to a shared identifier
(an encoding choice), but each NAME must still decode to its own history.
The generator biases toward the hard case: identical except for one
mutated cycle, which shrinking walks to the final one.
-}
prop_aliasingNeverChangesValues :: Property
prop_aliasingNeverChangesValues = property $ do
  xs <- forAll genStim
  let n = P.length xs
  mutateAt <- forAll (Gen.maybe (Gen.int (Range.linear 0 (n P.- 1))))
  let mutate j i v = if i P.== j then v + 1 else v
      ys = maybe xs (\j -> P.zipWith (mutate j) [0 :: Int ..] xs) mutateAt
  (_, _, vcd) <- recorded 0 n $ do
    let sa = sampleN n (traceSignalC "a" (stim @System xs))
        sb = sampleN n (traceSignalC "b" (stim @System ys))
    evaluate ((sa, sb) `deepseqX` (sa, sb))
  waveA <- waveOf n "a" vcd
  waveB <- waveOf n "b" vcd
  asInts waveA === model xs
  asInts waveB === model ys

{- | A window @(start, stop)@ shows exactly its cycles: the VCD starts at
@#start@ with the value of cycle @start@, every later cycle is the simulated
value, and nothing before it is shown. The recorder writes nothing before
@start@, so this also holds the first record against the cycles it skipped.
-}
prop_windowShowsItsCycles :: Property
prop_windowShowsItsCycles = property $ do
  xs <- forAll genStim
  let n = P.length xs
  start <- forAll (Gen.int (Range.linear 0 (n P.- 1)))
  (_, end, vcd) <- recorded start n $ do
    let ys = sampleN n (traceSignalC "sig" (stim @System xs))
    evaluate (ys `deepseqX` ys)
  end === n
  wave <- waveOf n "sig" vcd
  P.take 1 [t | '#' : t <- P.lines vcd] === [show start]
  asInts (P.drop start wave) === model (P.drop start xs)
  Hedgehog.assert (P.all (P.== "z") (P.take start wave))

{- | A signal of a slower domain changes at its own clock edges: in units of
the timescale (the faster period), cycle @c@ of a domain twice as slow is at
time @2c@. (Stock @dumpVCD@ repeats each slow value and so shifts them.)
-}
prop_slowDomainKeepsItsTime :: Property
prop_slowDomainKeepsItsTime = property $ do
  xs <- forAll genStim
  let n = P.length xs
  (_, end, vcd) <- recorded 0 (2 P.* n) $ do
    let fast = sampleN n (traceSignalC "fast" (stim @System xs))
        slow = sampleN n (traceSignalC "slow" (stim @Slow xs))
    evaluate ((fast, slow) `deepseqX` ())
  end === 2 P.* n
  slowWave <- waveOf (2 P.* n) "slow" vcd
  fastWave <- waveOf (2 P.* n) "fast" vcd
  asInts slowWave === P.concatMap (\x -> [x, x]) (model xs)
  asInts fastWave === model (P.take (2 P.* n) (xs P.<> P.repeat 0))

{- | Two instances of one component under one parent are numbered by where
they are written, never by which one the simulation reaches first. The
generator picks the evaluation order. Before the call site skipped the
library's own frames, every instance had the same call site, and @acc_0@
was whichever instance was evaluated first.
-}
prop_siblingsFollowSourceOrder :: Property
prop_siblingsFollowSourceOrder = property $ do
  secondFirst <- forAll Gen.bool
  (_, _, vcd) <- recorded 0 3 $ do
    let a = sampleN 3 writtenFirst
        b = sampleN 3 writtenSecond
        force x = evaluate (x `deepseqX` x)
    if secondFirst then force b *> force a else force a *> force b
  let waves = decodeVCDPaths 3 vcd
      waveAt p = maybe (annotate (p P.<> " missing") *> failure) pure (Map.lookup p waves)
  wave0 <- waveAt "acc_0.total"
  wave1 <- waveAt "acc_1.total"
  asInts wave0 === model [0, 0, 1]
  asInts wave1 === model [0, 0, 100]
 where
  writtenFirst, writtenSecond :: (HasCircuitContext) => Signal System (Unsigned 8)
  writtenFirst = component "acc" (counting 1)
  writtenSecond = component "acc" (counting 100)
  counting step =
    let total = register clockGen resetGen enableGen 0 (total + pure step)
     in traceSignalC "total" total
{- | A scope declares its signals in the order they are written, then its
child scopes in name order, with numbers compared as numbers. The generator
picks the order in which the simulation reaches them. Before this order was
fixed, signals came in registration order and @lane_10@ came before
@lane_2@.

The signals are written out of name order on purpose (@sig@ before @b_10@
before @a@), so that source order and name order differ.
-}
prop_declarationsAreSorted :: Property
prop_declarationsAreSorted = property $ do
  signalOrder <- forAll (Gen.shuffle [0 .. 2 :: Int])
  lanes <- forAll (Gen.shuffle [10, 0, 2 :: Int])
  (_, _, vcd) <- recorded 0 2 $ do
    let sampled =
          component "top"
            $ [written P.!! i | i <- signalOrder]
            P.<> [ component ("lane_" P.<> show i) (sampleN 2 (traceSignalC "x" (stim @System [3, 4])))
                 | i <- lanes
                 ]
    evaluate (sampled `deepseqX` sampled)
  declarations vcd
    === [ "scope top"
        , "sig"
        , "b_10"
        , "a"
        , "scope lane_0"
        , "x"
        , "up"
        , "scope lane_2"
        , "x"
        , "up"
        , "scope lane_10"
        , "x"
        , "up"
        , "up"
        ]
 where
  written :: (HasCircuitContext) => [[Unsigned 8]]
  written =
    [ sampleN 2 (traceSignalC "sig" (stim @System [1, 2]))
    , sampleN 2 (traceSignalC "b_10" (stim @System [1, 2]))
    , sampleN 2 (traceSignalC "a" (stim @System [1, 2]))
    ]
  declarations vcd =
    [ d
    | l <- P.lines vcd
    , d <- case P.words l of
        ["$scope", "module", nm, "$end"] -> ["scope " P.<> nm]
        ["$var", "wire", _, _, nm, "$end"] -> [nm]
        ["$upscope", "$end"] -> ["up"]
        _ -> []
    ]

tests :: TestTree
tests =
  testGroup
    "Test.Recorder"
    [ testProperty "every recorded cycle is the simulated value" prop_dumpIsFaithful
    , testProperty
        "recordingEnd counts forced cells, down to one"
        prop_recordingEndCountsForcing
    , testProperty
        "aliasing never changes a signal's decoded values"
        prop_aliasingNeverChangesValues
    , testProperty "a window shows exactly its cycles" prop_windowShowsItsCycles
    , testProperty "a slower domain changes at its own clock edges" prop_slowDomainKeepsItsTime
    , testProperty "sibling instances are numbered in source order" prop_siblingsFollowSourceOrder
    , testProperty
        "a scope declares its signals in source order, its scopes by name"
        prop_declarationsAreSorted
    ]
