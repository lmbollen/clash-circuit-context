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

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (
  MVar,
  modifyMVar_,
  newEmptyMVar,
  newMVar,
  putMVar,
  readMVar,
  takeMVar,
 )
import Control.Exception (
  SomeException,
  displayException,
  evaluate,
  finally,
  throwIO,
  try,
 )
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((-<.>), (</>))
import System.IO (hClose, openTempFile)
import System.IO.Unsafe (unsafePerformIO)

import Clash.CircuitContext (HasProbe, mealyProbed)
import Clash.CircuitContext.Recording (recoverRecording)

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
  traceSignalSW,
  withRecording,
  withRecordingFrom,
 )
import Clash.CircuitContext.Core (HierSeg (..), ProbeCtx (..), probe)
import Clash.CircuitContext.Recording (
  discardRecording,
  recordedChanges,
  recordedSignals,
  recordingEnd,
  signalKey,
  writeVCD,
 )
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
recorded start stop k = do
  (a, end, vcd, _) <- recordedWithSidecar start stop k
  pure (a, end, vcd)

-- | 'recorded', with the sidecar of the VCD (empty if none was written).
recordedWithSidecar ::
  Int -> Int -> ((HasCircuitContext) => IO a) -> PropertyT IO (a, Int, String, String)
recordedWithSidecar start stop k = liftIO $ do
  createDirectoryIfMissing True dir
  (path, h) <- openTempFile dir "recorder.vcd"
  hClose h
  (r, rec) <- withRecordingFrom (start P.* period) (Just (path -<.> "spill")) k
  a <- P.either throwIO pure r
  written <- writeVCD (P.fromIntegral start, P.fromIntegral stop) rec path
  discardRecording rec
  let readOr f = P.either (P.const (pure Text.empty)) (P.const (TextIO.readFile f)) written
  vcd <- readOr path
  json <- readOr (path -<.> "json")
  P.mapM_ removePathForcibly [path, path -<.> "json"]
  pure (a, P.fromIntegral (recordingEnd rec), Text.unpack vcd, Text.unpack json)
 where
  dir = "waveforms/test-recorder"

-- | The clock period of 'System', in ps.
period :: Int
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

{- | Every signal has a name of its own in the VCD, whatever names its scope
has: a name that the numbering of duplicates makes (@x_0@ for the first of
two @x@) is never a name that is already there. A name used once is kept.
The generator picks names that collide in each of the ways that numbering
can: with a name used once (@x_0@), and with a number made for another
name (@x__0@ for @x_@, and for @x@ when @x_0@ is taken). Before, the VCD
kept one of two signals with the same name, and the other was not there.
-}
prop_namesAreUnique :: Property
prop_namesAreUnique = property $ do
  names <-
    forAll
      (Gen.list (Range.linear 1 8) (Gen.element ["x", "x_0", "x_1", "x_", "x__0", "x_0_0"]))
  let n = P.length names
  (_, _, vcd) <- recorded 0 1 $ do
    let sampled =
          component
            "top"
            [ sampleN 1 (traceSignalC nm (pure (P.fromIntegral i) :: Signal System (Unsigned 8)))
            | (i, nm) <- P.zip [0 :: Int ..] names
            ]
    evaluate (sampled `deepseqX` sampled)
  let waves = decodeVCDPaths 1 vcd
  annotate (P.show (Map.keys waves))
  Map.size waves === n
  P.concatMap asInts (Map.elems waves)
    `sameElements` model (P.map P.fromIntegral [0 .. n P.- 1])
  [ nm
    | nm <- names
    , P.length (P.filter (P.== nm) names) P.== 1
    , ("top." P.<> nm) `Map.notMember` waves
    ]
    === []
 where
  sameElements a b = List.sort a === List.sort b

{- | The signals outside every component are in the stock scope @logic@.
A component named @logic@ beside them is numbered as a second @logic@:
@logic_0@. Before, the VCD had two scopes named @logic@, each with a @foo@,
the sidecar had one entry for the two (@logic.foo@), and the FST had two
@foo@ in one scope. The generator picks which one the simulation reaches
first.
-}
prop_rootScopeHasItsName :: Property
prop_rootScopeHasItsName = property $ do
  componentFirst <- forAll Gen.bool
  (_, _, vcd, json) <- recordedWithSidecar 0 1 $ do
    -- traceSignalSW: a signal described in the sidecar.
    let inside = component "logic" (sampleN 1 (traceSignalSW "foo" (stim @System [1])))
        outside = sampleN 1 (traceSignalSW "foo" (stim @System [2]))
        force x = evaluate (x `deepseqX` ())
    if componentFirst then force inside *> force outside else force outside *> force inside
  let waves = decodeVCDPaths 1 vcd
  annotate (P.show (Map.keys waves))
  P.length [() | "$scope module logic $end" <- P.lines vcd] === 1
  Map.toList (P.fmap asInts waves)
    === [("logic.foo", model [2]), ("logic_0.foo", model [1])]
  -- Both are in the sidecar, by their own path.
  [p | p <- paths, not (("\"" P.<> p P.<> "\"") `List.isInfixOf` json)]
    === []
 where
  paths = ["logic.foo", "logic_0.foo"]

{- | Two probes with one key are two signals. The key of a probe is its
scope, its name, and the instance ordinal and call site of its 'pcPath';
two 'ProbeCtx' with one 'pcPath' and their own caches each register a
probe @p@ with the same key. Before, the VCD had one of the two.
-}
prop_probesWithOneKey :: Property
prop_probesWithOneKey = property $ do
  (_, _, vcd) <- recorded 0 1 $ do
    caches <- P.mapM (P.const (newIORef Map.empty)) [1, 2 :: Int]
    let path = [HierSeg "machine" ""]
        probed =
          [ let ?probe = ProbeCtx ?circuitContext period path 0 cache in probe "p" v
          | (cache, v) <- P.zip caches [1, 2 :: Unsigned 8]
          ]
    -- Something sampled, so that the run has a cycle.
    evaluate ((probed, sampleN 1 (traceSignalC "t" (stim @System [0]))) `deepseqX` ())
  let waves = decodeVCD 1 vcd
  annotate (P.show (Map.keys waves))
  Map.toList (P.fmap asInts waves)
    === [("p_0", model [1]), ("p_1", model [2]), ("t", model [0])]

{- | The first firings of one probe on several threads at once register one
signal. Each thread fires cycle 0 of probe @p@ of one instance (one
'pcPath', one cache); each spells the name for itself ('waitingName'), and
a name, when forced, waits for the other threads to force theirs. The
recorder forces the name when it registers the probe, after it found the
probe missing from the cache, so without a lock all threads find it missing
and each registers one: @p_0@, @p_1@, … Then cycle 1 fires.

The suite runs without @-threaded@, so the threads take turns on one
capability, but a thread that waits lets the others run, and a thread can
be switched out at any allocation. Without the waits, 8 threads registered
two signals in 33 of 2000 runs.
-}
prop_probeFirstFiringsRace :: Property
prop_probeFirstFiringsRace = Hedgehog.withTests 20 $ property $ do
  (_, _, vcd) <- recorded 0 2 $ do
    cache <- newIORef Map.empty
    gate <- newMVar ([], False)
    let path = [HierSeg "machine" ""]
        fire nm c v = let ?probe = ProbeCtx ?circuitContext period path c cache in probe nm v
    dones <-
      P.mapM
        ( \i -> do
            done <- newEmptyMVar
            _ <-
              forkIO
                ( (evaluate (fire (waitingName gate threads i) 0 (7 :: Unsigned 8)) *> pure ())
                    `finally` putMVar done ()
                )
            pure done
        )
        [1 .. threads]
    P.mapM_ takeMVar dones
    _ <- evaluate (fire "p" 1 8)
    evaluate (sampleN 2 (traceSignalC "t" (stim @System [0, 0])) `deepseqX` ())
  let waves = decodeVCD 2 vcd
  annotate (P.show (Map.keys waves))
  Map.toList (P.fmap asInts waves) === [("p", model [7, 8]), ("t", model [0, 0])]
 where
  threads = 4

{- | The name @p@, as thread @i@ spells it. Forcing it waits until @n@
threads have forced theirs, or until a wait of about 20 ms ran out: from
then on, no name waits.
-}
waitingName :: MVar ([Int], Bool) -> Int -> Int -> String
waitingName gate n i = unsafePerformIO (arrive *> wait (20 :: Int)) `P.seq` "p"
 where
  arrive = modifyMVar_ gate (\(is, open) -> pure (i : is, open))
  wait 0 = modifyMVar_ gate (\(is, _) -> pure (is, True))
  wait t = do
    (is, open) <- readMVar gate
    if open P.|| P.length is P.>= n then pure () else threadDelay 1000 *> wait (t P.- 1)
{-# NOINLINE waitingName #-}

{- | A probed value that the design evaluates after a later cycle's is shown
at its own cycle, and @z@ is left exactly where a cycle was never evaluated:
from a recording in memory, from one with a spill file, and from that spill
file read back.

A sample-and-hold ('holdLate'): every fourth cycle keeps its input @x@ in the
state, and puts it out three cycles later, after the two @x@s in between were
put out (and so evaluated). The state is a tuple, which a register keeps
evaluated only to its constructor, so the kept @x@ is evaluated when it is
put out. Before late values were kept, its cycle showed @z@: "not
evaluated", which was false. And 'swapLate', whose late values come out of
order.
-}
prop_lateProbeIsShown :: Property
prop_lateProbeIsShown = property $ do
  xs <- forAll genStim
  lateProbeIsShown xs

{- | 'prop_lateProbeIsShown' on a run long enough that its late values take
several chunks in memory, and several regions of a spill file.
-}
prop_manyLateProbesAreShown :: Property
prop_manyLateProbesAreShown =
  Hedgehog.withTests 1
    . property
    $ lateProbeIsShown (P.take 40000 (P.cycle [0 .. 250]))

lateProbeIsShown :: [Unsigned 8] -> PropertyT IO ()
lateProbeIsShown xs = do
  annotate "holdLate"
  -- Put out at k (c 1, 2 modulo 4), or kept and put out at k + 3 (c 0).
  lateProbeShows xs holdLate $ \c k ->
    c `P.mod` 4 `P.elem` [1, 2] P.|| (c `P.mod` 4 P.== 0 P.&& k P.+ 3 P.< n)
  annotate "swapLate"
  -- Put out at k (c 2, 5, 6, 7), or kept and put out at k + 4 (c 0) or at
  -- k + 2 (c 1).
  lateProbeShows xs swapLate $ \c k ->
    c `P.elem` [2, 5, 6, 7]
      P.|| (c P.== 0 P.&& k P.+ 4 P.< n)
      P.|| (c P.== 1 P.&& k P.+ 2 P.< n)
 where
  n = P.length xs

{- | The probe @x@ of a design shows where it was evaluated, which a function
of the design's counter at a cycle and the cycle says.
-}
lateProbeShows ::
  [Unsigned 8] ->
  ((HasCircuitContext) => Signal System (Unsigned 8) -> Signal System (Unsigned 8)) ->
  (Int -> Int -> Bool) ->
  PropertyT IO ()
lateProbeShows xs design evaluatedAt = do
  let
    n = P.length xs
    -- The state register holds its reset value at cycles 0 and 1, so the x
    -- of cycle 0 is kept while it is in reset, and lost.
    evaluated k = k P.> 0 P.&& evaluatedAt ((k P.- 1) `P.mod` 8) k
    want = [if evaluated k then Just (toInteger x) else Nothing | (k, x) <- P.zip [0 ..] xs]
  liftIO (createDirectoryIfMissing True dir)
  (path, h) <- liftIO (openTempFile dir "late.vcd")
  liftIO (hClose h)
  let spill = path -<.> "spill"
      record where_ = liftIO $ do
        (r, rec) <- withRecordingFrom 0 where_ $ do
          -- The output is traced so that the recording ends at the last cycle.
          let ys = sampleN n (traceSignalC "out" (design (stim @System xs)))
          evaluate (ys `deepseqX` ys)
        _ <- P.either throwIO pure r
        pure rec
      shown rec = do
        written <- liftIO (writeVCD (0, P.fromIntegral n) rec path)
        written === Right ()
        vcd <- liftIO (P.readFile path)
        -- A probe that never fired is not in the VCD at all.
        _ <- liftIO (evaluate (P.length vcd))
        pure (maybe (P.replicate n Nothing) asInts (Map.lookup "x" (decodeVCD n vcd)))
  inMemory <- record Nothing
  onDisk <- record (Just spill)
  recovered <- liftIO (recoverRecording spill)
  readBack <- P.either (\e -> annotate e *> failure) pure recovered
  forM_ [("in memory", inMemory), ("with a spill file", onDisk), ("read back", readBack)]
    $ \(how, rec) -> do
      annotate how
      wave <- shown rec
      wave === want
  liftIO (P.mapM_ removePathForcibly [path, path -<.> "json", spill])
 where
  dir = "waveforms/test-recorder"

{- | The sample-and-hold of 'prop_lateProbeIsShown'. Its counter counts to
8, as 'swapLate''s, and the hold repeats every 4.
-}
holdLate ::
  (HasCircuitContext) => Signal System (Unsigned 8) -> Signal System (Unsigned 8)
holdLate = mealyProbed clockGen resetGen enableGen step (0, 0)
 where
  step ::
    (HasProbe) =>
    (Unsigned 8, Unsigned 3) -> Unsigned 8 -> ((Unsigned 8, Unsigned 3), Unsigned 8)
  step (h, c) i = ((if c `P.mod` 4 P.== 0 then x else h, c + 1), o)
   where
    x = probe "x" i
    o = case c `P.mod` 4 of
      0 -> 0
      3 -> h
      _ -> x

{- | Every eighth cycle keeps its @x@, and the next one too; it puts out the
second two cycles later, and the first a cycle after that. So the late
value of the earlier cycle comes after the later one's.
-}
swapLate ::
  (HasCircuitContext) => Signal System (Unsigned 8) -> Signal System (Unsigned 8)
swapLate = mealyProbed clockGen resetGen enableGen step (0, 0, 0)
 where
  step ::
    (HasProbe) =>
    (Unsigned 8, Unsigned 8, Unsigned 3) ->
    Unsigned 8 ->
    ((Unsigned 8, Unsigned 8, Unsigned 3), Unsigned 8)
  step (h0, h1, c) i = ((if c P.== 0 then x else h0, if c P.== 1 then x else h1, c + 1), o)
   where
    x = probe "x" i
    o = case c of
      0 -> 0
      1 -> 0
      3 -> h1
      4 -> h0
      _ -> x

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
    , testProperty "every signal has a name of its own" prop_namesAreUnique
    , testProperty
        "threads that record at once keep their own values"
        prop_threadsKeepTheirValues
    , testProperty "the stock logic scope keeps its name" prop_rootScopeHasItsName
    , testProperty "two probes with one key are two signals" prop_probesWithOneKey
    , testProperty
        "the first firings of a probe on several threads register one signal"
        prop_probeFirstFiringsRace
    , testProperty
        "a probed value evaluated after a later cycle is shown at its cycle"
        prop_lateProbeIsShown
    , testProperty
        "many probed values evaluated late are shown at their cycles"
        prop_manyLateProbesAreShown
    ]

{- | Threads that record at once keep their own values. Four wide signals
change in every cycle, so each one fills region after region of the spill
file, and each is forced by a thread of its own. Where a spill file is written through
a Handle (every platform but Linux), a write is a seek and then a write: a
thread switched out between the two wrote its region at the offset of the
other thread's region. The values are wide, so that the threads spend most
of their time writing: with 64-bit values, that happened too rarely to see.
-}
prop_threadsKeepTheirValues :: Property
prop_threadsKeepTheirValues = Hedgehog.withTests 6 $ property $ do
  wrong <- liftIO $ do
    createDirectoryIfMissing True dir
    (r, rec) <- withRecording (Just (dir </> "two-threads.spill")) $ do
      dones <-
        P.mapM
          ( \(name, k) -> do
              done <- newEmptyMVar
              _ <-
                forkIO
                  ( ( evaluate (P.length (P.filter (P.== 0) (sampleN cycles (traceSignalC name (counter k)))))
                        *> pure ()
                    )
                      `finally` putMVar done ()
                  )
              pure done
          )
          [(name, k) | k <- [1 .. 4], let name = "s" <> P.show k]
      P.mapM_ takeMVar dones
    P.either throwIO pure r
    wrong <- P.mapM (check rec) (recordedSignals rec)
    discardRecording rec
    pure (P.concat wrong)
  wrong === []
 where
  dir = "waveforms/test-recorder"
  cycles = 4000 :: Int
  counter :: Integer -> Signal System (BitVector 32768)
  counter k = P.fmap (\i -> P.fromInteger (i P.* 2654435761 P.+ k)) (fromList [0 ..])
  check rec s = do
    let k = P.read (P.takeWhile (P./= '@') (P.drop 1 (signalKey s)))
        want =
          [ (c, P.Just (0, P.fromInteger (P.toInteger c P.* 2654435761 P.+ k)))
          | c <- [0 .. cycles P.- 2]
          ]
    got <- try @SomeException (recordedChanges rec s)
    pure $ case got of
      P.Left e -> [signalKey s <> ": " <> displayException e]
      P.Right cs
        | P.take (cycles P.- 1) cs P.== want -> []
        | P.otherwise -> [signalKey s <> ": the values differ from the design's"]
