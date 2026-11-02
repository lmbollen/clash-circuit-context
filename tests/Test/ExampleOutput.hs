{- | Hold the Example level to its word: decode the waveform files its tests
wrote and check they show the runs they claim to. Sequenced by tasty AFTER
the examples ("Main" wires the dependency), because these read what those
write.

"The waveform exists" is not the acceptance criterion — the run being IN it
is. Both truncation bugs this package has had (the uncommitted last cycle,
the consumer-bounded capture) produced files that existed and decoded to less
than the failing run.
-}
module Test.ExampleOutput (tests) where

import qualified Data.Map.Strict as Map

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

import Example.Hedgehog (accModel, counterexampleVcd, largestVcd, sizedVcd)
import Example.SingleRun (singleRunVcd)
import Test.Vcd (asInts, cycleCount, decodeVCD)

{- | A kept case of 'Example.Hedgehog.accDut' is the run of its own input: the
@acc@ it recorded is 'accModel' of the @inp@ it recorded, cycle by cycle.
'Example.Hedgehog' does not tell which case 'AtSize' kept, so the waveform has
to show it.
-}
holdsItsOwnRun :: FilePath -> Assertion
holdsItsOwnRun path = do
  vcd <- readFile path
  let n = cycleCount vcd
      waves = decodeVCD n vcd
      wave nm = maybe (assertFailure (nm <> " missing from " <> path)) pure (Map.lookup nm waves)
  inp <- traverse (maybe (assertFailure "an undefined input") pure) . asInts =<< wave "inp"
  acc <- wave "acc"
  asInts acc @?= map (Just . toInteger) (accModel (map fromInteger inp))

tests :: TestTree
tests =
  testGroup
    "Test.ExampleOutput"
    [ testCase "the single-run waveform covers the whole run" $ do
        vcd <- readFile singleRunVcd
        cycleCount vcd @?= 32
    , testCase "the kept largest case holds the accumulator's own run" $
        holdsItsOwnRun largestVcd
    , testCase "the sized case left a small waveform of its own run" $ do
        vcd <- readFile sizedVcd
        assertBool "at least one cycle" (cycleCount vcd > 0)
        -- Size 15 of `Gen.list (Range.linear 1 32)` cannot generate the
        -- full-size case; the point of picking a size is a SMALL file.
        assertBool "smaller than the largest case" (cycleCount vcd < 32)
        holdsItsOwnRun sizedVcd
    , testCase "the counterexample waveform is the minimal shrunk case" $ do
        -- Any failing list shrinks to [0]; the off-by-one DUT then computes
        -- 1. One cycle, one wrong value — and that cycle lives entirely in
        -- the recorder's uncommitted tail, so this also proves a one-cycle
        -- counterexample survives capture.
        vcd <- readFile counterexampleVcd
        cycleCount vcd @?= 1
        wave <-
          maybe (assertFailure "wrong missing from VCD") pure $
            Map.lookup "wrong" (decodeVCD 1 vcd)
        asInts wave @?= [Just 1]
    ]
