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

import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Clash.CircuitContext.Waveform (waveformSlotPath)

import Example.Hedgehog (Artifacts (..))
import Example.SingleRun (singleRunVcd)
import Test.Vcd (asInts, cycleCount, decodeVCD, model)

tests :: Artifacts -> TestTree
tests art =
  testGroup
    "Test.ExampleOutput"
    [ testCase "the single-run waveform covers the whole run" $ do
        vcd <- readFile singleRunVcd
        cycleCount vcd @?= 32
    , testCase "the kept waveform holds the accumulator's own run" $ do
        acc <-
          readIORef (largestModel art)
            >>= maybe (assertFailure "the largest case never fired") pure
        let n = length acc
        vcd <- readFile (waveformSlotPath (largestSlot art))
        wave <-
          maybe (assertFailure "acc missing from VCD") pure $
            Map.lookup "acc" (decodeVCD n vcd)
        asInts wave @?= model acc
    , testCase "the sized case left a small, non-empty waveform" $ do
        vcd <- readFile (waveformSlotPath (sizedSlot art))
        assertBool "at least one cycle" (cycleCount vcd > 0)
        -- Size 15 of `Gen.list (Range.linear 1 32)` cannot generate the
        -- full-size case; the point of picking a size is a SMALL file.
        assertBool "smaller than the largest case" (cycleCount vcd < 32)
    , testCase "the counterexample waveform is the minimal shrunk case" $ do
        -- Any failing list shrinks to [0]; the off-by-one DUT then computes
        -- 1. One cycle, one wrong value — and that cycle lives entirely in
        -- the recorder's uncommitted tail, so this also proves a one-cycle
        -- counterexample survives capture.
        vcd <- readFile (waveformSlotPath (counterexampleSlot art))
        cycleCount vcd @?= 1
        wave <-
          maybe (assertFailure "wrong missing from VCD") pure $
            Map.lookup "wrong" (decodeVCD 1 vcd)
        asInts wave @?= [Just 1]
    ]
