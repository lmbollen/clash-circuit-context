{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

{- | Waveforms from hedgehog properties, written the way a downstream suite
writes them: with "Clash.CircuitContext.Waveform.Hedgehog"'s
'simulateWaveform'. A property simulates its design through it and checks the
samples as usual; the waveform of the counterexample is written when Hedgehog
reports the failure, and 'AtSize' also keeps one passing case.

"Test.ExampleOutput" later decodes the files these examples wrote.
-}
module Example.Hedgehog (
  scrubExamples,
  tests,

  -- * The designs the examples run
  accDut,
  accModel,

  -- * Where the examples write
  largestVcd,
  sizedVcd,
  counterexampleVcd,
) where

import qualified Prelude as P

import Control.Monad (when)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((-<.>), (<.>), (</>))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.Hedgehog (testProperty)

import Hedgehog (Property, forAll, property, withTests, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Explicit.Prelude

import Clash.CircuitContext (HasCircuitContext, traceSignalC)
import Clash.CircuitContext.Waveform.Hedgehog (Capture (..), simulateWaveform)

--------------------------------------------------------------------------------
-- The designs
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
accDut inp0 = out
 where
  inp = traceSignalC "inp" inp0
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

-- | Deliberately wrong: adds one to every sample. Its example asserts it
-- equals its input, fails, and leaves the counterexample.
offByOne ::
  (HasCircuitContext) =>
  Signal System (Unsigned 8) ->
  Signal System (Unsigned 8)
offByOne inp = traceSignalC "wrong" (inp + 1)

--------------------------------------------------------------------------------
-- Where the examples write
--------------------------------------------------------------------------------

vcdOf :: String -> FilePath
vcdOf name = "waveforms" </> name <.> "vcd"

largestVcd, sizedVcd, counterexampleVcd :: FilePath
largestVcd = vcdOf "example-largest-case"
sizedVcd = vcdOf "example-sized-case"
counterexampleVcd = vcdOf "example-counterexample"

-- | Delete the files of a previous run, so the output tests can only be
-- satisfied by THIS run.
scrubExamples :: IO ()
scrubExamples =
  P.mapM_
    (\p -> doesFileExist p >>= \e -> when e (removeFile p))
    [f | vcd <- [largestVcd, sizedVcd, counterexampleVcd], f <- [vcd, vcd -<.> "json"]]

--------------------------------------------------------------------------------
-- The examples
--------------------------------------------------------------------------------

{- | The artifact pattern: a PASSING property that keeps its largest case.
Hedgehog runs a 100-test property at sizes 0 to 99, so @AtSize 99@ keeps the
last, most thorough case, and the 99 other cases never record. The waveform
traces the input beside the sum, so "Test.ExampleOutput" can check one
against the other.
-}
prop_keepLargest :: Property
prop_keepLargest = property $ do
  xs <- forAll genStim
  let n = P.length xs
  out <- simulateWaveform (AtSize 99) "example-largest-case" n (accDut (stim xs))
  out === accModel xs

{- | The same, at a size the property actually reaches: this one runs
@withTests 20@, so it sees sizes 0 to 19 only. A small size also gives a
waveform short enough to read end to end.
-}
prop_keepSizedCase :: Property
prop_keepSizedCase = withTests 20 $ property $ do
  xs <- forAll genStim
  let n = P.length xs
  out <- simulateWaveform (AtSize 15) "example-sized-case" n (accDut (stim xs))
  out === accModel xs

{- | The failure pattern: the check is ordinary Hedgehog code after the
simulation. The property fails, Hedgehog shrinks it, and its report ends with
the path of the waveform of the counterexample it reports.
-}
prop_offByOneIsCaught :: Property
prop_offByOneIsCaught = property $ do
  xs <- forAll (Gen.list (Range.linear 1 8) (Gen.integral Range.linearBounded))
  let n = P.length xs
  out <- simulateWaveform Counterexample "example-counterexample" n (offByOne (stim xs))
  out === xs

tests :: TestTree
tests =
  testGroup
    "Example.Hedgehog"
    [ testProperty "a passing property keeps its largest case" prop_keepLargest
    , testProperty "…or a case of a chosen, reachable size" prop_keepSizedCase
    , testCase "a wrong DUT fails, leaving its shrunk counterexample" $ do
        ok <- Hedgehog.check prop_offByOneIsCaught
        assertBool "the deliberately wrong property must fail" (P.not ok)
    ]
