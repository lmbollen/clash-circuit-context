{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

{- | Instrumenting hedgehog properties: the three shapes from
"Clash.CircuitContext.Waveform.Hedgehog", written the way a downstream suite
writes them.

A property runs its body once per generated case but should leave at most ONE
waveform, and the decision which one is made BEFORE simulating, so losing
cases never record. The state that carries that decision — the slot, and the
fired-once 'IORef' — is owned by the CALLER, one per property: made inside
the body it would be fresh every case and "fire at most once" would mean
nothing. 'Artifacts' is that ownership made explicit, and it is also how
"Test.ExampleOutput" later finds the files these examples wrote.
-}
module Example.Hedgehog (
  Artifacts (..),
  newArtifacts,
  tests,

  -- * The designs the examples run
  accDut,
  accModel,
) where

import qualified Prelude as P

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, writeIORef)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((-<.>))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.Hedgehog (testProperty)

import Hedgehog (Property, forAll, property, withTests, (===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Explicit.Prelude

import Clash.CircuitContext (HasCircuitContext, traceSignalC)
import Clash.CircuitContext.Waveform (
  WaveformSlot,
  newWaveformSlot,
  waveformSlotPath,
 )
import Clash.CircuitContext.Waveform.Hedgehog (
  recordCaseOfSize,
  recordLargestCase,
  withWaveformCase,
  withWaveformOnCounterexample,
 )

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

-- | Deliberately wrong: adds one to every sample. Its example asserts it
-- equals its input, fails, and leaves the counterexample.
offByOne ::
  (HasCircuitContext) =>
  Signal System (Unsigned 8) ->
  Signal System (Unsigned 8)
offByOne inp = traceSignalC "wrong" (inp + 1)

--------------------------------------------------------------------------------
-- Caller-owned capture state
--------------------------------------------------------------------------------

-- | One property's worth of capture state each, plus what the examples
-- promised to write — so the output tests can hold them to it.
data Artifacts = Artifacts
  { largestSlot :: WaveformSlot
  , largestFired :: IORef Bool
  , largestModel :: IORef (Maybe [Unsigned 8])
  -- ^ The model of the case that was kept, stashed by the property.
  , sizedSlot :: WaveformSlot
  , sizedFired :: IORef Bool
  , counterexampleSlot :: WaveformSlot
  }

-- | Fresh state, with any files from a previous run scrubbed so the output
-- tests can only be satisfied by THIS run.
newArtifacts :: IO Artifacts
newArtifacts = do
  art <-
    Artifacts
      <$> newWaveformSlot "example-largest-case"
      <*> newIORef False
      <*> newIORef Nothing
      <*> newWaveformSlot "example-sized-case"
      <*> newIORef False
      <*> newWaveformSlot "example-counterexample"
  P.mapM_ (scrub . ($ art)) [largestSlot, sizedSlot, counterexampleSlot]
  pure art
 where
  scrub slot = do
    let vcd = waveformSlotPath slot
    P.mapM_
      (\p -> doesFileExist p >>= \e -> when e (removeFile p))
      [vcd, vcd -<.> "json"]

--------------------------------------------------------------------------------
-- The examples
--------------------------------------------------------------------------------

{- | The artifact pattern: a PASSING property that keeps its largest case.
'recordLargestCase' fires on the size-99 case — the last, most thorough one
of a default 100-test property — so the 99 losing cases never record. The
kept case's model is stashed for "Test.ExampleOutput".
-}
prop_keepLargest :: Artifacts -> Property
prop_keepLargest art = property $ do
  xs <- forAll genStim
  keep <- recordLargestCase (largestFired art)
  let n = P.length xs
  withWaveformCase keep (largestSlot art) n (sampleN n (accDut (stim xs))) $ \out -> do
    let acc = accModel xs
    when keep (liftIO (writeIORef (largestModel art) (Just acc)))
    out === acc

{- | The same, at a chosen size. The size to keep must be one the property
actually REACHES: this one runs @withTests 20@, so it only ever sees sizes
0..19 and 'recordLargestCase' (size 99) would never fire at all —
'recordCaseOfSize' 15 does. Small sizes also mean a waveform readable end to
end; hedgehog's @Size@ is the one knob on how big a case, and therefore the
file, gets.
-}
prop_keepSizedCase :: Artifacts -> Property
prop_keepSizedCase art = withTests 20 $ property $ do
  xs <- forAll genStim
  keep <- recordCaseOfSize (sizedFired art) 15
  let n = P.length xs
  withWaveformCase keep (sizedSlot art) n (sampleN n (accDut (stim xs))) $ \out ->
    out === accModel xs

{- | The failure pattern: assertions live in the consumer, so when they fail
the case is re-run recording and the SHRUNK counterexample survives in the
slot — the report ends with the file's absolute path. Passing cases cost
nothing.
-}
prop_offByOneIsCaught :: WaveformSlot -> Property
prop_offByOneIsCaught slot = property $ do
  xs <- forAll (Gen.list (Range.linear 1 8) (Gen.integral Range.linearBounded))
  let n = P.length xs
  withWaveformOnCounterexample slot n (sampleN n (offByOne (stim xs))) $ \out ->
    out === xs

tests :: Artifacts -> TestTree
tests art =
  testGroup
    "Example.Hedgehog"
    [ testProperty
        "a passing property keeps its largest case"
        (prop_keepLargest art)
    , testProperty
        "…or a case of a chosen, reachable size"
        (prop_keepSizedCase art)
    , testCase "a wrong DUT fails, leaving its shrunk counterexample" $ do
        ok <- Hedgehog.check (prop_offByOneIsCaught (counterexampleSlot art))
        assertBool "the deliberately wrong property must fail" (P.not ok)
    ]
