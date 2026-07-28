-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about orphan instances, caused by `createDomain`.
{-# OPTIONS_GHC -Wno-orphans #-}
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.RingBuffer where

import Clash.Explicit.Prelude

import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import Data.String.Interpolate (i)
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH

import Bittide.Instances.Tests.RingBuffer (
  peConfigFromBinaryName,
  ringBufferStream,
 )
import Control.Exception (evaluate)
import qualified Prelude as P
import Tests.Waveform (withWaveformLive)

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_ring_buffer_test :: H.Property
prop_ring_buffer_test =
  -- This test is _very_ slow, so we only run it once.
  H.withTests 1 $ H.property $ do
    latency <- H.forAll $ Gen.integral (Range.constant 0 100)
    liftIO $ putStrLn $ "Testing ring_buffer_test with latency " <> show latency <> " cycles"
    -- SINGLE run: the assertion's own lazy simulation is recorded live; the
    -- waveform (trailing 100k-cycle window) covers what was actually simulated.
    result <- liftIO $ do
      pc <- peConfigFromBinaryName "ring_buffer_test"
      case someNatVal (fromInteger latency) of
        Just (SomeNat (_ :: Proxy n)) ->
          withWaveformLive "ring_buffer_test" 100_000 (ringBufferStream (SNat @n) pc) $
            \s -> evaluate (forceString s)
        Nothing -> error [i|Invalid latency value: #{latency}|]
    H.annotate [i|Result of ring_buffer_test with latency #{latency} cycles: \n#{result}|]
    H.assert ("TEST PASSED" `isInfixOf` result)

-- | Force a String to normal form (the stream must be fully consumed inside
-- the live-capture context).
forceString :: String -> String
forceString s = P.length s `seq` s

tests :: TestTree
tests = $(testGroupGenerator)
