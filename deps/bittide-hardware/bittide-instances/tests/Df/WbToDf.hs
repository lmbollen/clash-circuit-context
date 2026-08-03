-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Df.WbToDf where

import Bittide.Instances.Tests.WbToDf
import Clash.Explicit.Prelude
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char
import Data.Maybe (catMaybes)
import Hedgehog
import Clash.CircuitContext (HasCircuitContext, withoutCircuitContext)
import Protocols
import Protocols.Experimental.Hedgehog
import Protocols.Experimental.Simulate (SimulationConfig (..), sampleC)
import Protocols.Idle
import Protocols.MemoryMap
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Clash.CircuitContext.Waveform (newWaveformSlot, waveformsRequested, withWaveformLazyWhen, writeWaveformSlot)

import qualified Prelude as P

-- | Test whether the wbToDf component correctly converts wishbone writes to Df stream transactions
prop_wb_to_df_test :: Property
prop_wb_to_df_test =
  -- Each case simulates a full CPU firmware boot, so run the property ONCE
  -- (the stall generator still exercises backpressure in that run).
  withTests 1 $ property $ do
    dumpVcd <- liftIO getDumpVcd
    peConfig <- liftIO peConfigSim
    let
      impl :: Circuit (Df System ()) (Df System SomeAdt, Df System (BitVector 8))
      impl = idleSink |> (unMemmap $ withoutCircuitContext (dut dumpVcd peConfig))
      -- The DUT's outputs as lazy streams, under the recording context.
      streams ::
        (HasCircuitContext) =>
        ([Maybe SomeAdt], [Maybe (BitVector 8)])
      streams = sampleC def{timeoutAfter = 200_000} (unMemmap (dut dumpVcd peConfig))
    -- SINGLE bounded capture run: the consumer stops as soon as the expected
    -- number of 'SomeAdt' outputs has been produced.
    -- Artifact capture only, and it has to be: the assertion below is
    -- 'propWithModelT', which runs its OWN simulation (with stalls) rather
    -- than this one, so capturing this run on that failure would show a
    -- waveform of different stimulus. Set CCC_WAVEFORMS to record it.
    keep <- liftIO waveformsRequested
    wf <- newWaveformSlot "wb_to_df_test"
    _ <-
      liftIO $
        withWaveformLazyWhen keep wf 100_000 streams $ \(adts, _uart) ->
          evaluate (forceList (P.take (length testValue) (catMaybes adts)))
    writeWaveformSlot wf
    propWithModelT eOpts (pure []) model impl prop
 where
  eOpts =
    defExpectOptions
      { eoSampleMax = 1_000_000
      , eoStopAfterEmpty = Just 1_000 -- Increase when using UART
      , eoStallsMax = 1000
      , eoConsecutiveStalls = 10
      }

  model _ = (toList testValue, [])
  prop (expected, _) (actual, uart) = do
    footnote $ "Log: " <> fmap (chr . fromIntegral) uart
    actual === expected

-- | Force a list's spine and elements (WHNF) so the consumer bounds the run.
forceList :: [a] -> ()
forceList = P.foldr (\x r -> x `seq` r) ()

tests :: TestTree
tests = $(testGroupGenerator)
