-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.Watchdog where

-- Preludes
import Clash.Prelude

-- Local
import Bittide.Instances.Tests.Watchdog (dut, peConfigSim)
import Bittide.ProcessingElement

-- Other
import Data.Char
import Data.Maybe
import Protocols.Experimental.Simulate (sampleC)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Clash.CircuitContext (HasCircuitContext, withoutCircuitContext)
import Control.Exception (evaluate)
import Clash.CircuitContext.Waveform (newWaveformSlot, withWaveformOnFailure)

-- Qualified
import qualified Data.List as L

sim :: IO ()
sim = do
  peConfig <- peConfigSim
  putStrLn $ simResult peConfig

simResult :: PeConfig 6 -> String
simResult peConfig = withoutCircuitContext (simStream peConfig)

-- | Like 'simResult', but under the caller's circuit context.
simStream :: (HasCircuitContext) => PeConfig 6 -> String
simStream peConfig = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def (dut peConfig)

{- | Run the timing module self test with processingElement and inspect it's uart output.
The test returns names of tests and a boolean indicating if the test passed.
-}
case_time_rust_self_test :: Assertion
case_time_rust_self_test = do
  peConfig <- peConfigSim
  -- SINGLE run: assertion simulation recorded live (trailing 100k window).
  -- Force the WHOLE first line (not just its head 'Char') INSIDE the capture
  -- context: 'evaluate' alone stops at WHNF, which would advance the simulation
  -- by a single UART byte and leave the waveform with just one character. The
  -- 'L.length' forces the line's full spine, so every cycle that produces
  -- "Timeout took 50 microseconds" is simulated and recorded here rather than
  -- later, under 'assertEqual', when the recording context has already frozen.
  wf <- newWaveformSlot "watchdog_self_test"
  withWaveformOnFailure wf 100_000 (simStream peConfig) $ \s -> do
    let firstLine = L.head (lines s)
    _ <- evaluate (L.length firstLine)
    assertEqual "Measured timeout wrong " "Timeout took 50 microseconds" firstLine

tests :: TestTree
tests = $(testGroupGenerator)
