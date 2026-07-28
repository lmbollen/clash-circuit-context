-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Df.ElasticBufferWb where

import Bittide.Instances.Tests.ElasticBufferWb
import Bittide.ProcessingElement (PeConfig)
import Clash.CircuitContext (HasCircuitContext, withoutCircuitContext)
import Clash.Explicit.Prelude
import Data.Char (chr)
import Data.Maybe (catMaybes, mapMaybe)
import Protocols
import Protocols.Experimental.Simulate (SimulationConfig (..), sampleC)
import Protocols.Idle
import Protocols.MemoryMap
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Control.Exception (evaluate)
import Tests.Waveform (withWaveformLive)
import VexRiscv (DumpVcd)

-- | Simulate the UART output of the elastic buffer test
sim :: IO ()
sim = do
  dumpVcd <- getDumpVcd
  peConfig <- peConfigSim
  putStr $ simResult dumpVcd peConfig

simResult :: (HasCallStack) => DumpVcd -> PeConfig 5 -> String
simResult dumpVcd peConfig = withoutCircuitContext (simStream dumpVcd peConfig)

-- | Like 'simResult', but under the caller's circuit context.
simStream :: (HasCallStack, HasCircuitContext) => DumpVcd -> PeConfig 5 -> String
simStream dumpVcd peConfig = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream = sampleC def{timeoutAfter = 300_000} testCircuit

  testCircuit :: Circuit () (Df XilinxSystem (BitVector 8))
  testCircuit = idleSource |> ignoreMM |> dut dumpVcd peConfig

{- | Test whether the elastic buffer can be controlled via Wishbone.
The firmware runs multiple tests and outputs a result for each and ends with
"All elastic buffer tests passed" if all tests succeed.
-}
case_elastic_buffer_wb_test :: Assertion
case_elastic_buffer_wb_test = do
  dumpVcd <- getDumpVcd
  peConfig <- peConfigSim
  -- SINGLE run: assertion simulation recorded live (trailing 100k window).
  (ok, uartString) <-
    withWaveformLive "elastic_buffer_wb_test" 100_000 (simStream dumpVcd peConfig) $
      \uartString -> do
        ok <- evaluate (firstTrue $ mapMaybe checkLine $ lines uartString)
        pure (ok, uartString)
  assertBool
    ("Received the following from the CPU over UART:\n" <> uartString)
    ok
 where
  firstTrue (True : _) = True
  firstTrue _ = False

  checkLine :: String -> Maybe Bool
  checkLine line
    | line == "All elastic buffer tests passed" = Just True
    | line == "Some elastic buffer tests failed" = Just False
    | otherwise = Nothing

tests :: TestTree
tests = $(testGroupGenerator)
