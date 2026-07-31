-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.DnaPortE2 where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Clash.Cores.Xilinx.Unisim.DnaPortE2
import Data.Char
import Data.Maybe
import Numeric
import Protocols.Experimental.Simulate (sampleC)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Bittide.Instances.Tests.DnaPortE2 (dut, peConfigSim)
import Bittide.ProcessingElement
import Control.Exception (evaluate)
import Clash.CircuitContext.Waveform (newWaveformSlot, withWaveformOnFailure)

import Clash.CircuitContext (HasCircuitContext, withoutCircuitContext)

import qualified Prelude as P

sim :: IO ()
sim = do
  peConfig <- peConfigSim
  putStr $ simResult peConfig

simResult :: PeConfig 4 -> String
simResult peConfig = withoutCircuitContext (simStream peConfig)

-- | The DUT's UART output as a lazy stream; consume only as much as needed.
simStream :: (HasCircuitContext) => PeConfig 4 -> String
simStream peConfig = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream =
    sampleC def
      $ withClockResetEnable clockGen (resetGenN d2) enableGen
      $ dut @System peConfig

-- | Test whether we can read the DNA from the DNA port peripheral.
case_dna_port_self_test :: Assertion
case_dna_port_self_test = do
  peConfig <- peConfigSim
  -- SINGLE run: the assertion's own lazy simulation is recorded live (see
  -- 'withWaveformLazy'); parsing stops at the first output line, and the
  -- waveform covers exactly the cycles that were simulated — no separate
  -- strict re-run over a fixed window.
  wf <- newWaveformSlot "dna_port_self_test"
  withWaveformOnFailure wf 100_000 (simStream peConfig) $ \s -> do
    receivedDna <- evaluate (parseResult s)
    let
      msg =
        "Received dna "
          <> showHex receivedDna ""
          <> " not equal to expected dna "
          <> showHex simDna2 ""
    assertBool msg (receivedDna == simDna2)

parseResult :: String -> BitVector 96
parseResult = pack . (read :: String -> Unsigned 96) . P.head . lines

tests :: TestTree
tests = $(testGroupGenerator)
