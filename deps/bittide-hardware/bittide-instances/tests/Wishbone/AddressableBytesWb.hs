-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Wishbone.AddressableBytesWb where

import Clash.Prelude

import Data.List (isInfixOf)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.TH (testGroupGenerator)

import Bittide.Instances.Tests.AddressableBytesWb (
  dutUartStreamC,
  getDumpVcd,
  peConfigSim,
 )
import Protocols.MemoryMap (unMemmap)
import Control.Exception (evaluate)
import Tests.Waveform (withWaveformLive)

case_sim :: Assertion
case_sim = do
  dumpVcd <- getDumpVcd
  peConfig <- peConfigSim
  -- SINGLE run: the assertion's own lazy simulation is recorded live; the
  -- waveform (trailing 100k-cycle window) covers what was actually simulated.
  (ok, simResult) <-
    withWaveformLive "addressable_bytes_wb_test" 100_000 (dutUartStreamC dumpVcd peConfig) $
      \simResult -> do
        ok <- evaluate ("RESULT: OK" `isInfixOf` simResult)
        pure (ok, simResult)
  assertBool ("Received the following from the CPU over UART:\n" <> simResult) ok

tests :: TestTree
tests = $(testGroupGenerator)
