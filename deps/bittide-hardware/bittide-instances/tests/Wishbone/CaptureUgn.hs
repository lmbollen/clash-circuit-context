-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
-- Don't warn about partial functions: this is a test, so we'll see it fail.
{-# OPTIONS_GHC -Wno-x-partial #-}

module Wishbone.CaptureUgn where

import Clash.Explicit.Prelude
import Clash.Prelude (withClockResetEnable)

import Clash.Signal.Internal
import Data.Char
import Data.Maybe
import Numeric
import Protocols.Experimental.Simulate (sampleC)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Bittide.Instances.Tests.CaptureUgn (dut, peConfigSim)
import Clash.CircuitContext (HasCircuitContext, withoutCircuitContext)
import Control.Exception (evaluate)
import Tests.Waveform (withWaveformLive)

import qualified Data.List as L

{- | Test whether we can read the local and remote sequence counters from the captureUgn
peripheral.
-}
case_capture_ugn_self_test :: Assertion
case_capture_ugn_self_test = do
  peConfig <- peConfigSim
  let
    msg actualLocalCounter actualRemoteCounter =
      "Received local counter 0x"
        <> showHex actualLocalCounter ""
        <> " and remote counter 0x"
        <> showHex actualRemoteCounter ""
        <> " are not equal to expected counters 0x"
        <> showHex expectedLocalCounter ""
        <> " and 0x"
        <> showHex expectedRemoteCounter ""
    -- Note we do 'succ' on the "expectedLocalCounter" to account for the
    -- dflipflop inserted on captureUgn's link input
    (succ -> expectedLocalCounter, unpack -> expectedRemoteCounter) = getSequenceCounters $ bundle (localCounter, eb)
    clk = clockGen
    rst = resetGenN d2
    ena = enableGen
    simStream :: (HasCircuitContext) => String
    simStream = chr . fromIntegral <$> catMaybes uartStream
     where
      uartStream =
        sampleC def
          $ withClockResetEnable clk rst enableGen
          $ dut @System peConfig eb localCounter

    {- The local counter starts counting up from 0x1122334411223344. The elastic buffer
    outputs Nothing for 1000 cycles, after which it will  start outputting a decreasing
    counter starting at 0xaabbccddeeff1234.
    -}
    localCounter = register clk rst ena 0xaabbccddeeff1234 (localCounter + 1)
    eb = regEn clk rst ena Nothing remoteStarted (Just . pack <$> remoteCounter)
     where
      remoteCounter = register clk rst ena (0x1122334411223344 :: Unsigned 64) (remoteCounter - 1)
      remoteStarted = counter .==. pure maxBound
      counter = register clk rst ena (0 :: Index 1000) (satSucc SatBound <$> counter)
  -- SINGLE run: the assertion's own lazy simulation is recorded live; the
  -- waveform (trailing 100k-cycle window) covers what was actually simulated.
  (actualLocalCounter, actualRemoteCounter) <-
    withWaveformLive "capture_ugn_self_test" 100_000 simStream (evaluate . parseResult)
  assertBool
    (msg actualLocalCounter actualRemoteCounter)
    ( actualLocalCounter
        == expectedLocalCounter
        && actualRemoteCounter
        == expectedRemoteCounter
    )

{- | Simulation function which matches the remote counter to the correct sample
of the local counter.
-}
getSequenceCounters ::
  Signal dom (Unsigned 64, Maybe (BitVector 64)) ->
  (Unsigned 64, BitVector 64)
getSequenceCounters ((a, Just b) :- _) = (a, b)
getSequenceCounters ((_, Nothing) :- xs) = getSequenceCounters xs

parseResult :: String -> (Unsigned 64, Unsigned 64)
parseResult = (read :: String -> (Unsigned 64, Unsigned 64)) . L.head . lines

tests :: TestTree
tests = $(testGroupGenerator)
