-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}
-- Auto-instrument the DUT: the plugin scopes 'dutWithPeConfig' and nests the
-- instrumented 'transmitRingBuffer'/'receiveRingBuffer' under it.

module Bittide.Instances.Tests.RingBuffer where

import Clash.Explicit.Prelude hiding (delayN)
import Clash.Prelude (
  HiddenClockResetEnable,
  delayN,
  hasClock,
  withClockResetEnable,
 )

import Clash.Cores.Xilinx.BlockRam (tdpbram)
import Data.Char (chr)
import Data.Maybe (catMaybes)
import GHC.Stack (HasCallStack)
import Project.FilePath
import Protocols
import Protocols.Df.Extra (tdpbramRamOp)
import Protocols.Experimental.Simulate (SimulationConfig (..), sampleC)
import Protocols.Idle
import Protocols.MemoryMap
import VexRiscv (DumpVcd (NoDumpVcd))

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.Instances.Common
import Bittide.ProcessingElement
import Bittide.RingBuffer
import Bittide.SharedTypes (withLittleEndian)
import Bittide.Wishbone
import Clash.CircuitContext (HasCircuitContext, withoutCircuitContext)

import qualified Data.List as L

createDomain vSystem{vName = "Slow", vPeriod = hzToPeriod 1_000_000}

-- | Memory depth for the ringbuffers (16 entries of 8 bytes each)
memDepth :: SNat 16
memDepth = SNat

dutMM :: (HasCallStack) => Protocols.MemoryMap.MemoryMap
dutMM =
  (\(SimOnly mm, _) -> mm)
    $ withClockResetEnable @Slow clockGen (resetGenN d2) enableGen
    $ toSignals
      ( withoutCircuitContext
          (dutWithPeConfig d0 (emptyPeConfig (SNat @IMemWords) (SNat @DMemWords) d0 d0 False vexRiscv0))
      )
      ((), pure $ deepErrorX "memoryMap")

-- | Parameterized DUT that loads a specific firmware binary with configurable latency.
dutWithPeConfig ::
  ( HasCircuitContext
  , HasCallStack
  , HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , KnownNat latency
  ) =>
  SNat latency ->
  PeConfig 6 ->
  Circuit (ToConstBwd Mm) (Df dom (BitVector 8))
dutWithPeConfig latency peConfig = withLittleEndian $ circuit $ \mm -> do
  (uartRx, jtagIdle) <- idleSource
  [uartBus, wbTx, wbRx, timeBus] <-
    processingElement NoDumpVcd peConfig -< (mm, jtagIdle)
  (uartTx, _uartStatus) <- uartInterfaceWb d16 d2 uartBytes -< (uartBus, uartRx)
  txOut <- transmitRingBuffer (tdpbramRamOp tdpbram hasClock hasClock) memDepth -< wbTx
  txOutDelayed <- applyC (toSignal . delayN latency 0 . fromSignal) id -< txOut
  receiveRingBuffer (\ena -> blockRam hasClock ena (replicate memDepth 0)) memDepth
    -< (wbRx, txOutDelayed)
  _cnt <- timeWb Nothing -< timeBus
  idC -< uartTx
{-# OPAQUE dutWithPeConfig #-}

{- | Recording-friendly DUT (left side @()@) with a fixed zero latency and the
'System' domain, ready to be wrapped in a waveform capture. Kept context-flowing
(no 'withoutCircuitContext') so the plugin-instrumented ring buffers nest under
the captured VCD.
-}
dutForWaveform ::
  (HasCircuitContext, HasCallStack) => PeConfig 6 -> Circuit () (Df System (BitVector 8))
dutForWaveform peConfig = circuit $ do
  mm <- ignoreMM
  uartTx <-
    withClockResetEnable clockGen (resetGenN d2) enableGen (dutWithPeConfig @System d0 peConfig)
      -< mm
  idC -< uartTx

type IMemWords = DivRU (64 * 1024) 4
type DMemWords = DivRU (64 * 1024) 4

peConfigFromBinaryName :: String -> IO (PeConfig 6)
peConfigFromBinaryName binaryName = do
  peConfigFromElf
    (SNat @IMemWords)
    (SNat @DMemWords)
    (NameOnly binaryName)
    Release
    d0
    d0
    False
    vexRiscv0

takeUntilList :: (Eq a) => [a] -> [a] -> [a]
takeUntilList _ [] = []
takeUntilList prefix xs@(y : ys)
  | prefix `L.isPrefixOf` xs = []
  | otherwise = y : takeUntilList prefix ys

-- RingBuffer test simulation
simRingBuffer :: IO ()
simRingBuffer = putStr =<< simResultRingBuffer d4

simResultRingBuffer :: forall latency. (HasCallStack, KnownNat latency) => SNat latency -> IO String
simResultRingBuffer latency = do
  peConfig <- peConfigFromBinaryName "ring_buffer_test"
  pure (withoutCircuitContext (ringBufferStream latency peConfig))

-- | The ring-buffer test's UART output as a lazy stream, under the caller's
-- circuit context (so a live waveform capture can record the run).
ringBufferStream ::
  forall latency.
  (HasCallStack, HasCircuitContext, KnownNat latency) =>
  SNat latency ->
  PeConfig 6 ->
  String
ringBufferStream latency peConfig =
  takeUntilList "=== Test Complete ===" $ chr . fromIntegral <$> catMaybes uartStream
 where
  dutNoMm = circuit $ do
    mm <- ignoreMM
    uartTx <-
      withClockResetEnable clockGen (resetGenN d2) enableGen
        $ dutWithPeConfig @System latency peConfig
        -< mm
    idC -< uartTx
  uartStream = sampleC def{timeoutAfter = 1_000_000} dutNoMm
