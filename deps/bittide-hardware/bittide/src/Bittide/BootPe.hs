-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.BootPe (BootPeBusses, bootPe) where

import Clash.Prelude
import Protocols

import Clash.Class.BitPackC (ByteOrder)
import GHC.Stack (HasCallStack)
import Protocols.MemoryMap (Mm)
import Protocols.Spi (Spi)
import VexRiscv

import Bittide.ClockControl.Si539xSpi (si539xSpiWb)
import Bittide.ProcessingElement (PeConfig (..), RemainingBusWidth, processingElement)
import Bittide.SharedTypes (BitboneMm)
import Bittide.Wishbone (timeWb, uartBytes, uartInterfaceWb)
import Clash.CircuitContext (HasCircuitContext)

type BootPeBusses = 6

{- | Processing element with builtin time component, UART interface and SI539x SPI
interface. It exports one bus for the transceiver component.
-}
{-# OPAQUE bootPe #-}
bootPe ::
  forall dom.
  ( HasCallStack
  , HasCircuitContext
  , HiddenClockResetEnable dom
  , 1 <= DomainPeriod dom
  , ?byteOrder :: ByteOrder
  ) =>
  PeConfig BootPeBusses ->
  Circuit
    ( ToConstBwd Mm
    , Jtag dom
    )
    ( "UART_BYTES" ::: Df dom (BitVector 8)
    , "SPI_DONE" ::: CSignal dom Bool
    , Spi dom
    , "TRANSCEIVER" ::: BitboneMm dom (RemainingBusWidth BootPeBusses)
    )
bootPe peConfig = circuit $ \(mm, jtag) -> do
  [timeBus, uartBus, siBus, transceiverBus] <-
    processingElement NoDumpVcd peConfig -< (mm, jtag)

  Fwd _localCounter <- timeWb Nothing -< timeBus
  (uartOut, _uartStatus) <-
    uartInterfaceWb d16 d16 uartBytes -< (uartBus, Fwd (pure Nothing))
  (spiDone, spiOut) <- si539xSpiWb (SNat @(Microseconds 10)) -< siBus

  -- XXX: Should the transceiver just be part of the PE? This would add a whooole
  --      bunch of constraints to it.
  idC -< (uartOut, spiDone, spiOut, transceiverBus)
