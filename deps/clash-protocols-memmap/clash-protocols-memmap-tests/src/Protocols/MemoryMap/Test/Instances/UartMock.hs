-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
module Protocols.MemoryMap.Test.Instances.UartMock where

import Clash.Prelude

import GHC.Stack (HasCallStack)
import Protocols (Circuit, ToConstBwd, toSignals)
import Protocols.Experimental.Wishbone (Wishbone, WishboneMode(..), WishboneS2M, WishboneM2S)
import Protocols.MemoryMap (Mm, withName, MemoryMap, getMMAny)
import Protocols.MemoryMap.Registers.WishboneStandard (deviceWbI, deviceConfig, registerConfig, registerWbI_)
import Clash.Class.BitPackC (ByteOrder (LittleEndian))
import Protocols.MemoryMap.Test.Interconnect (interconnect)

topEntity ::
  Clock System ->
  Signal System (WishboneM2S 30 4) ->
  Signal System (WishboneS2M 4)
topEntity clk input = output
 where
  fn = toSignals (withClockResetEnable clk resetGen enableGen someCircuit)
  ((_mm, output), ()) = fn (((), input), ())

mm :: MemoryMap
mm = getMMAny $ withClockResetEnable @System clockGen resetGen enableGen someCircuit

someCircuit ::
  forall dom.
  (HasCallStack, HiddenClockResetEnable dom, HasCallStack) =>
  Circuit (ToConstBwd Mm, Wishbone dom 'Standard 30 4) ()
someCircuit = circuit $ \(mm, master) -> do
  [a, b] <- interconnect -< (mm, master)
  withName "A" magicUart -< a
  withName "B" magicUart -< b

magicUart ::
  (HasCallStack, HiddenClockResetEnable dom, KnownNat addrWidth) =>
  Circuit (ToConstBwd Mm, Wishbone dom 'Standard addrWidth 4) ()
magicUart =
  let
    ?byteOrder = LittleEndian
  in
  circuit $ \(mm, wb) -> do
    [dataWb] <- deviceWbI (deviceConfig "Uart") -< (mm, wb)
    registerWbI_ (registerConfig "data" "") (0 :: BitVector 8) -< (dataWb, Fwd (pure Nothing))
