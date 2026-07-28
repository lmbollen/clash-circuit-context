-- SPDX-FileCopyrightText: 2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Prelude

import qualified Clash.Main as Clash

main :: IO ()
main = Clash.defaultMain ["Protocols.MemoryMap.Test.Instances.UartMock", "-main-is", "topEntity", "--verilog"]
