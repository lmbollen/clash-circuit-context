-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NumericUnderscores #-}
module Main (main) where

import Clash.Prelude

import Control.Monad (forM_)

import Protocols.MemoryMap
import Protocols.MemoryMap.Check.AbsAddress
import Clash.Class.BitPackC (ByteOrder (..))

import qualified Data.ByteString.Lazy as BS

import qualified Protocols.MemoryMap.Test.Instances.UartMock as UartMock
import qualified Protocols.MemoryMap.Test.Instances.InterconnectTypeTests as ITT
import Protocols.MemoryMap.Test.Instances.InterconnectTypeTests (runInterconnectTypeTests)
import Text.Printf (printf)
import Protocols.MemoryMap.Json (encode, memoryMapJson, LocationStorage (LocationSeparate))

withBO :: ByteOrder -> ((?byteOrder :: ByteOrder) => a) -> a
withBO bo val =
  let
    ?byteOrder = bo
  in val

memoryMapGeneration :: [MemoryMap]
memoryMapGeneration =
  [ UartMock.mm
  , withBO LittleEndian ITT.mm
  ]

main :: IO ()
main = do
  forM_ memoryMapGeneration $ \mm -> do
    let relTree = convert mm.tree
    let normTree = normalizeRelTree relTree
    let (absTree, _) = runMakeAbsolute mm.deviceDefs (0x0000_0000, 0xFFFF_FFFF) normTree

    let json = memoryMapJson LocationSeparate mm.deviceDefs absTree
    let content = encode json
    BS.putStr content
    putStrLn ""

  resultsLB <- withBO LittleEndian runInterconnectTypeTests
  putStrLn $ "Running Interconnect tests for Little"
  forM_ resultsLB $ \(name, res) -> do
    printf "Test %s resulted in: %s\n" name (show res)

  resultsBL <- withBO BigEndian runInterconnectTypeTests
  putStrLn $ "Running Interconnect tests for Big"
  forM_ resultsBL $ \(name, res) -> do
    printf "Test %s resulted in: %s\n" name (show res)
