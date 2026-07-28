-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Protocols.MemoryMap.Test.MemoryMaps where

import Prelude
import Control.Monad
import Language.Haskell.TH
import System.FilePath
import System.Directory

import Protocols.MemoryMap (MemoryMap(..))
import Protocols.MemoryMap.Check
import Protocols.MemoryMap.Test.Utils

import qualified Protocols.MemoryMap.Json as Json
import qualified Data.ByteString.Lazy as BS

import qualified Protocols.MemoryMap.Test.Instances.UartMock as UartMock
import qualified Protocols.MemoryMap.Test.Instances.InterconnectTypeTests as ITT
import Clash.Class.BitPackC (ByteOrder(..))


$(do
    let
      withBO :: ByteOrder -> ((?byteOrder :: ByteOrder) => a) -> a
      withBO bo action =
         let ?byteOrder = bo
         in action

    -------------------------------
    -- MEMORY MAPS               --
    --                           --
    -- Add new memory maps here  --
    -------------------------------
    let memoryMaps =
          [ ("UartMock", UartMock.mm)
          , ("InterconnectTypeTests", withBO LittleEndian ITT.mm)
          ] :: [(String, MemoryMap)]

    memMapDir <- runIO $ do
      root <- findParentContaining "cabal.project"
      let dir = root </> "test_memory_maps"
      -- clean existing memory maps
      removePathForcibly dir

      createDirectoryIfMissing True dir
      pure dir

    let convertedTrees = map (\m -> convert m.tree) (snd <$> memoryMaps)
    let normalizedTrees = map normalizeRelTree convertedTrees

    let absResults =
          flip map (memoryMaps `zip` normalizedTrees) $
            \((_name, mm), normalised) ->
              runMakeAbsolute mm.deviceDefs (0x0000_0000, 0xFFFF_FFFF) normalised

    forM_ (memoryMaps `zip` absResults) $ \((mmName, mm), (absTree, errors)) -> do
      if not $ null errors
        then do
          -- report errors
          forM_ errors $ \err -> do
            reportError (getErrorMessage err)
        else do
          -- output JSON

          let json = Json.memoryMapJson Json.LocationSeparate mm.deviceDefs absTree
          let jsonPath = memMapDir </> mmName <.> "json"
          runIO $ BS.writeFile jsonPath $ Json.encode json

    pure []
 )
