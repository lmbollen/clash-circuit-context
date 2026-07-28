-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Protocols.MemoryMap.Test.Utils where

import Prelude
import Control.Exception
import System.FilePath
import System.Directory

-- | Recursive function that returns a parent directory containing a certain filename.
findParentContaining :: String -> IO FilePath
findParentContaining filename = goUp =<< getCurrentDirectory
 where
  goUp :: FilePath -> IO FilePath
  goUp path
    | isDrive path = throwIO $ userError $ "Could not find " <> filename
    | otherwise = do
        exists <- doesFileExist (path </> filename)
        if exists
          then return path
          else goUp (takeDirectory path)
