{-# LANGUAGE MultiWayIf #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The directory a file will go in, made and checked before the run that writes
the file: a path whose directory cannot hold a file then fails at once, with
the directory that is in the way, not after the simulation, and not with
@createDirectory@'s \"already exists (File exists)\" for a part of the path
that is a file.
-}
module Clash.CircuitContext.Internal.Directory (
  Problem (..),
  problemMessage,
  directoryProblem,
) where

import Control.Exception (IOException, displayException, try)
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesPathExist,
  removeFile,
 )
import System.FilePath (takeDirectory)
import System.IO (hClose, openTempFile)
import System.IO.Error (isPermissionError)

-- | Why a file cannot be created in a directory.
data Problem
  = -- | This part of the path (absolute) is a file, not a directory.
    NotADirectory FilePath
  | -- | This directory (absolute) does not let this process create a file in it.
    NotWritable FilePath
  | -- | Another error of the file system.
    Other IOException

-- | A problem in words, with its directory.
problemMessage :: Problem -> String
problemMessage (NotADirectory dir) = dir <> " is a file, not a directory"
problemMessage (NotWritable dir) = dir <> " is not writable"
problemMessage (Other err) = displayException err

{- | Make this directory, an absolute path, with its missing parents, and
create and delete a file in it. Nothing, or why no file can be created in
it: the nearest part of the path that exists is a file, or is a directory
in which this process cannot create (a directory or a file).
-}
directoryProblem :: FilePath -> IO (Maybe Problem)
directoryProblem dir = do
  made <- try (createDirectoryIfMissing True dir)
  case made of
    Left err -> Just <$> explain err
    Right () -> do
      created <- try (openTempFile dir ".clash-circuit-context.tmp")
      case created of
        Left err -> Just <$> explain err
        Right (tmp, h) -> hClose h >> removeFile tmp >> pure Nothing
 where
  explain err = do
    blocking <- nearestExisting dir
    isDirectory <- doesDirectoryExist blocking
    pure $
      if
        | not isDirectory -> NotADirectory blocking
        | isPermissionError err -> NotWritable blocking
        | otherwise -> Other err
  nearestExisting d = do
    there <- doesPathExist d
    if there || takeDirectory d == d then pure d else nearestExisting (takeDirectory d)
