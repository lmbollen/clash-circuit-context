{-# LANGUAGE OverloadedStrings #-}

{- |
Helpers for the examples of the book. Not part of clash-circuit-context: the
book shows a waveform as the tree of its scopes and signals, and this module
prints that tree from a simulation.
-}
module BookSupport (printWaveform) where

import Prelude

import Control.Exception (evaluate, finally, throwIO)
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.FilePath ((-<.>))
import System.IO (hClose, openTempFile)

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import Clash.CircuitContext (HasCircuitContext, withRecording)
import Clash.CircuitContext.Recording (discardRecording, writeVCD)
import Clash.Prelude (KnownDomain, NFDataX, Signal, deepseqX, sampleN)

{- | Simulate a design for some cycles with recording, and print the scopes
and signals of its VCD: one line for each, indented by depth. The files are
written to the temporary directory, and deleted after.
-}
printWaveform :: (KnownDomain dom, NFDataX a) => Int -> ((HasCircuitContext) => Signal dom a) -> IO ()
printWaveform n design = do
  tmp <- getTemporaryDirectory
  (path, h) <- openTempFile tmp "book.vcd"
  hClose h
  let files = [path, path -<.> "json", path -<.> "spill"]
  flip finally (mapM_ removePathForcibly files) $ do
    (result, rec) <- withRecording (path -<.> "spill") (let xs = sampleN n design in evaluate (deepseqX xs xs))
    _ <- either throwIO pure result
    written <- writeVCD (0, n) rec path
    discardRecording rec
    either fail (const (TextIO.readFile path >>= mapM_ putStrLn . outline)) written

-- | The declarations of a VCD as an indented tree.
outline :: Text.Text -> [String]
outline = go 0 . map Text.words . Text.lines
 where
  go _ [] = []
  go depth (ws : rest) = case ws of
    "$scope" : _ : name : _ -> indent depth name : go (depth + 1) rest
    "$upscope" : _ -> go (depth - 1) rest
    "$var" : _ : _ : _ : name : _ -> indent depth name : go depth rest
    "$enddefinitions" : _ -> []
    _ -> go depth rest
  indent depth name = replicate (2 * depth) ' ' <> Text.unpack name
