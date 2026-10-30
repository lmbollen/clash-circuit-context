{-# LANGUAGE OverloadedStrings #-}

{- |
Helpers for the examples of the book. Not part of clash-circuit-context: the
book shows a waveform as the tree of its scopes and signals, and this module
prints that tree from a simulation.
-}
module BookSupport (printWaveform, printWaveformWith) where

import Prelude

import qualified Data.Text as Text

import Clash.CircuitContext (HasCircuitContext, dumpVCDC, withCircuitContext)
import Clash.Prelude (KnownDomain, NFDataX, Signal, deepseqX, sampleN)

{- | Simulate a design for some cycles under 'withCircuitContext', and print
the scopes and signals of its VCD: one line for each, indented by depth.
-}
printWaveform :: (KnownDomain dom, NFDataX a) => Int -> ((HasCircuitContext) => Signal dom a) -> IO ()
printWaveform n design = printWaveformWith n (sampleN n design)

-- | The same, for a simulation you write yourself.
printWaveformWith :: (NFDataX a) => Int -> ((HasCircuitContext) => [a]) -> IO ()
printWaveformWith n run = do
  (_, traces, probes) <- withCircuitContext (let xs = run in deepseqX xs (pure xs))
  dumpVCDC (0, n) traces probes >>= either fail (mapM_ putStrLn . outline)

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
