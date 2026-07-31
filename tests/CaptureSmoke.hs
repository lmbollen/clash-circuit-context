{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{- | The capture contract: a waveform costs nothing unless it is kept.

Under a parallel test runner peak memory is the sum over concurrently running
tests, so recording work that is thrown away is the dominant cost. These tests
pin the behaviour that makes it avoidable — each one asserts on the FILES,
because that is the observable a developer actually depends on.
-}
module Main where

import qualified Prelude as P

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitFailure)

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.CircuitContext.Waveform

-- | A trivial traced design; the payload type is irrelevant to what we assert.
counter :: (HasCircuitContext) => Signal System (Unsigned 8)
counter = traceSignalC "count" (fromList [0 ..])
{-# OPAQUE counter #-}

check :: String -> Bool -> IO ()
check what ok
  | ok = putStrLn ("ok: " <> what)
  | otherwise = putStrLn ("FAIL: " <> what) >> exitFailure

-- | Do the slot's files exist? (VCD and its ADT sidecar.)
filesFor :: WaveformSlot -> IO (Bool, Bool)
filesFor slot = do
  let vcd = waveformSlotPath slot
  (,) <$> doesFileExist vcd <*> doesFileExist (P.takeWhile (/= '.') vcd <> ".json")

scrub :: WaveformSlot -> IO ()
scrub slot = do
  let vcd = waveformSlotPath slot
      json = P.takeWhile (/= '.') vcd <> ".json"
  P.mapM_ (\p -> doesFileExist p >>= \e -> unless (not e) (removeFile p)) [vcd, json]

main :: IO ()
main = do
  -- A run nobody keeps must not produce a file …
  skipped <- newWaveformSlot "capture-skipped"
  scrub skipped
  _ <- withWaveformWhen False skipped 8 (sampleN 8 counter)
  (v0, j0) <- filesFor skipped
  check "withWaveformWhen False writes nothing" (P.not v0 P.&& P.not j0)

  -- … while one that is kept produces both halves.
  kept <- newWaveformSlot "capture-kept"
  scrub kept
  _ <- withWaveformWhen True kept 8 (sampleN 8 counter)
  writeWaveformSlot kept
  (v1, j1) <- filesFor kept
  check "withWaveformWhen True writes the VCD and its sidecar" (v1 P.&& j1)

  -- A PASSING run under on-failure capture writes nothing at all: the
  -- simulation runs without a recording context, so there is no history to
  -- render and nothing to discard.
  passing <- newWaveformSlot "capture-passing"
  scrub passing
  _ <- withWaveformOnFailure passing 8 (sampleN 8 counter) evaluate
  (v2, j2) <- filesFor passing
  check "withWaveformOnFailure writes nothing when the test passes" (P.not v2 P.&& P.not j2)

  -- A FAILING run writes both, and the original exception still propagates.
  failing <- newWaveformSlot "capture-failing"
  scrub failing
  outcome <-
    try @SomeException $
      withWaveformOnFailure failing 8 (sampleN 8 counter) $ \xs -> do
        _ <- evaluate (P.length xs)
        errorX "deliberate failure"
  check "the failure propagates" (P.either (P.const True) (P.const False) outcome)
  (v3, j3) <- filesFor failing
  check "withWaveformOnFailure writes both files when the test fails" (v3 P.&& j3)

  -- The always-recording variant behaves the same way from the outside.
  failing' <- newWaveformSlot "capture-failing-strict"
  scrub failing'
  _ <-
    try @SomeException $
      withWaveformOnFailure' failing' 8 (sampleN 8 counter) $ \xs -> do
        _ <- evaluate (P.length xs)
        errorX "deliberate failure"
  (v4, j4) <- filesFor failing'
  check "withWaveformOnFailure' writes both files when the test fails" (v4 P.&& j4)

  P.mapM_ scrub [kept, failing, failing']
  putStrLn "capture-smoke passed"
