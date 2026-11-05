{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

{- | A recording that is cancelled stops. The recorder catches what packing
a value throws, to show the value as @x@; a cancellation must go through,
or the cancelled simulation runs on, holding everything it sampled.
-}
module Test.Cancel (tests) where

import qualified Prelude as P

import System.Directory (doesFileExist, removePathForcibly)
import System.Timeout (timeout)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.CircuitContext.Waveform

-- | A traced counter from a run-time value, so that no run shares it.
counter :: (CircuitContextOnly) => Int -> Signal System (Unsigned 8)
counter k = traceSignalC "count" (fromList (P.map P.fromIntegral [k ..]))
{-# OPAQUE counter #-}

tests :: TestTree
tests =
  testGroup
    "Test.Cancel"
    [ -- Most of a recorded cycle is the recorder packing its value, so a
      -- cancellation lands there most of the time. Each run would take a
      -- second or so; cancelled, it takes 100 ms.
      testCase "a recording that is cancelled stops" $ do
        P.mapM_ removePathForcibly [f | k <- [1 .. 10 :: Int], f <- files k]
        runs <-
          P.mapM
            ( \k -> timeout 100000 (writeWaveform defaultOptions ("cancel-stops-" <> P.show k) (0, 3000000) (counter k))
            )
            [1 .. 10 :: Int]
        P.length (P.filter P.null runs) @?= 10
        P.mapM doesFileExist [f | k <- [1 .. 10 :: Int], f <- files k]
          >>= (@?= P.replicate 20 False)
    ]
 where
  files k = ["waveforms/cancel-stops-" <> P.show k <> ext | ext <- [".fst", ".spill"]]
