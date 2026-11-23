{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{- | A recording that is cancelled stops. The recorder catches what packing
a value throws, to show the value as @x@; a cancellation must go through,
or the cancelled simulation runs on, holding everything it sampled. And it
must go through as a cancellation: the design's values that were being
computed when it came give their values when they are forced again.
-}
module Test.Cancel (tests) where

import qualified Prelude as P

import Control.Concurrent (forkFinally, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.MVar (MVar)
import Control.Exception (SomeException, evaluate, try)
import System.Directory (doesFileExist, removePathForcibly)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.CircuitContext.Waveform
import Clash.Shockwaves.LUT (WaveformForLut (..), WaveformLUT)
import Clash.Shockwaves.Waveform (
  Translator (..),
  TranslatorVariant (..),
  Waveform (translator),
 )

-- | Four bits, translated by a generated lookup table.
newtype Nibble = Nibble (Unsigned 4)
  deriving (Generic, Show)
  deriving anyclass (BitPack, NFDataX, WaveformLUT)

deriving via WaveformForLut Nibble instance Waveform Nibble

{- | Two nibbles. The second's translator says whether it has a generated
lookup table only once 'gate' opens. The recorder asks that when it makes
the lookup-table entries of the first value: no sooner, as the first nibble
answers the question for the whole type when the signal registers, and the
sidecar does not show it. So a recording that waits for the gate waits
where the recorder renders a value, outside what @clash-shockwaves@
catches itself.
-}
newtype Gated = Gated (Unsigned 8)
  deriving (Generic, Show)
  deriving anyclass (BitPack, NFDataX)

instance Waveform Gated where
  translator =
    Translator
      8
      TProduct
        { subs = [("hi", translator @Nibble), ("lo", gated)]
        , start = ""
        , sep = " "
        , stop = ""
        , labels = []
        , preci = 11
        , preco = 11
        }
   where
    gated = case translator @Nibble of
      Translator w (TLut name _ ref) -> Translator w (TLut name gate ref)
      t -> t

-- | Put when the recorder waits for 'gate', and put to open it.
reached, opened :: MVar ()
reached = unsafePerformIO newEmptyMVar
opened = unsafePerformIO newEmptyMVar
{-# NOINLINE reached #-}
{-# NOINLINE opened #-}

-- | 'Nothing' (the table is generated), once the gate is open.
gate :: P.Maybe a
gate = unsafePerformIO (putMVar reached () >> takeMVar opened >> pure P.Nothing)
{-# NOINLINE gate #-}

-- | A traced signal of 'Gated'.
gatedSignal :: (HasCircuitContext) => Signal System Gated
gatedSignal = traceSignalSW "gated" (fromList (P.map Gated [0 ..]))
{-# OPAQUE gatedSignal #-}

-- | A traced counter from a run-time value, so that no run shares it.
counter :: (HasCircuitContext) => Int -> Signal System (Unsigned 8)
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
            ( \k ->
                timeout
                  100000
                  ( writeWaveform
                      defaultOptions
                      ("waveforms/cancel-stops-" <> P.show k)
                      (0, 3000000)
                      (counter k)
                  )
            )
            [1 .. 10 :: Int]
        P.length (P.filter P.null runs) @?= 10
        P.mapM doesFileExist [f | k <- [1 .. 10 :: Int], f <- files k]
          >>= (@?= P.replicate 20 False)
    , -- The recorder renders a value's lookup-table entries inside the
      -- design's values. A cancellation there must reach them as one: thrown
      -- again with throwIO, it would be stored in them, and every later use
      -- would throw it.
      testCase "a value whose recording is cancelled gives its value when forced again" $ do
        (r, _) <- withRecording Nothing $ do
          -- Bound, not let: a let-bound IO action is a function, so each run
          -- would sample a new instance of the signal.
          xs <- evaluate (sampleN 3 gatedSignal)
          let force = evaluate (P.sum [P.toInteger u | Gated u <- xs])
          ended <- newEmptyMVar
          run <- forkFinally force (putMVar ended)
          takeMVar reached
          killThread run
          _ <- takeMVar ended
          putMVar opened ()
          try @SomeException force
        case r of
          P.Right (P.Right total) -> total @?= 3
          other -> assertFailure ("forced again: " <> P.show (P.fmap (P.fmap P.id) other))
    ]
 where
  files k = ["waveforms/cancel-stops-" <> P.show k <> ext | ext <- [".fst", ".spill"]]
