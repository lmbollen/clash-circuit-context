{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

{- | What a recording keeps alive: a recorder keeps a signal's name, not the
signal. A million cycles recorded are a few MB of records, while the
history of the signal is tens of MB.

The test measures how much the live heap grows over the run, in the whole
process, so "Main" runs it alone, after every other test.
-}
module Test.Memory (memoryTest) where

import qualified Prelude as P

import Control.Exception (evaluate)
import GHC.Stats (gc, gcdetails_live_bytes, getRTSStats)
import System.Mem (performMajorGC)

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.CircuitContext.Recording (discardRecording, recordingEnd)

{- | A name that keeps the signal it names alive for as long as the name is
not evaluated, as a generic field name did: @selName@ takes the field's
value. It must use its argument, or GHC drops it.
-}
lazyName :: Int -> Signal System (Unsigned 8) -> String
lazyName n s = if n P.> 1000 then P.show (P.take 1 (sample s)) else "lazy"
{-# NOINLINE lazyName #-}

-- | A trace named by 'lazyName', of a signal that starts at a run-time value.
lazilyNamed :: (HasCircuitContext) => Int -> Signal System (Unsigned 8)
lazilyNamed n = traceSignalC (lazyName n s) s
 where
  s = fromList (P.map P.fromIntegral [n ..])
{-# OPAQUE lazilyNamed #-}

memoryTest :: TestTree
memoryTest =
  testCase "a recording holds the records, not the signal they came from" $ do
    n <- P.length <$> P.pure "run-time"
    before <- liveBytes
    (r, rec) <-
      withRecording
        "waveforms/memory.spill"
        (evaluate (P.sum (P.map P.toInteger (sampleN 1000000 (lazilyNamed n)))))
    assertBool "the run finished" (P.either (P.const False) (P.const True) r)
    after <- liveBytes
    assertBool
      ("the live heap grew by " <> P.show (after P.- before) <> " bytes")
      (after P.- before P.< 32000000)
    -- The recording is alive until here.
    recordingEnd rec @?= 1000000
    discardRecording rec
 where
  liveBytes :: IO Integer
  liveBytes = performMajorGC >> (P.toInteger . gcdetails_live_bytes . gc <$> getRTSStats)
