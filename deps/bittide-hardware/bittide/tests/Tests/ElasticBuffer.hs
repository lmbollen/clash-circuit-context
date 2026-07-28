-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.ElasticBuffer where

import Clash.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Bittide.ElasticBuffer
import Tests.Waveform (withWaveform)

import qualified Data.List as L

createDomain vXilinxSystem{vPeriod = hzToPeriod 200e6, vName = "Fast"}
createDomain vXilinxSystem{vPeriod = hzToPeriod 199e6, vName = "Slow"}

{- | Note [elastic buffer waveforms]

Each case routes a single shared DUT instance through 'withWaveform', so
'xilinxElasticBuffer'\'s internals (data count, under\/overflow, fill\/drain
state, CDC command, …) dump to @waveforms/\<case\>.vcd@ — the last run wins.

The dump window is in VCD ticks. For the mixed-clock cases one tick is
@gcd 5025 5000 = 25 ps@ (Slow read \@5025ps, Fast write \@5000ps), so a
window is @cycles * (period \`div\` 25)@ ticks — sized here to span the
whole sampled run so the asserted over-\/underflow transition is visible
(the VCD records only changes, so a wide window stays a small file). The
equal-clock case has one tick per cycle.
-}

{- | When the xilinxElasticBuffer is written to more quickly than it is being read from,
its data count should overflow.
-}
case_xilinxElasticBufferMaxBound :: Assertion
case_xilinxElasticBufferMaxBound = do
  let
    command = fromList $ L.replicate 60 (Just 1) <> L.repeat Nothing
    wData = pure (0 :: Unsigned 8)

  -- overflow is in the Fast (5000ps) write domain: 16192 cycles * 200 ticks.
  (underflows, overflows, _, _, _) <-
    withWaveform "case_xilinxElasticBufferMaxBound" (16192 * 200)
      $ let
          (dataCount, under, over, fifoOut, ack) =
            xilinxElasticBuffer @6 (clockGen @Slow) (clockGen @Fast) command wData
         in
          ( sampleN 2048 under
          , sampleN 16192 over
          , sampleN 2048 dataCount
          , sampleN 2048 fifoOut
          , sampleN 2048 ack
          )

  let
    -- Ignore the first 32 samples to allow the buffer to fill up
    underflowsTail = L.drop 32 underflows
    overflowsTail = L.drop 32 overflows

  assertBool
    ("elastic buffer should not underflow: " <> show underflowsTail)
    (not $ or underflowsTail)
  assertBool ("elastic buffer should overflow: " <> show overflowsTail) (or overflowsTail)

{- | When the xilinxElasticBuffer is read from more quickly than it is being written to,
its data count should underflow.
-}
case_xilinxElasticBufferMinBound :: Assertion
case_xilinxElasticBufferMinBound = do
  let
    command = fromList $ L.replicate 8 (Just 1) <> L.repeat Nothing
    wData = pure (0 :: Unsigned 8)

  -- underflow is in the Slow (5025ps) read domain: 2048 cycles * 201 ticks.
  (underflows, overflows, _, _, _) <-
    withWaveform "case_xilinxElasticBufferMinBound" (2048 * 201)
      $ let
          (dataCount, under, over, fifoOut, ack) =
            xilinxElasticBuffer @6 (clockGen @Fast) (clockGen @Slow) command wData
         in
          ( sampleN 2048 under
          , sampleN 2048 over
          , sampleN 2048 dataCount
          , sampleN 2048 fifoOut
          , sampleN 2048 ack
          )

  let
    -- Ignore the first 32 samples to allow the buffer to fill up
    underflowsTail = L.drop 32 underflows
    overflowsTail = L.drop 32 overflows

  assertBool ("elastic buffer should underflow: " <> show underflowsTail) (or underflowsTail)
  assertBool ("elastic buffer should not overflow: " <> show overflowsTail) (not $ or overflowsTail)

{- | When the xilinxElasticBuffer is written to as quickly to as it is read from, it should
neither overflow nor underflow.
-}
case_xilinxElasticBufferEq :: Assertion
case_xilinxElasticBufferEq = do
  let
    command = fromList $ L.replicate 16 (Just 1) <> L.repeat Nothing
    wData = pure (0 :: Unsigned 8)

  -- Force every output (not just under/over) so all of 'xilinxElasticBuffer'\'s
  -- demanded internals — data count, fill/drain state, CDC command, acks — end
  -- up in the waveform, rather than only the ones the assertions happen to read.
  (underflows, overflows, _, _, _) <-
    withWaveform "case_xilinxElasticBufferEq" 256
      $ let
          (dataCount, under, over, fifoOut, ack) =
            xilinxElasticBuffer @5 (clockGen @Slow) (clockGen @Slow) command wData
         in
          ( sampleN 256 under
          , sampleN 256 over
          , sampleN 256 dataCount
          , sampleN 256 fifoOut
          , sampleN 256 ack
          )

  let
    -- Ignore the first 32 samples to allow the buffer to fill up
    underflowsTail = L.drop 32 underflows
    overflowsTail = L.drop 32 overflows

  assertBool
    ("elastic buffer should not underflow: " <> show underflowsTail)
    (not $ or underflowsTail)
  assertBool ("elastic buffer should not overflow: " <> show overflowsTail) (not $ or overflowsTail)

tests :: TestTree
tests =
  testGroup
    "Tests.ElasticBuffer"
    [ testGroup
        "xilinxElasticBuffer"
        [ testCase "case_xilinxElasticBufferMaxBound" case_xilinxElasticBufferMaxBound
        , testCase "case_xilinxElasticBufferMinBound" case_xilinxElasticBufferMinBound
        , testCase "case_xilinxElasticBufferEq" case_xilinxElasticBufferEq
        ]
    ]
