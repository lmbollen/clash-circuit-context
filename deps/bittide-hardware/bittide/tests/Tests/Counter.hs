-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Counter where

import Clash.Explicit.Prelude
import qualified Prelude as P

import Clash.Cores.Xilinx (Xilinx)
import Control.Monad (forM_)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Bittide.Counter (domainDiffCounter)

import Clash.CircuitContext (traceSignalC, withoutCircuitContext)
import Clash.CircuitContext.Waveform (newWaveformSlot, waveformsRequested, withWaveformWhen, writeWaveformSlot)

import qualified Clash.Class.Cdc as Cdc

createDomain vXilinxSystem{vName = "D10", vPeriod = hzToPeriod 100e6}
createDomain vXilinxSystem{vName = "D17", vPeriod = hzToPeriod 170e6}
createDomain vXilinxSystem{vName = "D20", vPeriod = hzToPeriod 200e6}

noRst :: (KnownDomain dom) => Reset dom
noRst = unsafeFromActiveHigh (pure False)

rst :: (KnownDomain dom) => Reset dom
rst = unsafeFromActiveHigh (pure True)

rstN :: (KnownDomain dom) => Int -> Reset dom
rstN n = unsafeFromActiveHigh (fromList (P.replicate n True <> P.repeat False))

top ::
  forall src dst.
  ( KnownDomain src
  , KnownDomain dst
  , Cdc.ValidGray Xilinx 8 src dst
  ) =>
  Reset src ->
  Reset dst ->
  Signal dst (Signed 32)
top rstSrc rstDst =
  Cdc.withVendorI @Xilinx
    $ fst
    <$> withoutCircuitContext ((domainDiffCounter @_ @_ @Xilinx) clockGen rstSrc clockGen rstDst)

-- | 'domainDiffCounter' should continuously emit zeros when applied to the same domain
case_zeroSameDomain :: Assertion
case_zeroSameDomain = do
  keepwf0 <- waveformsRequested
  wf0 <- newWaveformSlot "case_zeroSameDomain"
  sampled <-
    withWaveformWhen keepwf0 wf0 1000
      $ sampleN 1000 (traceSignalC "diff" (top @D10 @D10 noRst noRst))
  writeWaveformSlot wf0
  sampled @?= P.replicate 1000 0

-- | 'domainDiffCounter' should continuously emit zeros when src reset is kept asserted
case_zeroSrcRst :: Assertion
case_zeroSrcRst = do
  keepwf1 <- waveformsRequested
  wf1 <- newWaveformSlot "case_zeroSrcRst"
  sampled <-
    withWaveformWhen keepwf1 wf1 1000
      $ sampleN 1000 (traceSignalC "diff" (top @D10 @D17 rst noRst))
  writeWaveformSlot wf1
  sampled @?= P.replicate 1000 0

-- | 'domainDiffCounter' should continuously emit zeros when dst reset is kept asserted
case_zeroDstRst :: Assertion
case_zeroDstRst = do
  keepwf2 <- waveformsRequested
  wf2 <- newWaveformSlot "case_zeroDstRst"
  sampled <-
    withWaveformWhen keepwf2 wf2 1000
      $ sampleN 1000 (traceSignalC "diff" (top @D10 @D17 noRst rst))
  writeWaveformSlot wf2
  sampled @?= P.replicate 1000 0

-- | No matter when we release the destination reset, we should zeros followed by counting
case_glitchless :: Assertion
case_glitchless =
  --
  forM_ [0 .. 512] $ \n -> do
    let
      sampled = sampleN 1000 (dut (rstN n))
      sampledNonZero = P.dropWhile (== 0) sampled
      len = P.length sampledNonZero
    assertBool (">1 @ " <> show n) (len > 1)
    assertEqual ("exp @ " <> show n) sampledNonZero (P.take len expected)
 where
  dut = top @D10 @D20 noRst
  expected = P.concat [[n, n] | n <- [1 ..]]

tests :: TestTree
tests = $(testGroupGenerator)

-- Run with:
--
--    ghcid -c cabal repl bittide:unittests -T Tests.Counter.main
--
-- Add -W if you want to run tests in spite of warnings
--
main :: IO ()
main = defaultMain tests
