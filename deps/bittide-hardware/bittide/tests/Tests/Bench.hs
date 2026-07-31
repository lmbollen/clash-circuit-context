-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

-- Instrument this module so the plugin turns 'accInstr' into a component and
-- probes its mealy step.

{- | A/B simulation-performance check for clash-circuit-context, and a
cross-mode correctness check of the "instrumentation is a no-op" claim.

Two definitionally-identical mealy accumulators — 'accPlain' (untouched) and
'accInstr' (a 'component' whose step is probed) — are sampled in three modes:

  1. baseline               — 'accPlain', no instrumentation at all;
  2. instrumented, off      — 'accInstr' under 'noCircuitContext';
  3. instrumented, on       — 'accInstr' under 'withCircuitContext' + VCD dump.

All three must produce identical samples (that is the no-op property), and we
print the wall-clock of each so the overhead of (2) over (1) — which should
be small — and the cost of actually recording (3) are visible.
-}
module Tests.Bench (tests) where

import Clash.Explicit.Prelude
import qualified Prelude as P

import Control.Exception (evaluate)
import System.CPUTime (getCPUTime)
import System.Mem (performMajorGC)
import Text.Printf (printf)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.CircuitContext
import Clash.CircuitContext.Waveform (newWaveformSlot, waveformsRequested, withWaveformWhen, writeWaveformSlot)

type St = (Unsigned 32, Unsigned 32)

-- | The step logic, shared verbatim between the two accumulators.
stepLogic :: St -> Unsigned 32 -> (St, Unsigned 32)
stepLogic (a, b) i = ((a', b'), o)
 where
  a' = a + i
  b' = xor b a'
  o = a' + b'
{-# INLINE stepLogic #-}

-- | Plain reference: an ordinary 'mealy', no clash-circuit-context.
accPlain ::
  Clock XilinxSystem ->
  Reset XilinxSystem ->
  Enable XilinxSystem ->
  Signal XilinxSystem (Unsigned 32) ->
  Signal XilinxSystem (Unsigned 32)
accPlain clk rst ena = mealy clk rst ena stepLogic (0, 0)

-- | Instrumented twin: OPAQUE + 'HasCircuitContext' ⇒ a @component "accInstr"@,
-- and the 'HasProbe' step auto-probes @a'@, @b'@, @o@ each cycle.
accInstr ::
  (HasCircuitContext) =>
  Clock XilinxSystem ->
  Reset XilinxSystem ->
  Enable XilinxSystem ->
  Signal XilinxSystem (Unsigned 32) ->
  Signal XilinxSystem (Unsigned 32)
accInstr clk rst ena inp = out
 where
  out = mealyProbed clk rst ena step (0, 0) inp
  step :: (HasProbe) => St -> Unsigned 32 -> (St, Unsigned 32)
  step (a, b) i = ((a', b'), o)
   where
    a' = a + i
    b' = xor b a'
    o = a' + b'
{-# OPAQUE accInstr #-}

clk :: Clock XilinxSystem
clk = clockGen

rst :: Reset XilinxSystem
rst = resetGen

ena :: Enable XilinxSystem
ena = enableGen

inp :: Signal XilinxSystem (Unsigned 32)
inp = fromList (P.map P.fromIntegral [(0 :: Int) ..])

-- | Sample count for the timing cases (per-cycle cost dominates startup).
nBig :: Int
nBig = 1_000_000

-- | Force @xs@ to normal form and return elapsed CPU seconds, having first
-- built the (shared) input prefix and run a major GC so the timed section
-- starts from a clean heap. Isolating one mode per process invocation (see
-- the shell loop that drives these) and taking the minimum over several runs
-- gives a stable number despite GC jitter.
timeMode :: (NFDataX a) => Int -> a -> IO Double
timeMode n xs = do
  _ <- evaluate (sampleN n inp `deepseqX` ())
  performMajorGC
  t0 <- getCPUTime
  _ <- evaluate (xs `deepseqX` xs)
  t1 <- getCPUTime
  pure (P.fromIntegral (t1 - t0) / 1e12)

-- Each timing case measures exactly ONE mode, so it can be run in its own
-- process (`unittests -p case_baseline`, etc.) free of cross-mode CAF sharing.

case_baseline :: Assertion
case_baseline = do
  t <- timeMode nBig (sampleN nBig (accPlain clk rst ena inp))
  printf "\n  baseline (plain)               n=%d : %.3fs\n" nBig t

case_offband :: Assertion
case_offband = do
  t <- timeMode nBig (let ?circuitContext = noCircuitContext in sampleN nBig (accInstr clk rst ena inp))
  printf "\n  instrumented, NOT recording    n=%d : %.3fs\n" nBig t

case_recording :: Assertion
case_recording = do
  let n = 50_000
  _ <- evaluate (sampleN n inp `deepseqX` ())
  performMajorGC
  t0 <- getCPUTime
  keepwf0 <- waveformsRequested
  wf0 <- newWaveformSlot "bench_recording"
  _ <- withWaveformWhen keepwf0 wf0 n (sampleN n (accInstr clk rst ena inp))
  writeWaveformSlot wf0
  t1 <- getCPUTime
  printf "\n  instrumented, recording+VCD    n=%d : %.3fs\n" n (P.fromIntegral (t1 - t0) / 1e12 :: Double)

-- | The no-op property: all three modes produce identical samples.
case_correctness :: Assertion
case_correctness = do
  let n = 20_000
      xsBase = sampleN n (accPlain clk rst ena inp)
      xsOff = let ?circuitContext = noCircuitContext in sampleN n (accInstr clk rst ena inp)
  keepwf1 <- waveformsRequested
  wf1 <- newWaveformSlot "bench_correctness"
  xsOn <- withWaveformWhen keepwf1 wf1 n (sampleN n (accInstr clk rst ena inp))
  writeWaveformSlot wf1
  assertEqual "instrumented-off must equal baseline" xsBase xsOff
  assertEqual "instrumented-recording must equal baseline" xsBase xsOn

tests :: TestTree
tests =
  testGroup
    "Tests.Bench"
    [ testCase "case_correctness" case_correctness
    , testCase "case_baseline" case_baseline
    , testCase "case_offband" case_offband
    , testCase "case_recording" case_recording
    ]
