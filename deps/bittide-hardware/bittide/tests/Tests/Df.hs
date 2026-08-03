-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.Df where

import Clash.Prelude

import Bittide.Df (asciiDebugMux)
import Bittide.SharedTypes (Byte)
import Clash.Hedgehog.Sized.Vector (genVec)
import Data.Char (chr, ord)
import Data.Maybe (fromMaybe, isJust)
import Protocols (Circuit, Df, (<|))
import Protocols.Experimental.Hedgehog (
  ExpectOptions (eoSampleMax),
  defExpectOptions,
  propWithModelSingleDomain,
 )
import Protocols.Experimental.Simulate (SimulationConfig (..), driveC)
import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.TH (testGroupGenerator)
import Clash.CircuitContext (HasCircuitContext, withoutCircuitContext)
import Clash.CircuitContext.Waveform (newWaveformSlot, waveformsRequested, writeWaveformSlot)
import Protocols.Waveform (withWaveformCWhen)

import qualified Data.List as L
import Hedgehog ((===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

type NSubordinates = 5

{- | Generate a bunch of ASCII lines to write and check that the mux interleaves
them correctly.

XXX: The timeout function is currently only tested at the last line. I.e., if
     the last generated line does not end with a newline, the timeout will
     be triggered and the line should still by present in the output. If the
     test circuit stalls \"too much\" though, this test will fail. Given that
     the test succeeds, this doesn't occur in practice. We could add this, but
     it would make the test more complicated.
-}
prop_asciiDebugMux :: H.Property
prop_asciiDebugMux =
  propWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 512}
    genData
    (\_ _ _ -> model)
    dut
    property
 where
  dut = exposeClockResetEnable (withoutCircuitContext (asciiDebugMux d128 labels))

  model :: Vec NSubordinates [BitVector 8] -> [BitVector 8]
  model (toList . map toLines -> inputLines) =
    toBytes
      $ L.concat
      $ L.zipWith
        (\c -> L.map (addPrefix c))
        labelsAsChars
        inputLines
   where
    addPrefix :: Char -> String -> String
    addPrefix c s = ('[' : c : ']' : ' ' : s) <> "\n"

  genData :: H.Gen (Vec NSubordinates [BitVector 8])
  genData = genVec $ Gen.list (Range.linear 0 50) genChar

  genChar :: H.Gen (BitVector 8)
  genChar =
    Gen.frequency
      [ (80, Gen.element [fromIntegral (ord c) | c <- ['A' .. 'Z']])
      , (20, pure $ fromIntegral $ ord '\n')
      ]

  property :: [BitVector 8] -> [BitVector 8] -> H.PropertyT IO ()
  property expected actual = do
    H.footnote ("expected:\n" <> (unlines $ toLines expected))
    H.footnote ("actual:\n" <> (unlines $ toLines actual))
    L.sort expected === L.sort actual

  -- ASCII labels: '0', '1', '2', ..., 'A', 'B', ...
  labels :: Vec NSubordinates (Vec 1 Byte)
  labels = map (:> Nil) (iterateI (+ 1) (fromIntegral (ord '0')))

  labelsAsChars :: [Char]
  labelsAsChars = L.map (chr . fromIntegral . head) (toList labels)

  toLines :: [Byte] -> [String]
  toLines = lines . L.map (chr . fromIntegral)

  toBytes :: [String] -> [Byte]
  toBytes = L.concatMap (L.map (fromIntegral . ord))

{- | Waveform for a @Df@ (@clash-protocols@) circuit, sampled via @sampleC@.

Same bridge as the AXI4-Stream demo in "Tests.Axi4", on a different protocol:
'Protocols.Waveform.withWaveformC' coerces the fully driven @asciiDebugMux@ to its
forward output signal with @toSignals@ and traces a projection of it. A single
deterministic run in its own test case, so it writes exactly one waveform.
-}
case_asciiDebugMuxWaveform :: Assertion
case_asciiDebugMuxWaveform = do
  keepwf <- waveformsRequested
  wf <- newWaveformSlot "case_asciiDebugMuxWaveform"
  samples <-
    withWaveformCWhen
      keepwf
      wf
      (resetCycles conf)
      nSamples
      "muxOut"
      (fmap (\m -> (fromMaybe 0 m, isJust m)))
      driven
  writeWaveformSlot wf
  let validBytes = [b | (b, valid) <- samples, valid]
  -- The mux forwards every input byte (and adds prefixes), so the number of
  -- valid output bytes is at least the number of input bytes.
  assertBool
    "mux should forward at least every input byte"
    (L.length validBytes >= L.length (L.concat (toList demoMsgs)))
 where
  conf = SimulationConfig 0 nSamples True
  nSamples = 256
  labels :: Vec NSubordinates (Vec 1 Byte)
  labels = map (:> Nil) (iterateI (+ 1) (fromIntegral (ord '0')))
  demoMsgs :: Vec NSubordinates [Maybe Byte]
  demoMsgs =
    msg "Hi\n"
      :> msg "Yo\n"
      :> msg "Hey\n"
      :> msg "OK\n"
      :> msg "Go\n"
      :> Nil
   where
    msg = L.map (Just . fromIntegral . ord)
  -- Keep the constraint so withWaveformC's context reaches asciiDebugMux
  -- (rich waveform: the mux's internals auto-trace).
  driven :: (HasCircuitContext) => Circuit () (Df System Byte)
  driven =
    withClockResetEnable @System clockGen resetGen enableGen $
      asciiDebugMux d128 labels <| driveC conf demoMsgs

tests :: TestTree
tests = $(testGroupGenerator)

-- Run with:
--
--    ghcid -c cabal repl bittide:unittests -T Tests.Df.main
--
-- Add -W if you want to run tests in spite of warnings
--
main :: IO ()
main = defaultMain tests
