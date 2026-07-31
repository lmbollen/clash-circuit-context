-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Wishbone.RegisterWb where

import Clash.Prelude

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Test.Tasty.TH (testGroupGenerator)

import Bittide.Cpus.Riscv32imc (vexRiscv0)
import Bittide.Instances.Common (PeConfigElfSource (NameOnly), peConfigFromElf)
import Bittide.Instances.Tests.RegisterWb (
  DMemWords,
  IMemWords,
  dutWithVcdAndPeConfig,
  getDumpVcd,
  peConfigSim,
  simStream,
 )
import Project.FilePath (CargoBuildType (Release))
import Protocols.MemoryMap (unMemmap)
import Control.Exception (evaluate)
import Clash.CircuitContext.Waveform (newWaveformSlot, withWaveformOnFailure)

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

case_sim :: Assertion
case_sim = do
  dumpVcd <- getDumpVcd
  peConfig <- peConfigSim
  -- SINGLE run: the assertion's own lazy simulation is recorded live; the
  -- waveform (trailing 100k-cycle window) covers what was actually simulated.
  wf0 <- newWaveformSlot "registerwb_sim"
  withWaveformOnFailure wf0 100_000 (simStream dumpVcd peConfig) $ \s -> do
    parsed <- evaluate (parseResultLine s)
    case parsed of
      Left err ->
        assertFailure $ "Parse error: " <> show err
      Right (Just err) ->
        assertFailure $ "Test failed with error: " <> err
      Right Nothing ->
        pure ()

-- | Test the C version of the RegisterWb test
case_c_sim :: Assertion
case_c_sim = do
  dumpVcd <- getDumpVcd
  peConfig <-
    peConfigFromElf
      (SNat @IMemWords)
      (SNat @DMemWords)
      (NameOnly "c_registerwb_test")
      Release
      d0
      d0
      False
      vexRiscv0
  -- SINGLE run (see 'case_sim').
  wf1 <- newWaveformSlot "registerwb_c_sim"
  withWaveformOnFailure wf1 100_000 (simStream dumpVcd peConfig) $ \s -> do
    parsed <- evaluate (parseResultLine s)
    case parsed of
      Left err ->
        assertFailure $ "Parse error: " <> show err
      Right (Just err) ->
        assertFailure $ "Test failed with error: " <> err
      Right Nothing ->
        pure ()

parseResultLine :: String -> Either P.ParseError (Maybe String)
parseResultLine = P.parse resultLineParser ""

resultLineParser :: P.Parser (Maybe String)
resultLineParser = do
  _ <- P.string "RESULT: "
  status <-
    P.choice
      [ P.string "OK" >> return (Nothing :: Maybe String)
      , P.string "PANIC" >> (Just . ("PANIC" <>) <$> restOfLine)
      , P.string "FAIL: " >> (Just <$> restOfLine)
      ]
  return status
 where
  restOfLine = P.manyTill P.anyChar (P.try (P.char '\n'))

tests :: TestTree
tests = $(testGroupGenerator)
