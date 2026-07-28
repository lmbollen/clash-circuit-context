-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Wishbone.Time where

-- Preludes
import Clash.Prelude

-- Local
import Bittide.Instances.Domains (Basic50)
import Bittide.Instances.Tests.TimeWb (
  dMemWords,
  dMemWordsC,
  dutNoMm,
  iMemWords,
  iMemWordsC,
  peConfigSim,
 )
import Bittide.ProcessingElement

-- Other
import Control.Monad (forM_, when)
import Data.Char
import Data.List (isInfixOf)
import Data.Maybe
import Protocols.Experimental.Simulate (SimulationConfig (..), sampleC)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Clash.CircuitContext (HasCircuitContext, withoutCircuitContext)
import Control.Exception (evaluate)
import Tests.Waveform (withWaveformLive)
import Text.Parsec
import Text.Parsec.String

sim :: IO ()
sim = do
  peConfig <- peConfigSim iMemWords dMemWords "time_self_test"
  putStrLn $ simResult peConfig

simResult :: PeConfig 4 -> String
simResult peConfig = withoutCircuitContext (simStream peConfig)

-- | Like 'simResult', but under the caller's circuit context.
simStream :: (HasCircuitContext) => PeConfig 4 -> String
simStream peConfig = chr . fromIntegral <$> catMaybes uartStream
 where
  uartStream =
    sampleC def
      $ withClockResetEnable @Basic50 clockGen resetGen enableGen
      $ dutNoMm peConfig

{- | Run the timing module self test with processingElement and inspect it's uart output.
The test returns names of tests and a boolean indicating if the test passed.
-}
case_time_rust_self_test :: Assertion
case_time_rust_self_test = do
  peConfig <- peConfigSim iMemWords dMemWords "time_self_test"
  -- SINGLE run: the assertion's own lazy simulation is recorded live; the
  -- waveform (trailing 100k-cycle window) covers what was actually simulated.
  (parsed, simOutput) <-
    withWaveformLive "time_self_test" 100_000 (simStream peConfig) $
      \simOutput -> do
        parsed <- evaluate (parseTestResults simOutput)
        pure (parsed, simOutput)
  -- Run the test with HUnit
  case parsed of
    Left err -> assertFailure $ show err <> "\n" <> simOutput
    Right results -> do
      forM_ results $ \result -> assertResult result
 where
  assertResult (TestResult name (Just err)) = assertFailure ("Test " <> name <> " failed with error" <> err)
  assertResult (TestResult _ Nothing) = return ()

type IMemWords = DivRU (8 * 1024) 4
type DMemWords = DivRU (4 * 1024) 4

data TestResult = TestResult String (Maybe String) deriving (Show)

type Ascii = BitVector 8
asciiToChar :: Ascii -> Char
asciiToChar = chr . fromIntegral

testResultParser :: Parser TestResult
testResultParser = do
  testName <- manyTill anyChar (try (string ": "))
  result <-
    choice
      [ string "None" >> return Nothing
      , Just <$> (string "Some(" *> manyTill anyChar (char ')'))
      ]
  _ <- endOfLine
  return $ TestResult testName result

testResultsParser :: Parser [TestResult]
testResultsParser = do
  _ <- string "Start time self test" >> endOfLine
  manyTill testResultParser done
 where
  done = try (string "Done") >> endOfLine >> return ()

parseTestResults :: String -> Either ParseError [TestResult]
parseTestResults = parse testResultsParser ""

{- | Run the timing module test with C HAL and inspect its uart output.
This test validates that the C HAL for the timer peripheral works correctly.
-}
case_time_c_test :: Assertion
case_time_c_test = do
  peConfig <- peConfigSim iMemWordsC dMemWordsC "c_timer_wb"
  let
    uartStreamC :: (HasCircuitContext) => [Maybe (BitVector 8)]
    uartStreamC =
      sampleC def{timeoutAfter = 300_000}
        $ withClockResetEnable @Basic50 clockGen (resetGenN d2) enableGen
        $ dutNoMm peConfig
    simResultLazy :: (HasCircuitContext) => String
    simResultLazy = chr . fromIntegral <$> catMaybes uartStreamC
  -- SINGLE run: the assertion's own lazy simulation is recorded live.
  (passedAll, completed, failed, simResultC) <-
    withWaveformLive "time_c_test" 100_000 simResultLazy $ \s -> do
      passedAll <- evaluate ("=== All tests PASSED! ===" `isInfixOf` s)
      completed <- evaluate ("C Timer HAL test completed successfully!" `isInfixOf` s)
      failed <- evaluate ("*** TEST FAILED:" `isInfixOf` s)
      pure (passedAll, completed, failed, s)
  when (not passedAll)
    $ assertFailure
    $ "C timer test did not report all tests PASSED\n"
    <> simResultC
  when (not completed)
    $ assertFailure
    $ "C timer test did not complete successfully\n"
    <> simResultC
  when failed
    $ assertFailure
    $ "C timer test reported a failure\n"
    <> simResultC

tests :: TestTree
tests = $(testGroupGenerator)
