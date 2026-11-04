{- | The waveform suite, in two levels.

* "Example.SingleRun" and "Example.Hedgehog" are USAGE — capturing one run's
  waveform, and instrumenting hedgehog properties — written the way a
  downstream suite writes them and kept honest by running here.

* The Test level pins features: "Test.Recorder" (recorder behaviour under
  generated stimuli), "Test.Capture" (the capture-cost contract), and
  "Test.ExampleOutput", which decodes the files the Example level wrote —
  which is why the Example group and it are SEQUENCED while everything else
  is free to run in parallel.
-}
module Main where

import Test.Tasty (
  DependencyType (AllFinish, AllSucceed),
  defaultMain,
  dependentTestGroup,
  testGroup,
 )

import qualified Example.Hedgehog
import qualified Example.SingleRun
import qualified Test.Capture
import qualified Test.ExampleOutput
import qualified Test.Memory
import qualified Test.Recorder

main :: IO ()
main = do
  Example.Hedgehog.scrubExamples
  defaultMain $
    -- The memory test measures the whole process, so it runs alone, last.
    dependentTestGroup
      "waveform-tests"
      AllFinish
      [ testGroup
          "the tests"
          [ Test.Recorder.tests
          , Test.Capture.tests
          , dependentTestGroup
              "the examples, then what they wrote"
              AllSucceed
              [ testGroup
                  "Example"
                  [ Example.SingleRun.tests
                  , Example.Hedgehog.tests
                  ]
              , Test.ExampleOutput.tests
              ]
          ]
      , Test.Memory.memoryTest
      ]
