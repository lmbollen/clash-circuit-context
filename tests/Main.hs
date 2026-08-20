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

import Test.Tasty (DependencyType (AllSucceed), defaultMain, sequentialTestGroup, testGroup)

import qualified Example.Hedgehog
import qualified Example.SingleRun
import qualified Test.Capture
import qualified Test.ExampleOutput
import qualified Test.Recorder

main :: IO ()
main = do
  artifacts <- Example.Hedgehog.newArtifacts
  defaultMain $
    testGroup
      "waveform-tests"
      [ Test.Recorder.tests
      , Test.Capture.tests
      , sequentialTestGroup
          "the examples, then what they wrote"
          AllSucceed
          [ testGroup
              "Example"
              [ Example.SingleRun.tests
              , Example.Hedgehog.tests artifacts
              ]
          , Test.ExampleOutput.tests artifacts
          ]
      ]
