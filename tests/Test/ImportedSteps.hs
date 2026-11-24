{-# LANGUAGE ImplicitParams #-}

{- | Step functions for "Test.ProbedSteps" to use from ANOTHER module: one
with @HasProbe@ in its signature and one without.
-}
module Test.ImportedSteps (importedStep, importedProbedStep) where

import Clash.Prelude

import Clash.CircuitContext (HasProbe)

-- | A step function without @HasProbe@: it probes nothing.
importedStep :: Int -> Int -> (Int, Int)
importedStep s i = (next, s)
 where
  next = s + i

-- | A step function with @HasProbe@: the plugin probes @next@.
importedProbedStep :: (HasProbe) => Int -> Int -> (Int, Int)
importedProbedStep s i = (next, s)
 where
  next = s + i
