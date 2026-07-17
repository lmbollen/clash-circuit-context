{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

{- | Phase 2: exercises 'autoTrace' MANUALLY (no plugin rewrite involved)
against the real runtime, validating the oracle's decisions:

* concrete traceable signal → traced;
* concrete signal with a non-'BitPack' payload → identity, and compiles;
* polymorphic signal with the full evidence in givens → traced;
* polymorphic signal WITHOUT 'BitPack' evidence → identity, and compiles
  (the no-backtracking property ordinary instances can't provide);
* 'Vec' of signals → traced element-wise with @_i@ names.
-}
module Main where

import qualified Prelude as P

import Control.Exception (evaluate)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import System.Exit (exitFailure)

import Clash.Explicit.Prelude

import Clash.CircuitContext.Auto (autoTrace)
import Clash.CircuitContext.Core (HasCircuitContext, withCircuitContext)

-- | Payload without BitPack: @CanTrace (Signal System NoPack)@ is 'False.
data NoPack = MkNoPack
  deriving (Generic, NFDataX, Show)

polyTraced ::
  (HasCircuitContext, KnownDomain dom, BitPack a, NFDataX a, Typeable a) =>
  Signal dom a ->
  Signal dom a
polyTraced s = autoTrace "polyyes" s

polyNoEvidence ::
  (HasCircuitContext, KnownDomain dom, NFDataX a) =>
  Signal dom a ->
  Signal dom a
polyNoEvidence s = autoTrace "polyno" s

check :: String -> Bool -> IO ()
check what ok
  | ok = putStrLn ("ok: " <> what)
  | otherwise = putStrLn ("FAIL: " <> what) >> exitFailure

main :: IO ()
main = do
  (_, traces, _probes) <- withCircuitContext $ do
    let
      mono =
        autoTrace "mono" (fromList [1, 2, 3, 4] :: Signal System (Unsigned 8))
      blocked =
        autoTrace
          "blocked"
          (fromList [MkNoPack, MkNoPack, MkNoPack] :: Signal System NoPack)
      polyY = polyTraced (fromList [7, 7, 7] :: Signal System (Unsigned 4))
      polyN = polyNoEvidence (fromList [9, 9, 9] :: Signal System (Unsigned 8))
      vec =
        autoTrace
          "vec"
          ( (fromList [True, True, True] :: Signal System Bool)
              :> (fromList [False, False, False] :: Signal System Bool)
              :> Nil
          )
    -- Force one sample of each so lazy trace registration fires.
    _ <- evaluate (P.head (sampleN 2 mono))
    _ <- evaluate (P.head (sampleN 2 blocked))
    _ <- evaluate (P.head (sampleN 2 polyY))
    _ <- evaluate (P.head (sampleN 2 polyN))
    _ <- P.traverse (evaluate . P.head . sampleN 2) (toList vec)
    pure ()
  let
    keys = Map.keys traces
    has nm = P.any (nm `isInfixOf`) keys
  putStrLn ("trace keys: " <> show keys)
  check "mono traced" (has "mono@")
  check "poly-with-evidence traced (given lookup)" (has "polyyes@")
  check "vec traced element-wise" (has "vec_0@" P.&& has "vec_1@")
  check "non-BitPack payload fell back to identity" (P.not (has "blocked"))
  check "poly-without-evidence fell back to identity" (P.not (has "polyno"))
  putStrLn "fallback passed"
