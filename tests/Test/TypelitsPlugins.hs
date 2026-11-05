{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

{- | The plugin beside the typelits solver plugins, enabled in the order a
design usually lists them: @ghc-typelits-natnormalise@, then
@ghc-typelits-knownnat@, then this plugin (see the suite's @ghc-options@).

GHC stops after four rounds in which a plugin made progress
(@-fconstraint-solver-iterations@). The oracle used to take two of them for
each binding that the typelits plugins had to rewrite first, and @ptr@ below
then failed to compile with @solveWanteds: too many iterations@. The
compile is the regression test; the run checks that the bindings are traced.
-}
module Main where

import qualified Prelude as P

import Control.Exception (evaluate)
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.CircuitContext.Recording (
  RecordedSignal (signalKey),
  discardRecording,
  recordedChanges,
  recordedSignals,
 )

-- | A pointer one bit wider than its address: the width is @aw + 1@.
ptr ::
  forall aw.
  (HasCircuitContext, KnownNat aw) =>
  Signal System Bool ->
  Signal System (Unsigned (aw + 1))
ptr inc = p
 where
  p = register clockGen resetGen enableGen 0 next
  next = mux inc (p + 1) p
{-# OPAQUE ptr #-}

check :: String -> Bool -> IO ()
check what ok
  | ok = putStrLn ("ok: " <> what)
  | otherwise = putStrLn ("FAIL: " <> what) >> exitFailure

main :: IO ()
main = do
  createDirectoryIfMissing True "waveforms"
  (r, rec) <-
    withRecording
      "waveforms/typelits-plugins.spill"
      (evaluate (P.sum (P.map P.toInteger (sampleN 6 (ptr @2 (pure True))))))
  check "the run finished" (P.either (P.const False) (P.const True) r)
  let
    -- A key is @scope\@ordinal\@callsite.name\@ordinal\@callsite@.
    leaf = P.takeWhile (P./= '@') . P.reverse . P.takeWhile (P./= '.') . P.reverse
    named name = [s | s <- recordedSignals rec, leaf (signalKey s) P.== name]
  check "p is traced" (P.not (P.null (named "p")))
  check "next is traced" (P.not (P.null (named "next")))
  case named "next" of
    s : _ -> do
      changes <- recordedChanges rec s
      -- (cycle, Just (undefined bits, value)) at each change. The reset
      -- holds p at 0 for two cycles.
      check
        ("next counts up: " <> P.show changes)
        (changes P.== [(c, P.Just (0, v)) | (c, v) <- [(0, 1), (2, 2), (3, 3), (4, 4), (5, 5)]])
    [] -> pure ()
  discardRecording rec
