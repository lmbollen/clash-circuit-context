{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
-- One pragma: the plugins run in the order given, and a formatter sorts
-- separate pragmas.
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise
  -fplugin=GHC.TypeLits.KnownNat.Solver
  -fplugin=Clash.CircuitContext.Plugin #-}

{- | The plugin beside the typelits solver plugins, enabled in the order a
design usually lists them: @ghc-typelits-natnormalise@, then
@ghc-typelits-knownnat@, then this plugin. "Test.ExtraSolverFirst" and
"Test.DeclinedWidths" enable @ghc-typelits-extra@'s solver first.

GHC stops after four rounds in which a plugin made progress
(@-fconstraint-solver-iterations@). The oracle used to take two of them for
each binding that the typelits plugins had to rewrite first, and @ptr@ below
then failed to compile with @solveWanteds: too many iterations@. Without
this plugin every module of the suite compiles in two rounds, and the suite
is held to that (see its @ghc-options@): the compile is the regression test.
The run checks that the bindings are traced.

"Test.RewriterShapes" holds the shapes where the oracle must not decide
early, and @check.sh@ checks its warnings. The run checks the one of them
that records a signal: @stepped@'s @next@.
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
  signalDescribed,
 )

import qualified Test.DeclinedWidths as DeclinedWidths
import Test.ExtraSolverFirst (ptrExtra)
import Test.RewriterShapes (stepped)
import qualified Test.SideConditions as SideConditions

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

  -- A local function that GHC generalises: its binding is recorded at the
  -- call, with the type there.
  (r2, rec2) <-
    withRecording
      "waveforms/typelits-plugins-stepped.spill"
      (evaluate (P.sum (P.map P.toInteger (sampleN 6 (stepped (pure True))))))
  check "the stepped run finished" (P.either (P.const False) (P.const True) r2)
  check
    "a generalised local function's binding is traced"
    (P.any ((P.== "next") . leaf . signalKey) (recordedSignals rec2))
  discardRecording rec2

  -- A width known only under a side condition is recorded where the
  -- signature gives the side condition, and only there.
  (r3, rec3) <-
    withRecording
      "waveforms/typelits-plugins-side-conditions.spill"
      ( evaluate
          ( P.length (sampleN 2 (SideConditions.unchecked (SNat @4)))
              P.+ P.length (sampleN 2 (SideConditions.checked (SNat @4)))
              P.+ P.length (sampleN 2 (SideConditions.logarithm (SNat @4)))
          )
      )
  check "the side-condition run finished" (P.either (P.const False) (P.const True) r3)
  let
    scopes = [P.takeWhile (P./= '@') (signalKey s) | s <- recordedSignals rec3]
  putStrLn ("side-condition signals: " <> P.show (P.map signalKey (recordedSignals rec3)))
  check "addr is recorded under 1 <= n" ("checked" `P.elem` scopes)
  check "addr is not recorded without 1 <= n" ("unchecked" `P.notElem` scopes)
  check "a Log2 width is not recorded" ("logarithm" `P.notElem` scopes)
  discardRecording rec3

  -- The same pointer, with ghc-typelits-extra's solver first; and the
  -- payloads that the oracle declines in its solver stage.
  (r4, rec4) <-
    withRecording
      "waveforms/typelits-plugins-extra-first.spill"
      ( evaluate
          ( P.sum (P.map P.toInteger (sampleN 6 (ptrExtra @2 (pure True))))
              P.+ P.toInteger (P.length (sampleN 2 (DeclinedWidths.foos @3 (pure 0))))
              P.+ P.sum (P.map P.toInteger (sampleN 2 (DeclinedWidths.bars @3 (pure 0))))
          )
      )
  check "the extra-first run finished" (P.either (P.const False) (P.const True) r4)
  let
    leaves4 = [(leaf (signalKey s), s) | s <- recordedSignals rec4]
  check "next is traced with the extra solver first" ("next" `P.elem` P.map P.fst leaves4)
  check
    "a payload without Waveform is recorded, as raw bits"
    (P.map (signalDescribed . P.snd) (P.filter ((P.== "foo") . P.fst) leaves4) P.== [P.False])
  check "a payload without BitPack is not recorded" ("bar" `P.notElem` P.map P.fst leaves4)
  discardRecording rec4
 where
  -- A key is @scope\@ordinal\@callsite.name\@ordinal\@callsite@.
  leaf = P.takeWhile (P./= '@') . P.reverse . P.takeWhile (P./= '.') . P.reverse
