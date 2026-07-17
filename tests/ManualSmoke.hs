{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{- | Smoke test for "Clash.Parallel.CircuitContext": hierarchy + scoped trace map +
probes inside a mealy step function, on stock clash-prelude.

Expected: a VCD with nested scopes @top@, @top.acc_0@ and @top.acc_1@ — two
instances of the SAME accumulator under the same parent, disambiguated
downstream by 'dumpVCDC' in call-site (design) order — plus @top.vacc_0@ and
@top.vacc_1@, a vector of instances named by structural position via
'imapComponents'. Each instance contains its traced @sum@ plus @step@, the
probe recorded per cycle from INSIDE the mealy transition function,
sample-aligned with the traced signals; cycles where the probed expression
was never forced show as @x@.
-}
module Main where

import qualified Prelude as P

import Control.Exception (evaluate)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

import Clash.Explicit.Prelude

import Clash.CircuitContext.Core

clk :: Clock System
clk = clockGen

rst :: Reset System
rst = resetGen

ena :: Enable System
ena = enableGen

-- | Accumulator whose step function probes an internal expression: the
-- pre-registration sum, invisible at any Signal.
acc :: HasCircuitContext => Signal System Int -> Signal System Int
acc = mealyProbed clk rst ena step 0
 where
  step :: HasProbe => Int -> Int -> (Int, Int)
  step s i = let s' = probe "step" (s + i) in (s', s)

-- NOTE: the traced expression must be composed INLINE under 'component'. A
-- @where@-bound signal solves @?circuitContext@ from the enclosing scope before
-- 'component' can rebind it (its HasCircuitContext constraint is discharged at the
-- binding, not the use) and the pushed segment is silently lost. If you want
-- a @where@ binding, give it an explicit @HasCircuitContext =>@ signature to keep it
-- polymorphic in the context.
-- Two instances of the same sub-circuit under the same component: both named
-- "acc", both tracing "sum" and probing "step". Neither collides — dumpVCDC
-- renders them as acc_0 and acc_1, ordered by call site (design order).
-- A further two instances are replicated over a Vec and named by structural
-- position (vacc_0, vacc_1) via imapComponents.
top :: HasCircuitContext => Signal System Int -> Signal System Int
top inp =
  component "top"
    ( traceSignalC "out"
        ( component "acc" (traceSignalC "sum" (acc inp))
            + component "acc" (traceSignalC "sum" (acc (inp + 1)))
            + sum
              ( imapComponents @2 "vacc"
                  (\_i s -> traceSignalC "sum" (acc s))
                  (inp :> (inp + 2) :> Nil)
              )
        )
    )

main :: IO ()
main = do
  (samples, traces, probes) <- withCircuitContext $ do
    let out = top (fromList [1, 1 ..])
        xs = sampleN 8 out
    _ <- evaluate (deepseqX xs xs)
    pure xs
  putStrLn ("samples:      " <> show samples)
  putStrLn ("trace names:  " <> show (Map.keys traces))
  putStrLn
    ( "probe map:    "
        <> show
          [ (nm, w, IntMap.toList vs)
          | (nm, (_per, w, vs)) <- Map.toList probes
          ]
    )
  vcd <- dumpVCDC (0, 8) traces probes
  case vcd of
    Left err -> putStrLn ("VCD error: " <> err)
    Right txt -> do
      TIO.writeFile "manual-smoke.vcd" txt
      putStrLn
        ( "VCD lines:    "
            <> show (P.length (Text.lines txt))
            <> " (vars: "
            <> show
              (P.length (P.filter (Text.isInfixOf (Text.pack "$var")) (Text.lines txt)))
            <> ")"
        )
