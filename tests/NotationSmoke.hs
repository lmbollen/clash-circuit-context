{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin=CircuitNotation #-}
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}

{- | Golden test against the REAL circuit-notation desugarer.

The @circuit@ blocks below are rewritten by circuit-notation's parse-stage
plugin into recursive @let@s of tagged port bindings
(@\<port\>_Fwd@\/@\<port\>_Bwd@, carrying the port's source span), which this
package's renamer then wraps with 'autoTrace' like any local binding. The
'Traceable' instances for the tag newtype, @()@ and tuples make those
bindings trace. What this proves:

* @\<-@ port binders trace under the enclosing component's scope, with the
  values the design actually computes;
* FORWARD REFERENCES survive: the notation closes them through a plain lazy
  let knot, so this test COMPLETING is the strictness proof — a trace that
  forced a port before the design does would @\<\<loop\>\>@;
* a port of composite protocol type traces component-wise (@nm_0@\/@nm_1@),
  with unit halves recording nothing;
* a port with a non-'BitPack' payload falls back to identity (still
  compiles, simply absent);
* a @_@-prefixed port opts out;
* a binder the notation INVENTS (@final:stmt@, from a non-@idC@ final
  statement) is skipped by its colon-marked name — the ports the user named
  around it still trace;
* the circuit's lambda-bound INTERFACE ports do not trace (yet — that is the
  upstream @trace-ports@ patch; their absence is asserted so the golden
  flips when it lands).

circuit-notation's own runtime ("Circuit": @BusTag@\/@TagCircuit@\/…) is used
via the plugin's default names. @BusTag@ mirrors the @Data.Tagged@ wrapper
clash-protocols maps the plugin to; its 'Traceable' instance below is the
one-line user-facing extension-point story from the 'Traceable' haddock.
-}
module Main where

import qualified Prelude as P

import Control.Exception (evaluate)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)

import Clash.Explicit.Prelude

-- Unqualified wholesale: the generated code references the runtime's
-- 'TagCircuit'/'BusTagBundle'/'unitBwd'/… by unqualified name.
import Circuit
import Clash.CircuitContext

-- The 'Traceable (BusTag b t)' orphan lives in "NotationSmokeTP" (the
-- trace-ports half of this suite); importing it brings the instance here.
import NotationSmokeTP (tpTop)

-- | Payload without 'BitPack': ports carrying it must fall back to identity.
data NoPack = MkNoPack
  deriving (Generic, NFDataX, Show)

--------------------------------------------------------------------------------
-- Leaf circuits (hand-written, plain 'Circuit' values)
--------------------------------------------------------------------------------

-- | Combinational adder over two Signal buses.
addC :: Circuit (Signal System Int, Signal System Int) (Signal System Int)
addC = Circuit $ \((a, b) :-> _) -> ((), ()) :-> (a + b)

-- | One-cycle delay: the state element that makes a feedback knot legal.
delayC :: Circuit (Signal System Int) (Signal System Int)
delayC = Circuit $ \(a :-> _) -> () :-> register clockGen resetGen enableGen 0 a

-- | One input, composite output — its single-name port has a tuple 'Fwd'.
splitC :: Circuit (Signal System Int) (Signal System Int, Signal System Int)
splitC = Circuit $ \(a :-> _) -> () :-> (a, a + 1)

-- | Erase to a non-'BitPack' payload.
toNoPackC :: Circuit (Signal System Int) (Signal System NoPack)
toNoPackC = Circuit $ \(a :-> _) -> () :-> fmap (const MkNoPack) a

-- | And back to a constant.
fromNoPackC :: Circuit (Signal System NoPack) (Signal System Int)
fromNoPackC = Circuit $ \(a :-> _) -> () :-> fmap (const 7) a

--------------------------------------------------------------------------------
-- The instrumented designs
--------------------------------------------------------------------------------

{- | Feedback through a FORWARD REFERENCE: @dly@ is consumed by the first
statement and bound by the second. The notation closes this through its lazy
let knot; a strict port trace deadlocks here.
-}
knotTop :: (HasCircuitContext) => Circuit (Signal System Int) (Signal System Int)
knotTop = circuit $ \i -> do
  acc <- addC -< (i, dly)
  dly <- delayC -< acc
  idC -< acc
{-# OPAQUE knotTop #-}

{- | Port shapes: a composite-typed port (@ab@), a tuple PATTERN port
(@(x, y)@ — each leaf its own binder), a non-'BitPack' port (@np@), and a
@_@-prefixed port (@_dbg@ — a per-occurrence hole in the notation; its
generated binder keeps the underscore, so the renamer's opt-out gate skips
it).
-}
portsTop :: (HasCircuitContext) => Circuit (Signal System Int) (Signal System Int)
portsTop = circuit $ \i -> do
  ab <- splitC -< i
  (x, y) <- idC -< ab
  np <- toNoPackC -< x
  z <- fromNoPackC -< np
  _dbg <- delayC -< y
  out <- addC -< (z, y)
  idC -< out
{-# OPAQUE portsTop #-}

{- | A non-@idC@ FINAL STATEMENT: the notation desugars @addC -< (a, i)@ into
@final:stmt <- addC -< (a, i); idC -< final:stmt@ — a binder the designer
never wrote. Its colon-marked name is the contract that keeps it out of the
trace (see the notation's Note [Synthesised binder names] and this package's
'wantedBinder'): only @a@, the port the user DID name, may appear.
-}
finalTop :: (HasCircuitContext) => Circuit (Signal System Int) (Signal System Int)
finalTop = circuit $ \i -> do
  a <- delayC -< i
  addC -< (a, i)
{-# OPAQUE finalTop #-}

--------------------------------------------------------------------------------
-- Harness
--------------------------------------------------------------------------------

-- | Drive a fully applied signal circuit and take its forward output.
runC ::
  Circuit (Signal System Int) (Signal System Int) ->
  Signal System Int ->
  Signal System Int
runC c i = let (_ :-> o) = runCircuit c (i :-> ()) in o

check :: String -> Bool -> IO ()
check what ok
  | ok = putStrLn ("ok: " <> what)
  | otherwise = putStrLn ("FAIL: " <> what) >> exitFailure

main :: IO ()
main = do
  ((kOut, pOut, tOut, fOut), traces, probes) <- withCircuitContext $ do
    let
      ks = sampleN 8 (runC knotTop (fromList [1, 1 ..]))
      ps = sampleN 8 (runC portsTop (fromList [1, 1 ..]))
      ts = sampleN 8 (runC tpTop (fromList [1, 1 ..]))
      fs = sampleN 8 (runC finalTop (fromList [1, 1 ..]))
    _ <- evaluate (deepseqX ks ks)
    _ <- evaluate (deepseqX ps ps)
    _ <- evaluate (deepseqX ts ts)
    _ <- evaluate (deepseqX fs fs)
    pure (ks, ps, ts, fs)
  putStrLn ("knot samples:  " <> show kOut)
  putStrLn ("ports samples: " <> show pOut)
  putStrLn ("tp samples:    " <> show tOut)
  putStrLn ("final samples: " <> show fOut)
  putStrLn ("traces: " <> show (Map.keys traces))
  let
    tks = Map.keys traces
    has nm = P.any (nm `isInfixOf`) tks
  -- knotTop: acc = i + dly, dly = register 0 acc (reset holds cycle 1), i = 1,1,…
  check "knot samples" (kOut P.== [1, 1, 2, 3, 4, 5, 6, 7])
  -- portsTop: z = 7 (const), y = i+1 = 2; out = z+y
  check "ports samples" (pOut P.== [9, 9, 9, 9, 9, 9, 9, 9])
  -- The laziness proof already happened: sampling terminated. Now coverage:
  check "knot: intermediate ports traced" (has "acc_Fwd@" P.&& has "dly_Fwd@")
  check "knot: ports scoped under the component" (has "knotTop@")
  check
    "composite port traces component-wise"
    (has "ab_Fwd_0@" P.&& has "ab_Fwd_1@")
  check
    "tuple-pattern ports trace per leaf"
    (has "x_Fwd@" P.&& has "y_Fwd@")
  check "non-BitPack port fell back to identity" (P.not (has "np_"))
  check "underscore port opted out" (P.not (has "_dbg"))
  check "unit (Bwd) halves record nothing" (P.not (has "_Bwd@"))
  -- Interface ports are lambda-bound by the notation — invisible WITHOUT the
  -- trace-ports flag (this module) and visible WITH it (NotationSmokeTP).
  check
    "interface ports not traced without trace-ports"
    (P.not (P.any ("knotTop" `isInfixOf`) (P.filter ("i_Fwd" `isInfixOf`) tks)))
  -- finalTop: a = register 0 i (reset held 1 cycle) = 0,0,1,1,…; out = a + i
  check "final-statement samples" (fOut P.== [1, 1, 2, 2, 2, 2, 2, 2])
  check
    "synthesised final:stmt binder not traced"
    (P.not (has "final:stmt"))
  check
    "…while the port the user DID name still is"
    (P.any (\k -> "finalTop" `isInfixOf` k P.&& "a_Fwd" `isInfixOf` k) tks)
  check "trace-ports: same knot still terminates" (tOut P.== kOut)
  check
    "trace-ports: interface port traced"
    (P.any (\k -> "tpTop" `isInfixOf` k P.&& "i_Fwd" `isInfixOf` k) tks)
  check
    "trace-ports: intermediate ports still traced"
    (P.any (\k -> "tpTop" `isInfixOf` k P.&& "acc_Fwd" `isInfixOf` k) tks)
  vcd <- dumpVCDC (0, 8) traces probes
  case vcd of
    Left err -> putStrLn ("FAIL: no VCD: " <> err) >> exitFailure
    Right txt -> do
      TIO.writeFile "notation-smoke.vcd" txt
      putStrLn "notation-smoke passed"
