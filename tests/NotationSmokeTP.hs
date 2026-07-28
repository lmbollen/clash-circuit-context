{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin=CircuitNotation #-}
{-# OPTIONS_GHC -fplugin-opt=CircuitNotation:trace-ports #-}
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}

{- | The @trace-ports@ half of the notation golden test: compiled with
@-fplugin-opt=CircuitNotation:trace-ports@, so the circuit's lambda-bound
INTERFACE ports are re-bound through source-located let indirections and
trace like the intermediate @\<-@ ports. The sibling module (compiled
without the flag) asserts interface ports are absent; this one asserts they
are present — same design, one flag.

Also hosts the 'Traceable' instance for circuit-notation's own tag newtype
(an orphan here; clash-protocols' @Tagged@ has its instance in the library).
-}
module NotationSmokeTP (tpTop) where

import Clash.Explicit.Prelude

import Circuit
import Clash.CircuitContext

-- | The extension point in action for circuit-notation's own runtime tag.
instance (Traceable t) => Traceable (BusTag b t) where
  traceNamed nm (BusTag t) = BusTag (traceNamed nm t)

-- | Combinational adder over two Signal buses.
addTpC :: Circuit (Signal System Int, Signal System Int) (Signal System Int)
addTpC = Circuit $ \((a, b) :-> _) -> ((), ()) :-> (a + b)

-- | One-cycle delay: the state element that makes a feedback knot legal.
delayTpC :: Circuit (Signal System Int) (Signal System Int)
delayTpC = Circuit $ \(a :-> _) -> () :-> register clockGen resetGen enableGen 0 a

{- | Same knot as the flag-off module's @knotTop@ — with @trace-ports@ the
interface port @i@ (and the master side) trace too, and the indirections
must be exactly as lazy as the lambda binders they alias (this compiling
and running is the proof).
-}
tpTop :: (HasCircuitContext) => Circuit (Signal System Int) (Signal System Int)
tpTop = circuit $ \i -> do
  acc <- addTpC -< (i, dly)
  dly <- delayTpC -< acc
  idC -< acc
{-# OPAQUE tpTop #-}
