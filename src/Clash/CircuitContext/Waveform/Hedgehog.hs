{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Choosing WHICH case of a hedgehog property leaves a waveform.

A property runs its body once per generated case; a test wants at most one
waveform. Deciding after the fact means recording all of them and throwing
nearly all away — under a parallel runner that is the dominant memory cost.
So the decision is made BEFORE the simulation runs, and the cases that lose
it never record at all ('Clash.CircuitContext.Waveform.withWaveformWhen' runs
them under 'Clash.CircuitContext.withoutCircuitContext').

The decision lives in a generator, which is what lets it see hedgehog's
'Size', and it claims a caller-owned 'IORef' so it fires at most once however
many times the generator is re-run during shrinking.

> prop_roundtrip :: IORef Bool -> Property
> prop_roundtrip fired = property $ do
>   input <- forAll genInput
>   keep  <- recordLargestCase fired
>   out   <- withWaveformWhen keep slot nCycles (…traceSignalC…)
>   out === expected input

For the failing case you want the counterexample, not a size heuristic —
capture that with 'Clash.CircuitContext.Waveform.withWaveformOnFailure', or
replay it exactly with 'recheckWithWaveform'.
-}
module Clash.CircuitContext.Waveform.Hedgehog (
  recordThisCase,
  recordLargestCase,
  maxSize,
  recheckWithWaveform,
) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef')

import Hedgehog (PropertyT, Seed, Size (..))
import Hedgehog.Internal.Property (forAllWithT)

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen

{- | The largest 'Size' hedgehog runs a case at.

The runner increments the size once per test and wraps back to 0 past this,
so a property left at the default 100 tests runs sizes 0..99 and its LAST
case is the biggest — the most thorough one, and the usual choice to keep.

Note a property with @withTests 1@ only ever runs at size 0; such a test
wants @'recordThisCase' fired (const True)@ instead.
-}
maxSize :: Size
maxSize = 99

{- | Decide, before simulating, whether THIS case should leave the waveform,
firing at most once per property.

@want@ sees the current 'Size'. The 'IORef' is the caller's — one per test,
same ownership as the waveform slot itself — and is claimed atomically, so
the first case satisfying @want@ wins and every later one (including the
re-runs shrinking performs) gets 'False'.

The decision is made inside a generator because that is the only place
hedgehog exposes 'Size'. It is rendered as nothing in the report and has no
shrinks, so it neither clutters output nor perturbs the shrink tree.
-}
recordThisCase :: IORef Bool -> (Size -> Bool) -> PropertyT IO Bool
recordThisCase fired want =
  forAllWithT (const "") . Gen.sized $ \size ->
    if want size
      then liftIO (atomicModifyIORef' fired (\f -> (True, not f)))
      else pure False

-- | 'recordThisCase' at the biggest size hedgehog generates: the last, most
-- thorough case of a default-configured property. See 'maxSize'.
recordLargestCase :: IORef Bool -> PropertyT IO Bool
recordLargestCase fired = recordThisCase fired (>= maxSize)

{- | Replay one specific case WITH recording, to get the waveform of a
counterexample hedgehog already found.

A failing property prints the size and seed needed to reproduce it; feeding
those here re-runs exactly that case. Nothing is recorded during ordinary
runs, so this costs nothing until you go looking:

> recheckWithWaveform (Size 27) someSeed (prop_roundtrip fired)

(The property must be built so that this run records — e.g. its flag comes
from @'recordThisCase' fired (const True)@.)
-}
recheckWithWaveform :: Size -> Seed -> Hedgehog.Property -> IO ()
recheckWithWaveform = Hedgehog.recheck
