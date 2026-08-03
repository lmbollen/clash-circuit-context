{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
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

For a FAILING case you want the counterexample rather than a size heuristic,
and 'withWaveformOnCounterexample' captures exactly that — see below for why
it cannot be 'Clash.CircuitContext.Waveform.withWaveformOnFailure'.
-}
module Clash.CircuitContext.Waveform.Hedgehog (
  -- * Capturing the counterexample
  withWaveformOnCounterexample,
  withWaveformCase,
  withWaveformCaseLazy,

  -- * Choosing a passing case to keep
  recordThisCase,
  recordCaseOfSize,
  recordLargestCase,
  maxSize,
  recheckWithWaveform,
) where

import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef')
import System.Directory (makeAbsolute)

import Hedgehog (PropertyT, Seed, Size (..))
import Hedgehog.Internal.Property (
  Failure,
  PropertyT (PropertyT),
  footnote,
  forAllWithT,
  mkTestT,
  runTestT,
 )

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen

import Clash.Prelude (NFDataX, deepseqX)

import Clash.CircuitContext.Core (
  HasCircuitContext,
  withCircuitContextWindowM,
  withoutCircuitContext,
 )
import Clash.CircuitContext.Waveform (
  WaveformSlot,
  captureRun,
  waveformSlotPath,
  writeWaveformSlot,
 )

{- | Capture the waveform of a property's COUNTEREXAMPLE: the shrunk, minimal
case that hedgehog finally reports.

Every case runs first with recording OFF, so a property that passes costs
nothing at all. When @consume@ fails — @'Hedgehog.==='@, @assert@, @failure@ —
that one case is re-run with recording on, its waveform replaces the slot's,
and the failure is re-raised unchanged so hedgehog still shrinks and still
reports exactly what it would have.

Because shrinking re-runs the property on ever smaller inputs, and each
failing case overwrites the slot, what survives is the waveform of the LAST
failing case — which is the minimal counterexample hedgehog prints. That is
the one worth looking at: the same failure, in as few cycles and as small a
design as hedgehog could reduce it to. Only failing cases pay for a recording,
so the cost is one recording per step down the shrink path.

This cannot be 'Clash.CircuitContext.Waveform.withWaveformOnFailure'. A
hedgehog failure is a value in @PropertyT@'s error layer, not a thrown
exception, so no amount of 'Control.Exception.try' in IO can see it; catching
it means running the property monad, which is what this does.

> prop_roundtrip :: WaveformSlot -> Property
> prop_roundtrip wf = property $ do
>   input <- forAll genInput
>   withWaveformOnCounterexample wf 10_000 (sampleN n (dut input)) $ \out ->
>     out === model input

Two constraints, both from the re-run:

* @consume@ runs twice for a failing case, so it must not contain @forAll@
  (the second run would draw different values) and must not perform one-shot
  IO. Assertions and forcing, which is what a property body is.

* the re-run must reproduce the failure, which holds for a simulation whose
  inputs are all supplied by the test. Where it does not, nothing is written
  and the report says so, rather than offering the waveform of a run that
  worked as if it were the failure's.

The file is written here, and the failure report ends with its absolute path —
'Clash.CircuitContext.Waveform.waveformDir' is relative to the working
directory, which differs between @cabal test@ and @cabal run@, so a relative
path would be a small treasure hunt.
-}
withWaveformOnCounterexample ::
  (NFDataX r) =>
  -- | This test's slot; the counterexample is recorded into it.
  WaveformSlot ->
  -- | Trailing capture window in cycles; see
  -- 'Clash.CircuitContext.Waveform.withWaveformLazy'.
  Int ->
  -- | The simulation, under a recording context.
  (HasCircuitContext => r) ->
  -- | Assertions over it; run twice when they fail.
  (r -> PropertyT IO a) ->
  PropertyT IO a
withWaveformOnCounterexample = withWaveformCase False

{- | 'withWaveformOnCounterexample', plus: keep THIS case's waveform even
though it passed.

One combinator for both reasons a property leaves a waveform, because a case
either passes or fails and so needs simulating only once either way. Pass the
flag from 'recordLargestCase' (itself usually behind
'Clash.CircuitContext.Waveform.waveformsRequested') and a green run captures
the biggest case while a red one captures the counterexample — which
overwrites it, since a failure is the more interesting of the two.

'True' records as the case runs, so it needs no re-run and no reproducibility;
'False' is exactly 'withWaveformOnCounterexample' and costs nothing while the
property passes.
-}
withWaveformCase ::
  (NFDataX r) =>
  -- | Keep this case's waveform even if it passes?
  Bool ->
  WaveformSlot ->
  Int ->
  (HasCircuitContext => r) ->
  (r -> PropertyT IO a) ->
  PropertyT IO a
withWaveformCase = withWaveformCaseWith forceSim

{- | 'withWaveformCase' for a simulation that must NOT be forced whole: a
firmware test reading a UART stream until it sees its result, where the
consumer's own exit condition is what bounds the run.

The waveform then covers exactly the cycles the consumer forced, which is the
point — but it also means a consumer that stops early records little. See
'withWaveformCase' for what that costs when it is not deliberate.
-}
withWaveformCaseLazy ::
  Bool ->
  WaveformSlot ->
  Int ->
  (HasCircuitContext => r) ->
  (r -> PropertyT IO a) ->
  PropertyT IO a
withWaveformCaseLazy = withWaveformCaseWith pure

{- | Force a simulation result to normal form so that every traced signal
records its whole run.

Bound to ONE monomorphic thunk before forcing: forcing @sim@ twice would
instantiate its @?circuitContext@ twice and register every signal twice
(spurious @name_0@\/@name_1@ siblings).
-}
forceSim :: (NFDataX r) => r -> IO r
forceSim r = evaluate (r `deepseqX` r)

withWaveformCaseWith ::
  (r -> IO r) ->
  Bool ->
  WaveformSlot ->
  Int ->
  (HasCircuitContext => r) ->
  (r -> PropertyT IO a) ->
  PropertyT IO a
withWaveformCaseWith force keep slot window sim consume
  | keep = do
      (outcome, traces, probes) <-
        withCircuitContextWindowM window $ do
          recorded <- liftIO (force sim)
          attempt (consume recorded)
      wrote <- liftIO $ do
        captured <- captureRun slot traces probes
        when captured (writeWaveformSlot slot)
        if captured
          then Just <$> makeAbsolute (waveformSlotPath slot)
          else pure Nothing
      case outcome of
        Right a -> pure a
        Left failure -> do
          -- A failure's report must say where its waveform went whichever
          -- branch captured it; the re-run branch below already does.
          forM_ wrote $ \path -> footnote ("waveform: " <> path)
          reraise failure
  | otherwise = do
      outcome <- attempt (consume (withoutCircuitContext sim))
      case outcome of
        Right a -> pure a
        Left failure -> do
          -- Second run of THIS case, recording. Instantiating the constrained
          -- @sim@ again gives an independent simulation; the journal of the
          -- re-run is dropped so the report shows each annotation once.
          (again, traces, probes) <-
            withCircuitContextWindowM window $ do
              recorded <- liftIO (force sim)
              attemptQuietly (consume recorded)
          case again of
            Left _ -> do
              wrote <- liftIO $ do
                captured <- captureRun slot traces probes
                when captured (writeWaveformSlot slot)
                if captured
                  then Just <$> makeAbsolute (waveformSlotPath slot)
                  else pure Nothing
              -- Into the JOURNAL rather than onto stderr: hedgehog prints the
              -- journal of the case it finally reports, so the path appears
              -- once, beside the counterexample it belongs to — and not once
              -- per step down the shrink path.
              forM_ wrote $ \path -> footnote ("waveform: " <> path)
            Right _ ->
              footnote
                ( "no waveform: re-running this case did not reproduce the"
                    <> " failure, so "
                    <> waveformSlotPath slot
                    <> " would have shown a working run."
                )
          reraise failure

{- | Run a property action and hand back its failure as a value instead of
letting it short-circuit the rest of the property. The journal (annotations,
footnotes, @forAll@ renderings) is kept, so the report is unchanged.
-}
attempt :: PropertyT IO a -> PropertyT IO (Either Failure a)
attempt (PropertyT p) = PropertyT (mkTestT (fmap keepJournal (runTestT p)))
 where
  keepJournal (r, journal) = (Right r, journal)

-- | 'attempt', discarding the journal: for a re-run whose annotations the
-- report has already shown.
attemptQuietly :: PropertyT IO a -> PropertyT IO (Either Failure a)
attemptQuietly (PropertyT p) = PropertyT (mkTestT (fmap dropJournal (runTestT p)))
 where
  dropJournal (r, _) = (Right r, mempty)

-- | Put back a failure taken out by 'attempt', exactly as it was.
reraise :: Failure -> PropertyT IO a
reraise failure = PropertyT (mkTestT (pure (Left failure, mempty)))

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

{- | 'recordThisCase' at a chosen size: the first case hedgehog runs at least
that big leaves the waveform.

'Size' is hedgehog's one knob on how large a case is — how long a generated
list, how wide a generated word — so it is also the knob on how big the
waveform is. Small sizes give a waveform you can read end to end; large ones
exercise the design properly. Ask for whichever the design question needs:

> keep <- recordCaseOfSize fired 10   -- a few cycles, readable by eye
> keep <- recordCaseOfSize fired 90   -- near the biggest case the run makes
-}
recordCaseOfSize :: IORef Bool -> Size -> PropertyT IO Bool
recordCaseOfSize fired n = recordThisCase fired (>= n)

-- | 'recordCaseOfSize' at the biggest size hedgehog generates: the last, most
-- thorough case of a default-configured property. See 'maxSize'.
recordLargestCase :: IORef Bool -> PropertyT IO Bool
recordLargestCase fired = recordCaseOfSize fired maxSize

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
