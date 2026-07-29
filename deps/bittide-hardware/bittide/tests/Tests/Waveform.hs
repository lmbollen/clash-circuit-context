-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Test-suite helper for emitting a hierarchical VCD waveform from a Clash
simulation, built on @clash-circuit-context@.

There are two entry points, depending on how the design under test is sampled:

* 'withWaveform' — for designs sampled directly with @sampleN@/@simulateN@.
  Wrap the signals you care about in 'traceSignalC' and hand the (forced)
  simulation result to 'withWaveform'.

* 'withWaveformC' — for @clash-protocols@ circuits sampled with @sampleC@.
  A t'Circuit' is not a 'Signal', so there is nothing to hand to 'traceSignalC'
  directly; 'withWaveformC' coerces the (fully driven) circuit to its forward
  signals with 'toSignals', traces a projection of them, and dumps. 'driveFwd'
  is the underlying coercion if you need the signals for something else too.

Both write to @waveforms/\<name\>.vcd@ (relative to the current directory).

One waveform per test, holding the LAST run
===========================================

A hedgehog property runs its body many times (once per generated case), but a
test should leave behind exactly ONE waveform: that of its LAST run. Each call
records its run as the pending waveform for @name@ (overwriting the previous
one), and a single 'flushWaveforms' at the end of the suite renders and writes
each file once. Deferring the render keeps a 1000-case property from
rendering\/writing a VCD every case — only the final, surviving run is ever
turned into a file.

"Last run" is the useful run in both outcomes, because of how hedgehog drives
the size parameter:

* On SUCCESS it grows the size from 0 to its maximum across the run, so the
  last case is (one of) the largest — the most thorough waveform.
* On FAILURE it shrinks and re-runs, so the last run is the final, minimal
  counterexample — the one you want to look at.

Distinct tests must use distinct names (each name is one file). The suite wires
'flushWaveforms' in via 'Control.Exception.finally' (see "UnitTests"), so it
fires even on the exit exception @defaultMain@ throws.
-}
module Tests.Waveform (
  withWaveform,
  withWaveformC,
  driveFwd,
  flushWaveforms,
  waveformDir,
) where

import Prelude

import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Proxy (Proxy (..))
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath (takeFileName, (-<.>), (<.>), (</>))
import System.IO (hClose, openTempFile)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

import Clash.Prelude (BitPack, KnownDomain, NFDataX, Signal, deepseqX, sampleN)

import Clash.CircuitContext (
  HasCircuitContext,
  dumpVCDC,
  traceSignalC,
  withCircuitContext,
 )

-- Every traced signal now carries its payload type's ADT description, so a
-- helper polymorphic in that payload must say so.
import qualified Data.Aeson as Json

import Clash.CircuitContext.Shockwaves (dumpVCDSW)
import Clash.Shockwaves.Waveform (Waveform)

import Protocols (Bwd, Circuit, Fwd, toSignals)
import Protocols.Experimental.Simulate (Backpressure (boolsToBwd))

-- | Directory (relative to the working directory) the VCDs are written to.
waveformDir :: FilePath
waveformDir = "waveforms"

-- | The @.vcd@ path for a waveform base name.
waveformPath :: String -> FilePath
waveformPath name = waveformDir </> name <.> "vcd"

{- | The pending waveform for each output path: a DEFERRED VCD render of that
name's last data-producing run. Overwritten on every run, rendered\/written
once by 'flushWaveforms'. A process-global 'IORef' (the suite is one process);
each fresh suite run starts empty.
-}
pendingWaveforms :: IORef (Map.Map FilePath (Text.Text, Json.Value))
pendingWaveforms = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE pendingWaveforms #-}

{- | Simulate under a fresh circuit context, force the result so every
instrumented signal registers, and write ONE hierarchical VCD to
@waveformDir\/\<name\>.vcd@.

The simulation is given as a value @'HasCircuitContext' => r@ (typically the
list @sampleN@\/@simulateN@ returns): the implicit context is supplied here, and
@r@ is forced to normal form inside the context so every @traceSignalC@\/probe
fires. The forced @r@ is returned unchanged so the caller can keep asserting on
it.

Safe to call from inside a hedgehog property: only the first recording run for
@name@ writes a file (see the module header), and subsequent runs are simulated
without a recording context so they cost nothing extra.
-}
withWaveform ::
  forall m r.
  (MonadIO m, NFDataX r) =>
  -- | Base name for the @.vcd@ file (e.g. the property name). Must be unique
  -- across tests.
  String ->
  -- | Dump window, starting at 0. For single-clock traces this is in cycles;
  -- when traces span multiple clock domains the unit is one VCD tick.
  Int ->
  -- | The simulation result to trace and return.
  (HasCircuitContext => r) ->
  m r
withWaveform name nSamples sim = liftIO (recordAndDump name (0, nSamples) sim)

{- | Emit a waveform from a @clash-protocols@ circuit that would normally be run
with @sampleC@.

A t'Circuit' cannot be handed to 'traceSignalC' directly — it is a function over
'Fwd'\/'Bwd' signals, not a 'Signal'. 'withWaveformC' bridges that gap exactly
as the module header describes: it coerces the (fully driven) circuit to its
forward output signal with 'toSignals' (driving the far end "always ready" after
the reset window — see 'driveFwd'), applies your @project@ to pick a
'BitPack'able view to trace, samples it (which is what makes the trace register),
and dumps one VCD.

Typical use — drive the circuit with 'Protocols.Experimental.Simulate.driveC'
and pass the whole @driveC conf inp |> dut@ as @circuit@:

> withWaveformC "myFifo" (resetCycles conf) 256 "out" (fmap summarize)
>   (dut <| driveC conf inp)

The projected samples are returned, so a simple sanity assertion is possible
without simulating twice.
-}
withWaveformC ::
  forall b dom x m.
  (MonadIO m, Backpressure b, KnownDomain dom, BitPack x, NFDataX x, Waveform x) =>
  -- | Base name for the @.vcd@ file. Must be unique across tests.
  String ->
  -- | Reset cycles: the far end is held "not ready" this many cycles before
  -- going "always ready" (matches @'Protocols.Experimental.Simulate.resetCycles'@).
  Int ->
  -- | Number of cycles to sample and dump.
  Int ->
  -- | Name of the traced wire in the VCD.
  String ->
  -- | Projection of the circuit's forward output to a 'BitPack'able signal.
  (Fwd b -> Signal dom x) ->
  -- | The fully driven circuit (left side @()@), e.g. @dut <| driveC conf inp@.
  (HasCircuitContext => Circuit () b) ->
  m [x]
withWaveformC name resetCycles nSamples sigName project mkCircuit =
  liftIO $
    recordAndDump name (0, nSamples) $
      sampleN nSamples (traceSignalC sigName (project (driveFwd resetCycles mkCircuit)))

{- | Coerce a fully driven @clash-protocols@ circuit (left side @()@) to its
forward output signal, driving the far end "not ready" for @resetCycles@ cycles
and "ready" forever after. This is the @sampleC@-style coercion the waveform
dumper runs the circuit through, exposed on its own for callers that also want
the forward signal for assertions.
-}
driveFwd ::
  forall b.
  (Backpressure b) =>
  -- | Reset cycles (far end held not-ready).
  Int ->
  Circuit () b ->
  Fwd b
driveFwd resetCycles circuit = fwd
 where
  (_, fwd) = toSignals circuit ((), readys)
  readys :: Bwd b
  readys = boolsToBwd (Proxy @b) (replicate resetCycles False <> repeat True)

{- | Shared core: force @sim@ under a fresh recording context (so every
@traceSignalC@\/probe fires), record this run as @name@'s pending waveform
(overwriting the previous — LAST run wins), and return the forced result. The
VCD render is deferred to 'flushWaveforms'; nothing is written here.
-}
recordAndDump ::
  forall r.
  (NFDataX r) =>
  String ->
  (Int, Int) ->
  (HasCircuitContext => r) ->
  IO r
recordAndDump name slice sim = do
  (forced, traces, probes) <-
    withCircuitContext $ do
      -- Bind the implicit-parameter-constrained @sim@ to ONE monomorphic thunk
      -- before forcing. Forcing it twice would instantiate the @?circuitContext@
      -- dictionary twice and register every signal twice (spurious
      -- @name_0@/@name_1@ siblings).
      let forced = sim
      _ <- evaluate (forced `deepseqX` forced)
      pure forced
  -- Record as the latest run for this name, UNLESS it traced nothing (e.g. a
  -- zero-cycle case): an empty run must not clobber a good earlier waveform.
  -- The render is deferred (see 'flushWaveforms') so we render/write once, not
  -- once per hedgehog case.
  -- Render the VCD NOW, not at 'flushWaveforms', so this run's (large)
  -- trace+probe maps are forced to a compact 'Text' and released immediately
  -- rather than kept alive across every instrumented test until suite end.
  unless (Map.null traces && Map.null probes) $ do
    rendered <- dumpVCDSW slice traces probes
    case rendered of
      Left _ -> pure ()
      Right (txt, meta) -> do
        txt' <- evaluate txt
        atomicModifyIORef' pendingWaveforms $ \m ->
          (Map.insert (waveformPath name) (txt', meta) m, ())
  pure forced

{- | Render and write every waveform accumulated this run — one file per name,
each holding that name's LAST data-producing run (see the module header). Call
once, after all tests have finished. Never throws: a name whose last run
renders to nothing is simply skipped, and each file is written atomically
(unique temp + rename).
-}
flushWaveforms :: IO ()
flushWaveforms = do
  pending <- readIORef pendingWaveforms
  unless (Map.null pending) $ do
    createDirectoryIfMissing True waveformDir
    forM_ (Map.toList pending) $ \(path, (txt, meta)) -> do
      (tmp, h) <- openTempFile waveformDir (takeFileName path <.> "tmp")
      TIO.hPutStr h txt
      hClose h
      renameFile tmp path
      -- The ADT sidecar beside the VCD: same base name, .json. A typed-waveform
      -- viewer reads it to decode the bits this VCD only names.
      Json.encodeFile (path -<.> "json") meta
