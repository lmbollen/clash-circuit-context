-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Waveform capture for @clash-protocols@ circuits.

The lifecycle (where files go, last-run-wins, the flush) lives in
"Clash.CircuitContext.Waveform". This module supplies only the piece that
cannot: a t'Circuit' is a function over 'Fwd'\/'Bwd' signals rather than a
'Signal', so it needs coercing before there is anything to trace. That bridge
cannot live in @clash-circuit-context@ because @clash-protocols@ depends on it
-- the reverse dependency would be a cycle -- so it lives here, next to the
protocol code.
-}
module Protocols.Waveform (
  withWaveformC,
  withWaveformCWhen,
  driveFwd,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy (..))

import qualified Data.List as List

import Clash.Prelude

import Clash.CircuitContext (HasCircuitContext, traceSignalC)
import Clash.CircuitContext.Waveform (WaveformSlot, recordAndDump, withWaveformWhen)

import Protocols (Bwd, Circuit, Fwd, toSignals)
import Protocols.Experimental.Simulate (Backpressure (boolsToBwd))

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
  (MonadIO m, Backpressure b, KnownDomain dom, BitPack x, NFDataX x) =>
  -- | This test's slot; the run is recorded into it.
  WaveformSlot ->
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
withWaveformC slot resetCycles nSamples sigName project mkCircuit =
  liftIO $
    recordAndDump slot (0, nSamples) $
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
  readys = boolsToBwd (Proxy @b) (List.replicate resetCycles False <> List.repeat True)

{- | 'withWaveformC', but only record when the flag is 'True'.

'False' drives and samples the circuit with recording off — no registration,
no render, no file — so a repeated property pays for the one case it keeps
and nothing for the rest. See 'Clash.CircuitContext.Waveform.withWaveformWhen'.
-}
withWaveformCWhen ::
  forall b dom x m.
  (MonadIO m, Backpressure b, KnownDomain dom, BitPack x, NFDataX x) =>
  -- | Record this run?
  Bool ->
  WaveformSlot ->
  -- | Reset cycles (far end held not-ready).
  Int ->
  -- | Number of cycles to sample.
  Int ->
  -- | Name of the traced wire in the VCD.
  String ->
  -- | Projection of the circuit's forward output to a 'BitPack'able signal.
  (Fwd b -> Signal dom x) ->
  -- | The fully driven circuit (left side @()@).
  (HasCircuitContext => Circuit () b) ->
  m [x]
withWaveformCWhen keep slot resetCycles nSamples sigName project mkCircuit =
  withWaveformWhen keep slot nSamples $
    sampleN nSamples (traceSignalC sigName (project (driveFwd resetCycles mkCircuit)))
