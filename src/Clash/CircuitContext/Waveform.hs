{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Write the waveform of a simulation, from any IO code or from a test.

Both functions take 'WaveformOptions' ('defaultOptions' writes an FST,
recorded in memory), and what 'Clash.Signal.Trace.dumpVCD' takes: the
'Signal' to observe and a window @(start, stop)@ of cycles. The simulation runs @stop@
cycles, and the waveform shows cycles @start@ to @stop - 1@; use @(0, n)@ to
see all @n@. The signal carries 'HasCircuitContext', which they supply. A
design with outputs in one domain gives one signal with @bundle@; a
@clash-protocols@ circuit gives one with @toSignals@. For outputs in more
than one domain, see "Clash.CircuitContext.Recording".

A waveform is a pair of files, @waveforms\/\<name\>.fst@ (or @.vcd@, see
'Format') and the @waveforms\/\<name\>.json@ sidecar a typed viewer reads,
relative to the working directory. One name is one pair of files, so two
tests must not share a name. While a simulation records, its values are kept
as compact records, in memory or in @waveforms\/\<name\>.spill@ (see
'Spill'); a spill file is deleted when the waveform is written, and kept when
the run is stopped, for 'recoverWaveform'.

> opts = defaultOptions {format = VCD, spill = OnDisk}

For a Hedgehog property, see "Clash.CircuitContext.Waveform.Hedgehog".
-}
module Clash.CircuitContext.Waveform (
  -- * Options
  WaveformOptions (..),
  Format (..),
  Spill (..),
  defaultOptions,

  -- * Writing a waveform
  writeWaveform,
  withWaveform,
  recoverWaveform,
) where

import Control.Exception (
  SomeAsyncException,
  SomeException,
  evaluate,
  fromException,
  throwIO,
  try,
 )
import Control.Monad (unless, when)
import System.IO (hPutStrLn, stderr)

import Clash.Explicit.Signal (Signal, sampleN)
import Clash.Prelude (KnownDomain, NFDataX, deepseqX)

import Clash.CircuitContext.Core (
  HasCircuitContext,
  withoutCircuitContext,
 )
import Clash.CircuitContext.Internal.Waveform

{- | Simulate @stop@ cycles of a design and write the waveform of cycles
@[start, stop)@. Returns all @stop@ samples.

> main = do
>   out <- writeWaveform defaultOptions "counter" (0, 100) (top clockGen resetGen enableGen)
>   print out

Cycles before @start@ are simulated, but not recorded. If nothing in the
simulation was recorded, no file is written, and a message on stderr says
so. If the simulation throws, the waveform up to there is written, and the
exception goes on.
-}
writeWaveform ::
  forall dom a.
  (KnownDomain dom, NFDataX a) =>
  WaveformOptions ->
  -- | The name of the waveform: @waveforms\/\<name\>.fst@ or @.vcd@
  String ->
  -- | The cycles to show, @(start, stop)@; @stop@ is the number of samples
  (Int, Int) ->
  -- | The signal to observe
  (HasCircuitContext => Signal dom a) ->
  IO [a]
writeWaveform opts name window@(_, stop) sig = do
  checkWindow name window
  (out, rec) <- record opts name (fst ps) $ do
    let xs = sampleN stop sig
    evaluate (xs `deepseqX` xs)
  written <- save Requested opts name ps rec
  unless written $
    hPutStrLn
      stderr
      ( "clash-circuit-context: "
          <> name
          <> ": nothing was recorded, so no waveform was written."
      )
  either throwIO pure out
 where
  ps = psWindow @dom window

{- | Run a test's check on @stop@ cycles of a design, and write the waveform
of cycles @[start, stop)@ only when the check fails.

> case_counter = withWaveform defaultOptions "counter" (0, 100) (top clockGen resetGen enableGen) $ \out ->
>   out @?= expected

The simulation runs without recording, so a passing test costs nothing extra.
When the check throws (a failed assertion is an exception), the simulation runs
a second time with recording, the waveform is written, its absolute path is
printed on stderr, and the check's exception is rethrown. When the check
passes, a waveform left by an earlier failing run is deleted (one written by
'writeWaveform' is kept).

* The check gets all @stop@ samples, lazily. The waveform has the cycles the
  check evaluated, so a check that stops at the first wrong value gives a
  waveform that ends there.
* The check runs twice when it fails. Give it a check that only tests values,
  not work that must happen once.
* The second run must fail in the same way. If it passes, no waveform is
  written and a message on stderr says so. A design that resolves undefined
  values at random can do that; simulate it with 'writeWaveform' instead, and
  check the samples it returns.

To get the waveform of a test that passes, use 'writeWaveform'. To simulate
without recording at all, sample the design under 'withoutCircuitContext'.
(Building without the plugin is no substitute: an empty 'Traceable' instance
needs the plugin, and hand-written recorders still record.)
-}
withWaveform ::
  forall dom a b.
  (KnownDomain dom, NFDataX a) =>
  WaveformOptions ->
  -- | The name of the waveform: @waveforms\/\<name\>.fst@ or @.vcd@
  String ->
  -- | The cycles to show, @(start, stop)@; @stop@ is the number of samples
  (Int, Int) ->
  -- | The signal to observe
  (HasCircuitContext => Signal dom a) ->
  -- | The check
  ([a] -> IO b) ->
  IO b
withWaveform opts name window@(_, stop) sig check = do
  checkWindow name window
  first <- try (check (withoutCircuitContext (sampleN stop sig)))
  case first of
    Right b -> removeFailureWaveform name >> pure b
    -- Re-simulating a large design on the way out of a cancelled test
    -- would turn "stop now" into minutes of work.
    Left err | isAsync err -> throwIO err
    Left err -> do
      -- A fresh instantiation of the signal, under a recording context: an
      -- independent simulation, recorded as far as the check forces it.
      (again, rec) <- record opts name (fst ps) (check (sampleN stop sig))
      case again of
        Left _ -> do
          written <- save Failure opts name ps rec
          when written (announce (format opts) name)
        Right _ -> do
          discard rec
          hPutStrLn
            stderr
            ( "clash-circuit-context: "
                <> name
                <> ": the failure did not happen again with recording, so no"
                <> " waveform was written. Simulate this test with writeWaveform"
                <> " to record the run it checks."
            )
      throwIO err
 where
  ps = psWindow @dom window

  isAsync :: SomeException -> Bool
  isAsync err = case fromException err of
    Just (_ :: SomeAsyncException) -> True
    Nothing -> False

{- | Write the waveform of a run that was stopped (a cancelled test, a killed
process) with its values in a spill file ('OnDisk'): from
@waveforms\/\<name\>.spill@, as the options' format, everything the run
recorded up to where it stopped. Deletes the spill file once the waveform is
written. Returns the waveform's path, or why there is none.

> recoverWaveform defaultOptions "counter" >>= print
-}
recoverWaveform :: WaveformOptions -> String -> IO (Either String FilePath)
recoverWaveform = recover
