{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Write the waveform of a simulation, from any IO code or from a test.

Both functions take what 'Clash.Signal.Trace.dumpVCD' takes: the 'Signal' to
observe and the number of samples. The signal carries 'HasCircuitContext',
which they supply. A design with outputs in one domain gives one signal with
@bundle@; a @clash-protocols@ circuit gives one with @toSignals@.

A waveform is a pair of files, @waveforms\/\<name\>.vcd@ and the
@waveforms\/\<name\>.json@ sidecar a typed viewer reads, relative to the working
directory. One name is one pair of files, so two tests must not share a name.

For a Hedgehog property, see "Clash.CircuitContext.Waveform.Hedgehog".
-}
module Clash.CircuitContext.Waveform (
  writeWaveform,
  withWaveform,
) where

import Control.Exception (
  SomeAsyncException,
  SomeException,
  fromException,
  throwIO,
  try,
 )
import Control.Monad (unless, when)
import System.IO (hPutStrLn, stderr)

import Clash.Explicit.Signal (Signal, sampleN)
import Clash.Prelude (NFDataX)

import Clash.CircuitContext.Core (
  HasCircuitContext,
  withCircuitContextWindowE,
  withoutCircuitContext,
 )
import Clash.CircuitContext.Internal.Waveform

{- | Simulate @n@ samples of a design and write its waveform. Returns the
samples.

> main = do
>   out <- writeWaveform "counter" 100 (top clockGen resetGen enableGen)
>   print out

If nothing in the simulation was recorded, no file is written, and a message
on stderr says so.
-}
writeWaveform ::
  (NFDataX a) =>
  -- | The name of the waveform: @waveforms\/\<name\>.vcd@
  String ->
  -- | Number of samples
  Int ->
  -- | The signal to observe
  (HasCircuitContext => Signal dom a) ->
  IO [a]
writeWaveform name n sig = do
  (out, traces, probes) <- record n sig
  written <- save Requested name traces probes
  unless written $
    hPutStrLn
      stderr
      ( "clash-circuit-context: "
          <> name
          <> ": nothing was recorded, so no waveform was written."
      )
  pure out

{- | Run a test's check on @n@ samples of a design, and write the waveform only
when the check fails.

> case_counter = withWaveform "counter" 100 (top clockGen resetGen enableGen) $ \out ->
>   out @?= expected

The simulation runs without recording, so a passing test costs nothing extra.
When the check throws (a failed assertion is an exception), the simulation runs
a second time with recording, the waveform is written, its absolute path is
printed on stderr, and the check's exception is rethrown. When the check
passes, a waveform left by an earlier failing run is deleted (one written by
'writeWaveform' is kept).

* The check gets the samples lazily. The waveform has the cycles the check
  evaluated, so a check that stops at the first wrong value gives a waveform
  that ends there.
* The check runs twice when it fails. Give it a check that only tests values,
  not work that must happen once.
* The second run must fail in the same way. If it passes, no waveform is
  written and a message on stderr says so. A design that resolves undefined
  values at random can do that; simulate it with 'writeWaveform' instead, and
  check the samples it returns.

To get the waveform of a test that passes, use 'writeWaveform'. To get no
waveforms at all, build without the plugin: then only hand-written recorders
record anything.
-}
withWaveform ::
  forall dom a b.
  (NFDataX a) =>
  -- | The name of the waveform: @waveforms\/\<name\>.vcd@
  String ->
  -- | Number of samples
  Int ->
  -- | The signal to observe
  (HasCircuitContext => Signal dom a) ->
  -- | The check
  ([a] -> IO b) ->
  IO b
withWaveform name n sig check = do
  first <- try (check (withoutCircuitContext (sampleN n sig)))
  case first of
    Right b -> removeFailureWaveform name >> pure b
    -- Re-simulating a large design on the way out of a cancelled test
    -- would turn "stop now" into minutes of work.
    Left err | isAsync err -> throwIO err
    Left err -> do
      (again, traces, probes) <- recorded
      case again of
        Left _ -> do
          written <- save Failure name traces probes
          when written (announce name)
        Right _ ->
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
  -- A fresh instantiation of the signal, under a recording context: an
  -- independent simulation, recorded as far as the check forces it.
  recorded = withCircuitContextWindowE maxBound (check (sampleN n sig))

  isAsync :: SomeException -> Bool
  isAsync err = case fromException err of
    Just (_ :: SomeAsyncException) -> True
    Nothing -> False
