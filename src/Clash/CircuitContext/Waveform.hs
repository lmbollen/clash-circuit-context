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

'writeWaveform' and 'withWaveform' take 'WaveformOptions' ('defaultOptions'
writes an FST, recorded in memory), and what 'Clash.Signal.Trace.dumpVCD'
takes: the 'Signal' to observe and a window @(start, stop)@ of cycles. The
simulation runs @stop@ cycles, and the waveform shows cycles @start@ to
@stop - 1@; use @(0, n)@ to see all @n@. The signal carries 'HasCircuitContext', which they supply. A
design with outputs in one domain gives one signal with @bundle@; a
@clash-protocols@ circuit gives one with @toSignals@. For outputs in more
than one domain, see "Clash.CircuitContext.Recording".

A waveform is a pair of files at the path you give, which has no extension:
@\<path\>.fst@ (or @.vcd@, see 'Format') and the @\<path\>.json@ sidecar a
typed viewer reads. The path is relative to the working directory, or
absolute (@\"waveforms\/counter\"@, @\"\/tmp\/out\/counter\"@), and missing
directories are made; messages name the absolute path. A path that cannot be
meant is refused before anything runs, with an error that says what to give
instead: an empty path, one that names a directory (it ends in a separator,
@.@ or @..@), and one that ends in @.fst@, @.vcd@, @.json@ or @.spill@. So is
a path whose directory cannot hold the files, in either 'Spill' mode: the
functions make the directory, and create a file in it, before the design
runs, and the error names the absolute path of what is in the way
(@\<dir\> is a file, not a directory@, @\<dir\> is not writable@). One path is one
waveform: writing it in one format deletes a waveform of the other format
at that path, and two calls (in one test or in two) must not share a path
(a recording with the spill file of one that is running is refused, see
'OnDisk'). While a
simulation records, its values are kept as compact records, in memory or in
@\<path\>.spill@ (see 'Spill'); a spill file is
deleted when the waveform is written, and kept when a recording run is
stopped or its waveform cannot be written, for 'recoverWaveform'.

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
  ErrorCall (ErrorCall),
  SomeAsyncException,
  SomeException,
  displayException,
  evaluate,
  fromException,
  throwIO,
  try,
 )

import Clash.Explicit.Signal (Signal, sampleN)
import Clash.Prelude (KnownDomain, NFDataX, deepseqX)

import Clash.CircuitContext.Core (
  HasCircuitContext,
  withoutCircuitContext,
 )
import Clash.CircuitContext.Internal.Message (say)
import Clash.CircuitContext.Internal.Waveform

{- | Simulate @stop@ cycles of a design and write the waveform of cycles
@[start, stop)@. Returns all @stop@ samples.

> main = do
>   out <- writeWaveform defaultOptions "waveforms/counter" (0, 100) (top clockGen resetGen enableGen)
>   print out

Cycles before @start@ are simulated, but not recorded. If nothing in the
window was recorded, no file is written, and a message on stderr says why,
in the cycles of the window. If the simulation throws, the waveform up to there is written, and the
exception goes on.

The waveform is what this function is called for, so when it cannot be
written, the function throws, with either 'Spill' option: an 'ErrorCall'
that names the waveform and says why (a full disk, a directory where the
file should go). With 'OnDisk', the spill file of the run is kept, for
'recoverWaveform', and the error names it. A directory that cannot hold the
waveform is refused before the design runs. When the simulation threw as
well, its exception goes on, and why the waveform was not written is said
on stderr.
-}
writeWaveform ::
  forall dom a.
  (KnownDomain dom, NFDataX a) =>
  WaveformOptions ->
  {- | The path of the waveform, without extension: @\<path\>.fst@ or @.vcd@.
  An empty path, a directory, a path that ends in @.fst@, @.vcd@, @.json@ or
  @.spill@, and a path whose directory cannot be made or written, are
  refused, before anything runs
  -}
  FilePath ->
  -- | The cycles to show, @(start, stop)@; @stop@ is the number of samples
  (Int, Int) ->
  -- | The signal to observe
  (HasCircuitContext => Signal dom a) ->
  IO [a]
writeWaveform opts path0 window@(_, stop) sig = do
  checkPath path0
  path <- resolvePath path0
  checkWindow path window
  checkDirectory path
  (out, rec) <- record opts path (fst (windowPs win)) $ do
    let xs = sampleN stop sig
    evaluate (xs `deepseqX` xs)
  written <- save Requested opts path win rec
  case (written, out) of
    -- The waveform is what this was called for. A run that threw goes on
    -- with its own exception, which says more.
    (Left (Unwritable why), Right _) ->
      throwIO (ErrorCall ("clash-circuit-context: " <> noWaveform path why))
    (Left unsaved, _) -> announce path (Left (unsavedReason unsaved))
    (Right _, _) -> pure ()
  either throwIO pure out
 where
  win = cycleWindow @dom window

{- | Run a test's check on @stop@ cycles of a design, and write the waveform
of cycles @[start, stop)@ only when the check fails.

> case_counter = withWaveform defaultOptions "waveforms/counter" (0, 100) (top clockGen resetGen enableGen) $ \out ->
>   out @?= expected

The simulation runs without recording, so a passing test costs nothing extra.
The waveform's directory is made, and checked, before the first run: a path
that cannot hold the waveform fails the test at once, also a test that
passes, and not only when it fails, when its waveform is needed. When the check throws (a failed assertion is an exception), the simulation runs
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
* The test fails with the check's exception, also when the second run
  cannot record (its spill file is in use by another recording, the disk is
  full): a message on stderr then says why there is no waveform.

To get the waveform of a test that passes, use 'writeWaveform'. To simulate
without recording at all, sample the design under 'withoutCircuitContext'.
(Building without the plugin is no substitute: an empty 'Traceable' instance
needs the plugin, and hand-written recorders still record.)
-}
withWaveform ::
  forall dom a b.
  (KnownDomain dom, NFDataX a) =>
  WaveformOptions ->
  {- | The path of the waveform, without extension: @\<path\>.fst@ or @.vcd@.
  An empty path, a directory, a path that ends in @.fst@, @.vcd@, @.json@ or
  @.spill@, and a path whose directory cannot be made or written, are
  refused, before anything runs
  -}
  FilePath ->
  -- | The cycles to show, @(start, stop)@; @stop@ is the number of samples
  (Int, Int) ->
  -- | The signal to observe
  (HasCircuitContext => Signal dom a) ->
  -- | The check
  ([a] -> IO b) ->
  IO b
withWaveform opts path0 window@(_, stop) sig check = do
  checkPath path0
  path <- resolvePath path0
  checkWindow path window
  checkDirectory path
  first <- try (check (withoutCircuitContext (sampleN stop sig)))
  case first of
    Right b -> removeFailureWaveform path >> pure b
    -- Re-simulating a large design on the way out of a cancelled test
    -- would turn "stop now" into minutes of work.
    Left err | isAsync err -> throwIO err
    Left err -> do
      -- A fresh instantiation of the signal, under a recording context: an
      -- independent simulation, recorded as far as the check forces it.
      -- The test fails with the check's exception whatever becomes of the
      -- waveform: an error of the recording (a spill file that another
      -- recording uses, a full disk) is said, not thrown in its place.
      recorded <- try $ do
        (again, rec) <- record opts path (fst (windowPs win)) (check (sampleN stop sig))
        case again of
          Left _ -> save Failure opts path win rec >>= announce path . either (Left . unsavedReason) Right
          Right _ -> do
            discard rec
            say
              ( path
                  <> ": the failure did not happen again with recording, so no"
                  <> " waveform was written. Simulate this test with writeWaveform"
                  <> " to record the run it checks."
              )
      case recorded of
        Left recErr
          | isAsync recErr -> throwIO recErr
          | otherwise -> say (path <> ": no waveform written: " <> displayException recErr)
        Right () -> pure ()
      throwIO err
 where
  win = cycleWindow @dom window

  isAsync :: SomeException -> Bool
  isAsync err = case fromException err of
    Just (_ :: SomeAsyncException) -> True
    Nothing -> False

{- | Write the waveform of a run that was stopped (a cancelled test, a killed
process) with its values in a spill file ('OnDisk'): from @\<path\>.spill@,
as the options' format, everything the run recorded up to where it stopped.
Deletes the spill file once the waveform is written. Returns the waveform's
absolute path, or why there is none.

The path is the waveform's, without extension, as the other functions take
it: @\"waveforms\/counter\"@ for @waveforms\/counter.spill@. A path that ends
in @.spill@ (or @.fst@, @.vcd@, @.json@), an empty path, and a directory are
refused, with a 'Left' that says what to give; so is a spill file whose
directory cannot hold the waveform (it is not writable), before the spill
file is read. The spill file is then kept.

> recoverWaveform defaultOptions "waveforms/counter" >>= print
-}
recoverWaveform :: WaveformOptions -> FilePath -> IO (Either String FilePath)
recoverWaveform = recover
