{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Emitting waveforms from a TEST SUITE: the lifecycle around 'dumpVCDC' that
every instrumented suite otherwise has to reinvent — where files go, which run
of a repeated property survives, when the render happens, and how the pair of
files a typed viewer reads is written without a window in which they disagree.

Two entry points, by how the design is sampled:

* 'withWaveform' — designs sampled with @sampleN@\/@simulateN@. Hand it the
  simulation result; it is forced under a fresh recording context so every
  'Clash.CircuitContext.traceSignalC' and probe fires.

* 'withWaveformLazy' — designs whose consumer decides how far to simulate
  (a firmware test reading a UART stream until it sees its result). ONE lazy
  run serves as both the assertion and the capture, so nothing is simulated
  twice and the waveform covers exactly the cycles the test caused.

A @clash-protocols@ t'Protocols.Circuit' is not a 'Clash.Signal.Signal' and so
needs a coercion before any of this applies. That bridge cannot live here:
@clash-protocols@ depends on this package, so depending on it back would be a
cycle. Provide it alongside your protocol code (bittide keeps @withWaveformC@
in @bittide-extra@'s "Protocols.Waveform") and build it on 'withWaveform'.

== One waveform per test, holding the LAST run

A hedgehog property runs its body once per generated case, but a test should
leave behind exactly ONE waveform: its LAST run. Each capture records into the
test's own 'WaveformSlot', replacing whatever was there, and the slot is
written once when the test ends — so a 1000-case property writes one file, not
one per case.

\"Last run\" is the useful run either way, because of how hedgehog drives the
size parameter: on SUCCESS it grows size to its maximum, so the last case is
the most thorough; on FAILURE it shrinks and re-runs, so the last is the
minimal counterexample.

Distinct tests must use distinct names — one name is one file. Nothing needs
wiring into @main@: a test owns its slot, so nothing outlives it.

== Memory

Two properties are load-bearing rather than incidental, both learned the hard
way on a real suite:

* A captured run is reduced to forced, compact values immediately, and only
  one test's worth is ever held. Retaining anything lazy here — notably the
  sidecar's 'Json.Value', a lazy tree over the trace maps — retains that run's
  entire RLE history; in a process-global store that meant every instrumented
  test's history at once, 26 GB resident, measured.

Nothing here serializes tests. An earlier version did — a global lock around
the simulation phase — because a leak made each concurrent capture retain its
whole history, so running several at once multiplied gigabytes. With capture
bounded per test, a parallel runner is free to use every core.
-}
module Clash.CircuitContext.Waveform (
  -- * The per-test slot
  WaveformSlot,
  newWaveformSlot,
  withWaveformSlot,
  writeWaveformSlot,
  waveformSlotPath,

  -- * Capturing
  withWaveform,
  withWaveformLazy,
  recordAndDump,

  -- * Capturing only when it will be kept
  waveformsRequested,
  withWaveformWhen,
  withWaveformLazyWhen,
  withWaveformOnFailure,
  withWaveformOnFailure',

  -- * Writing
  waveformDir,
  waveformPath,
) where

import Control.Exception (SomeException, evaluate, finally, throwIO, try)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath (takeFileName, (-<.>), (<.>), (</>))
import System.Environment (lookupEnv)
import System.IO (hClose, hPutStrLn, openTempFile, stderr)

import qualified Data.Aeson as Json
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

import Clash.Prelude (NFDataX, deepseqX)

import Clash.CircuitContext.Core (
  HasCircuitContext,
  recordedCycles,
  withCircuitContext,
  withCircuitContextWindow,
  withCircuitContextWindowE,
  withoutCircuitContext,
 )
import Clash.CircuitContext.Shockwaves (dumpVCDSW)

-- | Directory (relative to the working directory) the VCDs are written to.
waveformDir :: FilePath
waveformDir = "waveforms"

-- | The @.vcd@ path for a waveform base name.
waveformPath :: String -> FilePath
waveformPath name = waveformDir </> name <.> "vcd"

{- | Where ONE test's waveform accumulates: the last data-producing run of
that test, rendered and waiting to be written.

A test creates its own slot and passes it to whichever capture function it
uses. This is deliberately not process-global state: waveforms from different
tests are unrelated, so a shared registry keyed by filename would couple them
for no reason, and — because it can only be drained once every test has
finished — it would have to retain every rendered VCD until the end of the
suite. Those are not small (bittide's 29 total ~1.5 GB, individual firmware
captures reaching 270 MB), and the only thing deferral buys is writing a
repeated property's file once instead of once per generated case, which
matters solely for waveforms measured in kilobytes.

Owned by the test, the same deferral costs one test's waveform at a time and
the file is written the moment that test ends.
-}
data WaveformSlot
  = WaveformSlot FilePath (IORef (Maybe (Text.Text, ByteString.ByteString)))

{- | A slot writing to @waveformDir\/\<name\>.vcd@ (and its @.json@ sidecar).
Names must be unique across tests: one name is one file.

Pair with 'writeWaveformSlot', or use 'withWaveformSlot' to bracket both.
-}
newWaveformSlot :: (MonadIO m) => String -> m WaveformSlot
newWaveformSlot name =
  liftIO (WaveformSlot (waveformPath name) <$> newIORef Nothing)

{- | Write the slot's surviving run, if it captured one. Idempotent: writing
twice writes the same bytes, and a slot that never captured anything (every
run empty, or the render failed) writes nothing rather than truncating a file.
-}
-- | The @.vcd@ path a slot writes to.
waveformSlotPath :: WaveformSlot -> FilePath
waveformSlotPath (WaveformSlot path _) = path

writeWaveformSlot :: (MonadIO m) => WaveformSlot -> m ()
writeWaveformSlot (WaveformSlot path pending) = liftIO $ do
  captured <- readIORef pending
  forM_ captured $ \(txt, meta) -> do
    createDirectoryIfMissing True waveformDir
    -- A waveform is a PAIR of files: the VCD, and the ADT sidecar a typed
    -- viewer reads to decode the bits the VCD only names. Both go through
    -- temp + rename, and the SIDECAR LANDS FIRST, so the invariant a reader
    -- depends on holds: if the .vcd is there, its .json is there and complete.
    let sidecar = path -<.> "json"
    (jTmp, jH) <- openTempFile waveformDir (takeFileName sidecar <.> "tmp")
    ByteString.hPut jH meta
    hClose jH
    renameFile jTmp sidecar

    (tmp, h) <- openTempFile waveformDir (takeFileName path <.> "tmp")
    TIO.hPutStr h txt
    hClose h
    renameFile tmp path

{- | Bracket a test with its slot: create it, run the test, and write whatever
the test captured — even if it throws, so a failing test still leaves the
waveform that shows why.

> case_myTest = withWaveformSlot "my_test" $ \wf ->
>   withWaveformLazy wf 100_000 (simStream cfg) $ \out -> …
-}
withWaveformSlot :: String -> (WaveformSlot -> IO a) -> IO a
withWaveformSlot name act = do
  slot <- newWaveformSlot name
  act slot `finally` writeWaveformSlot slot

{- | Record a run into the slot, replacing any earlier one (LAST run wins).

Forces both parts to compact, fully evaluated values first. A 'Json.Value' is
a lazy tree whose thunks close over the trace and probe maps it was built
from, so retaining an unforced one retains that run's entire RLE history —
26 GB resident across a suite, measured, when this was left lazy.
-}
capture :: WaveformSlot -> Text.Text -> Json.Value -> IO ()
capture (WaveformSlot _ pending) txt meta = do
  txt' <- evaluate txt
  meta' <- evaluate (ByteString.toStrict (Json.encode meta))
  writeIORef pending (Just (txt', meta'))


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
  -- | This test's slot; the run is recorded into it.
  WaveformSlot ->
  -- | Dump window, starting at 0. For single-clock traces this is in cycles;
  -- when traces span multiple clock domains the unit is one VCD tick.
  Int ->
  -- | The simulation result to trace and return.
  (HasCircuitContext => r) ->
  m r
withWaveform slot nSamples sim = liftIO (recordAndDump slot (0, nSamples) sim)


{- | Shared core: force @sim@ under a fresh recording context (so every
@traceSignalC@\/probe fires), record this run as @name@'s pending waveform
(overwriting the previous — LAST run wins), and return the forced result. The
The waveform is written before returning.
-}

{- | Single-run waveform capture: simulate ONCE, lazily.

@sim@ is the simulation value under a recording context — typically the
output stream of @sampleC conf (dut …)@ where the DUT carries
'HasCircuitContext'. @consume@ forces exactly as much of it as the test
needs and may exit early (e.g. stop at the first @\"RESULT: OK\"@). The
recorded waveform covers precisely the cycles the consumer caused to be
simulated.

This replaces the {separate strict re-run over a fixed window} pattern for
the firmware tests: that design simulated every DUT TWICE and always for the
full worst-case window (600 k–1 M cycles) regardless of when the test
actually finished — the dominant compute and memory cost of the suite. Here
the assertion run IS the capture run: no second simulation, no dead-air
cycles past the exit point, and memory is bounded by what actually ran.
-}
withWaveformLazy ::
  forall r a m.
  (MonadIO m) =>
  -- | This test's slot; the run is recorded into it.
  WaveformSlot ->
  -- | Trailing capture window in cycles: the waveform keeps (at least) this
  -- many cycles of history before the point the simulation stopped; older
  -- cycles render @x@. Bounds recorder memory and VCD size for signals that
  -- change every cycle (program counters, busses). 'maxBound' = unlimited.
  Int ->
  -- | The (lazy) simulation value; consumed at most once.
  (HasCircuitContext => r) ->
  -- | Consumer: forces what the test needs, returns the test's result.
  (r -> IO a) ->
  m a
withWaveformLazy slot window sim consume = liftIO $ do
  (a, traces, probes) <- withCircuitContextWindow window (consume sim)
  let end = recordedCycles traces probes
  unless (Map.null traces && Map.null probes || end == 0) $ do
    rendered <- dumpVCDSW (0, end) traces probes
    case rendered of
      Left _ -> pure ()
      Right (txt, meta) -> do
        capture slot txt meta
  pure a


recordAndDump ::
  forall r.
  (NFDataX r) =>
  WaveformSlot ->
  (Int, Int) ->
  (HasCircuitContext => r) ->
  IO r
recordAndDump slot slice sim = do
  (forced, traces, probes) <-
    withCircuitContext $ do
      -- Bind the implicit-parameter-constrained @sim@ to ONE monomorphic thunk
      -- before forcing. Forcing it twice would instantiate the @?circuitContext@
      -- dictionary twice and register every signal twice (spurious
      -- @name_0@/@name_1@ siblings).
      let forced = sim
      _ <- evaluate (forced `deepseqX` forced)
      pure forced
  -- Write this run as @name@'s waveform, UNLESS it traced nothing (e.g. a
  -- zero-cycle case): an empty run must not clobber a good earlier one.
  -- Rendering and writing HERE, rather than deferring, is what releases this
  -- run's trace and probe maps immediately (see 'writeWaveform').
  unless (Map.null traces && Map.null probes) $ do
    rendered <- dumpVCDSW slice traces probes
    case rendered of
      Left _ -> pure () -- empty/failed render: don't clobber an earlier run
      Right (txt, meta) -> do
        capture slot txt meta
  pure forced

{- | Has the developer asked for waveforms this run?

Reads @CCC_WAVEFORMS@: unset, empty, @0@, @no@ or @false@ mean no. This is the
switch for captures that exist to produce artifacts rather than to debug a
failure — regenerating the documented waveforms, or looking at a passing
design — so that an ordinary green run pays nothing for them:

> keep <- waveformsRequested
> out  <- withWaveformWhen keep slot n …

Capturing a FAILING run needs no switch; that is
'withWaveformOnFailure' and is always on.
-}
waveformsRequested :: (MonadIO m) => m Bool
waveformsRequested = liftIO $ do
  v <- lookupEnv "CCC_WAVEFORMS"
  pure $ case fmap (map toLower) v of
    Nothing -> False
    Just "" -> False
    Just "0" -> False
    Just "no" -> False
    Just "false" -> False
    Just _ -> True

{- | 'withWaveform', but only record when the flag is 'True'.

'False' runs the simulation under 'withoutCircuitContext', where every
'Clash.CircuitContext.traceSignalC' is the identity: nothing registers,
nothing accumulates, nothing is rendered and no file is written. The
difference is not a filter at the end — the recording never happens.

That matters under a parallel test runner, where peak memory is the sum over
concurrently running tests. A repeated property that keeps one case out of a
hundred should pay for one, and this is how:

> keep <- recordLargestCase fired          -- see "…Waveform.Hedgehog"
> out  <- withWaveformWhen keep slot n $ …
-}
withWaveformWhen ::
  forall m r.
  (MonadIO m, NFDataX r) =>
  -- | Record this run?
  Bool ->
  WaveformSlot ->
  Int ->
  (HasCircuitContext => r) ->
  m r
withWaveformWhen keep slot nSamples sim
  | keep = withWaveform slot nSamples sim
  | otherwise = liftIO (evaluate (let r = withoutCircuitContext sim in r `deepseqX` r))

-- | 'withWaveformLazy', but only record when the flag is 'True'; see
-- 'withWaveformWhen'.
withWaveformLazyWhen ::
  forall r a.
  Bool ->
  WaveformSlot ->
  Int ->
  (HasCircuitContext => r) ->
  (r -> IO a) ->
  IO a
withWaveformLazyWhen keep slot window sim consume
  | keep = withWaveformLazy slot window sim consume
  | otherwise = consume (withoutCircuitContext sim)

{- | Capture a waveform ONLY for a failing run, by re-running it.

The simulation runs first with recording OFF, so a passing test costs nothing
at all — no maps, no render, no file. If @consume@ throws (an assertion
failing is a throw), the simulation is run a SECOND time with recording on,
that run's waveform is written, and the original exception is rethrown.

Two things to know before using it:

* @consume@ runs twice, so it must be re-runnable — forcing and assertions,
  not one-shot IO.

* the re-run must reproduce the failure. That holds for a design whose only
  inputs are the ones the test supplies, and NOT for one that resolves
  undefined values randomly per run: @clash-vexriscv@ does exactly that
  (@unsafeMakeDefinedRandom@ on its CPU inputs), and two runs of one such
  simulation demonstrably differ. Use 'withWaveformOnFailure'' there.
-}
withWaveformOnFailure ::
  forall r a.
  WaveformSlot ->
  Int ->
  (HasCircuitContext => r) ->
  (r -> IO a) ->
  IO a
withWaveformOnFailure slot window sim consume = do
  first <- try (consume (withoutCircuitContext sim))
  case first of
    Right a -> pure a
    Left err -> do
      -- Second run, recording. Instantiating the constrained @sim@ again gives
      -- a genuinely independent simulation (the same double-instantiation
      -- 'recordAndDump' is careful to AVOID; here it is the point).
      (again :: Either SomeException a, traces, probes) <-
        withCircuitContextWindowE window (consume sim)
      case again of
        Left _ -> do
          -- Reproduced: this recording IS of a failing run.
          let end = recordedCycles traces probes
          unless (Map.null traces && Map.null probes || end == 0) $ do
            rendered <- dumpVCDSW (0, end) traces probes
            case rendered of
              Left _ -> pure ()
              Right (txt, meta) -> capture slot txt meta
          writeWaveformSlot slot
        Right _ ->
          -- The re-run PASSED, so its waveform shows a working run and would
          -- be read as the failure's. Say so and write nothing; the design is
          -- not reproducible enough for this combinator (see the haddock) and
          -- the test should use 'withWaveformOnFailure''.
          hPutStrLn
            stderr
            ( "clash-circuit-context: "
                <> waveformSlotPath slot
                <> ": the failing run did not reproduce, so no waveform was"
                <> " written. Use withWaveformOnFailure' for this test."
            )
      throwIO (err :: SomeException)

{- | 'withWaveformOnFailure' for simulations that cannot be re-run faithfully.

Records the run as it happens — so the waveform is of the run that actually
failed — but renders and writes NOTHING unless @consume@ throws. A passing
test still produces no file and no render; it does pay for the recording
itself, which is the price of not needing a reproducible re-run.
-}
withWaveformOnFailure' ::
  forall r a.
  WaveformSlot ->
  Int ->
  (HasCircuitContext => r) ->
  (r -> IO a) ->
  IO a
withWaveformOnFailure' slot window sim consume = do
  (r, traces, probes) <- withCircuitContextWindowE window (consume sim)
  case r of
    Right a -> pure a -- discard the recording UNRENDERED
    Left err -> do
      let end = recordedCycles traces probes
      unless (Map.null traces && Map.null probes || end == 0) $ do
        rendered <- dumpVCDSW (0, end) traces probes
        case rendered of
          Left _ -> pure ()
          Right (txt, meta) -> capture slot txt meta
      writeWaveformSlot slot
      throwIO err
