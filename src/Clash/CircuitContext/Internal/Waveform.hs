{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

What "Clash.CircuitContext.Waveform" and "Clash.CircuitContext.Waveform.Hedgehog"
share: where a waveform goes, how a recording becomes the pair of files a
viewer reads, and how a signal is simulated with or without recording.

== Memory

A recording is rendered and written as soon as its run ends, and nothing
keeps it afterwards. Retaining anything lazy here (notably the sidecar's
'Json.Value', a lazy tree over the trace maps) retains that run's entire
history: in an earlier, process-global store that meant every instrumented
test's history at once, 26 GB resident, measured. Nothing here serializes
tests either, so a parallel runner is free to use every core.
-}
module Clash.CircuitContext.Internal.Waveform (
  waveformPath,
  waveformsRequested,
  simulate,
  record,
  save,
  removeWaveform,
  announce,
) where

import Control.Exception (evaluate)
import Data.Char (toLower)
import System.Directory (
  createDirectoryIfMissing,
  makeAbsolute,
  removePathForcibly,
  renameFile,
 )
import System.Environment (lookupEnv)
import System.FilePath (takeFileName, (-<.>), (<.>), (</>))
import System.IO (hClose, hPutStrLn, openTempFile, stderr)

import qualified Data.Aeson as Json
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO

import Clash.Explicit.Signal (Signal, sampleN)
import Clash.Prelude (NFDataX, deepseqX)

import Clash.CircuitContext.Core (
  HasCircuitContext,
  ProbeMap,
  TraceData,
  recordedCycles,
  withCircuitContext,
  withoutCircuitContext,
 )
import Clash.CircuitContext.Shockwaves (dumpVCDSW)

{- | The @.vcd@ path of a waveform name: @waveforms\/\<name\>.vcd@, relative to
the process's WORKING DIRECTORY. That is not the same place under every way
of running a suite (@cabal test@ runs in the package directory, @cabal run@
where it was invoked), which is why a failure's waveform is announced with
its absolute path.
-}
waveformPath :: String -> FilePath
waveformPath name = "waveforms" </> name <.> "vcd"

{- | Has the developer asked for every waveform? Reads @CCC_WAVEFORMS@: unset,
empty, @0@, @no@, @off@ or @false@ mean no.
-}
waveformsRequested :: IO Bool
waveformsRequested = do
  v <- lookupEnv "CCC_WAVEFORMS"
  pure $ case fmap (map toLower) v of
    Nothing -> False
    Just "" -> False
    Just "0" -> False
    Just "no" -> False
    Just "off" -> False
    Just "false" -> False
    Just _ -> True

{- | @n@ samples of the signal, forced, without recording: every recorder is
the identity under 'withoutCircuitContext', so nothing registers and nothing
accumulates.
-}
simulate :: (NFDataX a) => Int -> (HasCircuitContext => Signal dom a) -> IO [a]
simulate n sig = evaluate (let xs = withoutCircuitContext (sampleN n sig) in xs `deepseqX` xs)

{- | @n@ samples of the signal under a fresh recording context, forced so every
trace and probe fires, and the recording.

The samples are bound to ONE thunk before they are forced. Forcing the
constrained value twice would instantiate the context twice and register every
signal twice (spurious @name_0@\/@name_1@ siblings).
-}
record ::
  (NFDataX a) =>
  Int ->
  (HasCircuitContext => Signal dom a) ->
  IO ([a], TraceData, ProbeMap)
record n sig =
  withCircuitContext $ do
    let xs = sampleN n sig
    evaluate (xs `deepseqX` xs)

{- | Render a recording and write it as @waveforms\/\<name\>.vcd@ and its
@.json@ sidecar. 'False', and nothing written, when the run recorded nothing
or the render failed; an earlier file is then left alone.

The waveform covers the cycles the run forced ('recordedCycles'). Both files
go through a temporary file and a rename, and the SIDECAR LANDS FIRST: if the
@.vcd@ is there, its @.json@ is there and complete. (Not that the two are from
the same run: a crash between the renames leaves a new @.json@ beside the
previous run's @.vcd@.)
-}
save :: String -> TraceData -> ProbeMap -> IO Bool
save name traces probes
  | Map.null traces && Map.null probes = pure False
  | end == 0 = pure False
  | otherwise = do
      rendered <- dumpVCDSW (0, end) traces probes
      case rendered of
        Left _ -> pure False
        Right (txt, meta) -> do
          txt' <- evaluate txt
          meta' <- evaluate (ByteString.toStrict (Json.encode meta))
          createDirectoryIfMissing True "waveforms"
          (jTmp, jH) <- openTempFile "waveforms" (takeFileName sidecar <.> "tmp")
          ByteString.hPut jH meta'
          hClose jH
          renameFile jTmp sidecar
          (tmp, h) <- openTempFile "waveforms" (takeFileName path <.> "tmp")
          TIO.hPutStr h txt'
          hClose h
          renameFile tmp path
          pure True
 where
  end = recordedCycles traces probes
  path = waveformPath name
  sidecar = path -<.> "json"

-- | Delete @waveforms\/\<name\>.vcd@ and its @.json@, if they exist.
removeWaveform :: String -> IO ()
removeWaveform name =
  mapM_ removePathForcibly [waveformPath name, waveformPath name -<.> "json"]

{- | Say on stderr where a failure's waveform landed, as an ABSOLUTE path: the
directory is relative to the working directory, which differs between ways of
running a suite.
-}
announce :: String -> IO ()
announce name = do
  path <- makeAbsolute (waveformPath name)
  hPutStrLn stderr ("clash-circuit-context: wrote " <> path)
