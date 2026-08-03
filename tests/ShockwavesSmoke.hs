{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{- | Both halves of the combined output, from one simulation and with no manual
tracing calls:

* the HIERARCHY and values come from this package — @top@ and @stage@ are
  components because they are @OPAQUE@ and carry 'HasCircuitContext', and their
  local bindings are auto-traced by the plugin's renamer;
* the ADT DESCRIPTION comes from @clash-shockwaves@ — every traced payload has a
  'Waveform' instance, so registration captures its constructors, fields and bit
  ranges, and 'adtSidecar' emits them in the schema a typed-waveform viewer
  reads.

The design below is deliberately ADT-shaped rather than a counter: a sum type
(@Phase@) so constructor names have to survive, and a record over that sum
(@Packet@) so a nested structure does.
-}
module Main where

import qualified Prelude as P

import Control.Exception (evaluate)
import Data.List (isInfixOf)
import System.Exit (exitFailure)

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazyChar8
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.CircuitContext.Shockwaves (dumpVCDSW)
import Clash.Shockwaves.Internal.Waveform (Waveform)
import Clash.Shockwaves.LUT (WaveformForLut (..), WaveformLUT)

import qualified Data.Aeson.KeyMap as KeyMap

-- | A sum type: its CONSTRUCTOR names are what the sidecar must carry.
data Phase = Idle | Busy | Done
  deriving (Show, Generic, BitPack, NFDataX, Waveform)

{- | A record over that sum, so the sidecar has to describe a nested structure
(field names AND the inner type's constructors).
-}
data Packet = Packet
  { pAddr :: Unsigned 8
  , pPhase :: Phase
  }
  deriving (Show, Generic, BitPack, NFDataX, Waveform)

{- | A LUT-translated payload: its sidecar translations are built from the bit
patterns that actually occurred, one LUT entry each — which makes it the type
that notices when a value is dumped without its entry.
-}
newtype Opcode = Opcode (Unsigned 2)
  deriving (Generic, Show)
  deriving anyclass (BitPack, NFDataX, WaveformLUT)

deriving via WaveformForLut Opcode instance Waveform Opcode

{- | A component: @OPAQUE@ + 'HasCircuitContext', so the plugin wraps it in its
own VCD scope and auto-traces @packet@ and @phase@ underneath.
-}
stage ::
  (HasCircuitContext) =>
  Clock System ->
  Reset System ->
  Signal System (Unsigned 8) ->
  Signal System Packet
stage clk rst addr = packet
 where
  phase = register clk rst enableGen Idle (next <$> phase)
  packet = Packet <$> addr <*> phase
  next p = case p of
    Idle -> Busy
    Busy -> Done
    Done -> Idle
{-# OPAQUE stage #-}

-- | The top component, holding two @stage@ instances so scopes have siblings.
top ::
  (HasCircuitContext) =>
  Clock System ->
  Reset System ->
  Signal System (Unsigned 8) ->
  Signal System (Unsigned 8)
top clk rst addr = out
 where
  a = stage clk rst addr
  b = stage clk rst (addr + 1)
  out = (+) <$> (pAddr <$> a) <*> (pAddr <$> b)
{-# OPAQUE top #-}

check :: String -> Bool -> IO ()
check what ok
  | ok = putStrLn ("ok: " <> what)
  | otherwise = putStrLn ("FAIL: " <> what) >> exitFailure

main :: IO ()
main = do
  let clk = clockGen
      rst = resetGenN (SNat @2)
  (samples, traces, probes) <- withCircuitContext $ do
    let xs = sampleN 16 (top clk rst (fromList [0 ..]))
    _ <- evaluate (xs `deepseqX` xs)
    pure xs
  putStrLn ("samples: " <> show samples)

  rendered <- dumpVCDSW (0, 16) traces probes
  case rendered of
    Left err -> putStrLn ("FAIL: dump failed: " <> err) >> exitFailure
    Right (vcd, meta) -> do
      TextIO.writeFile "shockwaves-smoke.vcd" vcd
      Json.encodeFile "shockwaves-smoke.json" meta
      let
        v = Text.unpack vcd
        j = ByteStringLazyChar8.unpack (Json.encode meta)

      -- Hierarchy: this package's contribution.
      check "VCD nests a scope per component" ("$scope module top" `isInfixOf` v)
      check
        "two stage instances get sibling scopes"
        ("stage_0" `isInfixOf` v P.&& "stage_1" `isInfixOf` v)
      check "auto-traced ADT binding present" ("packet" `isInfixOf` v)

      -- ADT description: clash-shockwaves' contribution, same simulation.
      check "sidecar has the shockwaves schema" $
        P.all (`isInfixOf` j) ["\"signals\"", "\"types\"", "\"luts\""]
      check "sum-type constructors described" $
        P.all (`isInfixOf` j) ["Idle", "Busy", "Done"]
      check "record field names described" $
        P.all (`isInfixOf` j) ["pAddr", "pPhase"]
      check
        "a descriptor is keyed by the VCD's own hierarchical path"
        ("top.stage_0.packet" `isInfixOf` j)

  -- LUT entries must cover the DRAINED cycles too. Sampling 4 cells commits
  -- cycles 0..2 (recording runs one cell behind), so cycle 3 — the only
  -- occurrence of @Opcode 3@ — reaches the VCD by being drained from the
  -- packed tail. Its LUT entry has to exist all the same, or the viewer
  -- decodes every cycle except the last one: for a counterexample, exactly
  -- the cycle the capture exists to show.
  (_, traces2, probes2) <- withCircuitContext $ do
    let op :: Signal System Opcode
        op = fromList (P.map Opcode [0, 0, 0, 3, 0])
        xs = sampleN 4 (traceSignalC "op" op)
    _ <- evaluate (xs `deepseqX` xs)
    pure xs
  rendered2 <- dumpVCDSW (0, 4) traces2 probes2
  case rendered2 of
    Left err -> putStrLn ("FAIL: LUT dump failed: " <> err) >> exitFailure
    Right (_, meta2) -> do
      let lutsJson = case meta2 of
            Json.Object o ->
              P.maybe "" (ByteStringLazyChar8.unpack . Json.encode)
                $ KeyMap.lookup "luts" o
            _ -> ""
      check
        "a value first occurring at the drained last cycle has its LUT entry"
        ("Opcode 3" `isInfixOf` lutsJson)
  putStrLn "shockwaves-smoke passed"
