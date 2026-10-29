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
* the ADT DESCRIPTION comes from @clash-shockwaves@ — a payload with a
  'Waveform' instance has its constructors, fields and bit ranges captured at
  registration, and 'adtSidecar' emits them in the schema a typed-waveform
  viewer reads.

The design below is deliberately ADT-shaped rather than a counter: a sum type
(@Phase@) so constructor names have to survive, and a record over that sum
(@Packet@) so a nested structure does.

It also pins the two things a descriptor must NOT depend on:

* PROBES are described. @fsm@'s state is visible only from inside a mealy
  step, so it reaches the waveform as a probe and not a trace — which is
  precisely the signal whose constructors a reader wants named. Beside it,
  @ticks@ has 'BitPack' and no 'Waveform' and must still record, as bits:
  recording and describing are separate questions, and collapsing them would
  drop the size-polymorphic state a probe exists to see. The same goes for a
  trace: @stage@'s @raw@ is a 'Ticks' too, and is traced all the same.

* The sidecar is keyed on the paths the VCD actually declares: numbered from
  the trace AND probe key set together (keying it on the traces alone put
  descriptors on names the VCD never wrote), and including the @logic@ scope
  a dot-free name is declared under. Both are checked against the @$var@
  declarations of the dumped VCD, not against a substring.

And one thing a descriptor must never cost: the VCD. A broken 'Waveform'
instance (@Skewed@) loses its own description and nothing else — the files
are still written, and the test failure the capture was for still
propagates.
-}
module Main where

import qualified Prelude as P

import Control.Exception (Exception, SomeException, evaluate, fromException, throwIO, try)
import Control.Monad (when)
import Data.List (intercalate, isInfixOf, isSuffixOf)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitFailure)
import System.FilePath ((-<.>))

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazyChar8
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.CircuitContext.Shockwaves (dumpVCDSW)
import Clash.CircuitContext.Waveform (
  newWaveformSlot,
  waveformSlotPath,
  withWaveformOnFailure,
 )
import Clash.Shockwaves.LUT (WaveformForLut (..), WaveformLUT)
import Clash.Shockwaves.Waveform (Waveform (translator))

import qualified Data.Aeson.Key as Key
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

{- | 'BitPack' and 'NFDataX' but deliberately NO 'Waveform': a probe of this
records as bits, and stays out of the sidecar. That is the case probing must
keep — the state inside a mealy step is routinely a type shockwaves cannot
describe, and deciding "probe it" and "describe it" together would drop it.
-}
newtype Ticks = Ticks (Unsigned 4)
  deriving (Generic, Show)
  deriving anyclass (BitPack, NFDataX)

unTicks :: Ticks -> Unsigned 8
unTicks (Ticks t) = resize t

{- | A hand-written 'Waveform' instance that is WRONG: it borrows
@Unsigned 8@'s translator, eight bits wide, for a four-bit type.
@clash-shockwaves@ rejects that by calling 'error' when the descriptor is
rendered, which is the failure the sidecar must contain rather than pass on.
-}
newtype Skewed = Skewed (Unsigned 4)
  deriving (Generic, Show)
  deriving anyclass (BitPack, NFDataX)

instance Waveform Skewed where
  translator = translator @(Unsigned 8)

-- | The test failure a capture exists to show; it has to survive the capture.
data Deliberate = Deliberate
  deriving (Show)

instance Exception Deliberate

{- | A component: @OPAQUE@ + 'HasCircuitContext', so the plugin wraps it in its
own VCD scope and auto-traces @packet@, @phase@ and @raw@ underneath.
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
  -- No 'Waveform' for 'Ticks': traced as raw bits, absent from the sidecar.
  -- It feeds the output so that it is forced, and therefore registered.
  raw = Ticks . resize <$> addr
  packet = Packet <$> (addr + (unTicks <$> raw)) <*> phase
  next p = case p of
    Idle -> Busy
    Busy -> Done
    Done -> Idle
{-# OPAQUE stage #-}

{- | An FSM whose state is visible only from INSIDE a mealy step — a probe,
not a trace, which is how this package sees state at all. @nextPhase@ and
@ticks@ are auto-probed by the plugin (the step's 'HasProbe' signature
switches the subtree to probing), and they are the pair that pins the split:
@nextPhase@ is describable and reaches the sidecar, @ticks@ is not and still
records, as bits.
-}
fsm ::
  (HasCircuitContext) =>
  Clock System ->
  Reset System ->
  Signal System (Unsigned 8) ->
  Signal System (Unsigned 8)
fsm clk rst inp = mealyProbed clk rst enableGen step (Idle, Ticks 0) inp
 where
  step :: (HasProbe) => (Phase, Ticks) -> Unsigned 8 -> ((Phase, Ticks), Unsigned 8)
  step (p, Ticks t) i = ((nextPhase, ticks), out)
   where
    -- The output has to DEPEND on the state, or a lazy mealy never forces it
    -- and a probe of it never fires: a probe records when its expression is
    -- forced, exactly like a trace.
    out = i + resize t + phaseCode
    phaseCode = case p of
      Idle -> 0
      Busy -> 1
      Done -> 2
    ticks = Ticks (t + 1)
    nextPhase = case p of
      Idle -> Busy
      Busy -> Done
      Done -> Idle
{-# OPAQUE fsm #-}

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
  counted = fsm clk rst addr
  out = (+) <$> (pAddr <$> a) <*> ((+) <$> (pAddr <$> b) <*> counted)
{-# OPAQUE top #-}

-- | The sidecar's @signals@ object: the VCD paths that carry a description.
describedPaths :: Json.Value -> String
describedPaths meta = case meta of
  Json.Object o ->
    P.maybe "" (ByteStringLazyChar8.unpack . Json.encode) (KeyMap.lookup "signals" o)
  _ -> ""

-- | The sidecar's @signals@ keys: the VCD paths that carry a description.
signalPaths :: Json.Value -> [String]
signalPaths (Json.Object o)
  | Just (Json.Object sigs) <- KeyMap.lookup "signals" o =
      P.map Key.toString (KeyMap.keys sigs)
signalPaths _ = []

{- | Every var a VCD declares, by full scope path: what a viewer looks a
sidecar entry up by.
-}
declaredPaths :: String -> [String]
declaredPaths = go [] . P.map P.words . P.lines
 where
  go scopes (("$scope" : _ : nm : _) : rest) = go (nm : scopes) rest
  go (_ : scopes) (("$upscope" : _) : rest) = go scopes rest
  go scopes (("$var" : _ : _ : _ : ref : _) : rest) =
    intercalate "." (P.reverse (ref : scopes)) : go scopes rest
  go scopes (_ : rest) = go scopes rest
  go _ [] = []

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
      TextIO.writeFile "adt-sidecar.vcd" vcd
      Json.encodeFile "adt-sidecar.json" meta
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
      check "sidecar has the shockwaves schema"
        $ P.all (`isInfixOf` j) ["\"signals\"", "\"types\"", "\"luts\""]
      check "sum-type constructors described"
        $ P.all (`isInfixOf` j) ["Idle", "Busy", "Done"]
      check "record field names described"
        $ P.all (`isInfixOf` j) ["pAddr", "pPhase"]
      check
        "a descriptor is keyed by the VCD's own hierarchical path"
        ("top.stage_0.packet" `P.elem` signalPaths meta)
      check
        "every sidecar key is a var the VCD declares"
        (P.all (`P.elem` declaredPaths v) (signalPaths meta))
      check
        "a trace without Waveform is recorded, as bits"
        (P.any ("stage_0.raw" `isSuffixOf`) (declaredPaths v))
      check
        "...and stays out of the sidecar"
        (P.not (P.any ("raw" `isSuffixOf`) (signalPaths meta)))

      -- Probes, which carry no descriptor until one is asked for per type.
      check
        "the probed FSM state reached the VCD"
        ("nextPhase" `isInfixOf` v P.&& "ticks" `isInfixOf` v)
      check
        "a describable probe IS in the sidecar"
        ("nextPhase" `isInfixOf` describedPaths meta)
      check
        "an undescribable probe records anyway, and stays out of it"
        (P.not ("ticks" `isInfixOf` describedPaths meta))

  -- LUT entries must cover the DRAINED cycles too. Sampling 4 cells commits
  -- cycles 0..2 (recording runs one cell behind), so cycle 3 — the only
  -- occurrence of @Opcode 3@ — reaches the VCD by being drained from the
  -- packed tail. Its LUT entry has to exist all the same, or the viewer
  -- decodes every cycle except the last one: for a counterexample, exactly
  -- the cycle the capture exists to show.
  (_, traces2, probes2) <- withCircuitContext $ do
    let op :: Signal System Opcode
        op = fromList (P.map Opcode [0, 0, 0, 3, 0])
        -- 'traceSignalSW', the describing variant: an explicit 'traceSignalC'
        -- records raw bits only, as 'probe' does.
        xs = sampleN 4 (traceSignalSW "op" op)
    _ <- evaluate (xs `deepseqX` xs)
    pure xs
  rendered2 <- dumpVCDSW (0, 4) traces2 probes2
  case rendered2 of
    Left err -> putStrLn ("FAIL: LUT dump failed: " <> err) >> exitFailure
    Right (vcd2, meta2) -> do
      -- @op@ is traced outside any component, so the VCD declares it in the
      -- stock @logic@ scope, and a viewer looks it up as @logic.op@.
      check
        "a dot-free trace is keyed by its logic-scope path"
        ( "logic.op" `P.elem` signalPaths meta2
            P.&& "logic.op" `P.elem` declaredPaths (Text.unpack vcd2)
        )
      let lutsJson = case meta2 of
            Json.Object o ->
              P.maybe "" (ByteStringLazyChar8.unpack . Json.encode)
                $ KeyMap.lookup "luts" o
            _ -> ""
      check
        "a value first occurring at the drained last cycle has its LUT entry"
        ("Opcode 3" `isInfixOf` lutsJson)

  -- A broken descriptor, captured the way a failing test is: through
  -- 'withWaveformOnFailure', whose rendering used to throw shockwaves' error
  -- in place of the failure and before either file was written.
  slot <- newWaveformSlot "adt-sidecar-broken"
  let vcdFile = waveformSlotPath slot
      jsonFile = vcdFile -<.> "json"
  P.mapM_ (\f -> doesFileExist f >>= (`when` removeFile f)) [vcdFile, jsonFile]
  outcome <-
    try
      $ withWaveformOnFailure
        slot
        maxBound
        ( sampleN
            4
            ( bundle
                ( traceSignalSW "skewed" (fromList (P.map Skewed (P.cycle [1, 2, 3])))
                , traceSignalSW "phase" (fromList (P.cycle [Idle, Busy, Done]))
                ) ::
                Signal System (Skewed, Phase)
            )
        )
        (\xs -> evaluate (xs `deepseqX` ()) >> throwIO Deliberate)
  check
    "the test's own failure propagates past a broken descriptor"
    ( case outcome of
        Left err -> P.maybe False (\Deliberate -> True) (fromException err)
        Right () -> False
    )
  wroteVcd <- doesFileExist vcdFile
  check "the VCD is written all the same" wroteVcd
  vcd3 <- P.readFile vcdFile
  check
    "...and records the broken signal, as bits"
    ("logic.skewed" `P.elem` declaredPaths vcd3)
  meta3 <- Json.decodeFileStrict jsonFile :: IO (P.Maybe Json.Value)
  check
    "the sidecar leaves out the broken descriptor and keeps the rest"
    (P.fmap signalPaths meta3 P.== P.Just ["logic.phase"])
  putStrLn "adt-sidecar passed"
