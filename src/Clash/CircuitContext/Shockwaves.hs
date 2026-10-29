{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The @clash-shockwaves@ half of the output: a JSON sidecar describing what the
traced bits /mean/, next to the hierarchical VCD describing where they /are/.

The two libraries know different things, and neither subsumes the other:

* This package knows /where/ a signal is — design hierarchy, one scope per
  component instance, per-simulation isolation, and a three-state value model
  (@z@ never sampled, @x@ undefined, defined bits kept either way). It has no
  notion of what the bits mean: a @WishboneM2S@ is 72 anonymous bits.
* @clash-shockwaves@ knows /what/ a signal is — constructors, fields, bit ranges
  and styles, derived from the payload type via 'Waveform'. Its own recorder is
  flat (a single @$scope module logic@) and lives in one process-global
  'System.IO.Unsafe.unsafePerformIO' 'Data.IORef.IORef', so it cannot express
  hierarchy or keep concurrent simulations apart.

Combining them needs no coordination at dump time, because the descriptor is
captured where the payload type is still known — at registration, into
'teAdt' \/ 'peAdt' — and travels with the recording. 'adtSidecar' then keys
descriptors by the same 'disambiguate'd keys 'dumpVCDC' writes into the VCD,
turned into full scope paths by the same 'vcdPath' rule, so a descriptor
lands on exactly the var the VCD declares: sibling suffixes (@name_0@,
@name_1@) included, and the @logic@ scope a dot-free name is declared under.
No name guessing, and nothing to keep in sync.

Traces AND probes, over the same key set the dump uses. Both halves of that
matter: a probe is how an FSM state is seen at all (it is state, never a
wire), so probes are precisely the signals whose constructors a reader most
wants named; and sibling numbering is a function of the WHOLE key set, so
disambiguating the traces alone puts a descriptor on @phase@ where the VCD
declares @phase_0@ — the same trap 'dumpVCDC' documents at its @cleanOf@.

> (_, traces, probes) <- withCircuitContext $ do
>     let xs = sampleN 64 (top …)
>     evaluate (deepseqX xs xs)
> Right (vcd, meta) <- dumpVCDSW (0, 64) traces probes
> TIO.writeFile     "top.vcd"  vcd
> writeFileJSON     "top.json" meta
-}
module Clash.CircuitContext.Shockwaves (
  dumpVCDSW,
  adtSidecar,
) where

import Clash.Signal.Trace (Value)

import Control.Exception (
  SomeAsyncException,
  SomeException,
  displayException,
  evaluate,
  fromException,
  throwIO,
  try,
 )
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString.Lazy as ByteStringLazy

import Data.Aeson ((.=))
import qualified Data.Aeson as Json

import Clash.CircuitContext.Core (
  ProbeEntry (..),
  ProbeMap,
  TraceData,
  TraceEntry (..),
  disambiguate,
  dumpVCDC,
  vcdPath,
 )

import Clash.Shockwaves.Internal.BitList (BitList (BL))
import Clash.Shockwaves.Internal.Translator (
  addTypesT,
  addValueT,
  hasGeneratedLutT,
 )
import Clash.Shockwaves.Internal.Types (LUTMap, SignalMap, Translator, TypeMap)

{- | 'dumpVCDC' plus the typed-waveform sidecar: a hierarchical VCD, and the
ADT description of the signals in it ready for @Clash.Shockwaves.writeFileJSON@.

The sidecar cannot take the VCD down with it. A descriptor is rendered by the
payload type's 'Clash.Shockwaves.Waveform' instance, and @clash-shockwaves@
reports a broken one by calling 'error' — a hand-written instance whose
translator width differs from the type's 'BitSize', most plainly. Each
signal's share of the sidecar is therefore forced here, on its own, and one
that throws is left out and reported on stderr, naming the signal: the VCD
still records its bits, every other signal keeps its description, and the
result is safe to force. Without this, the error would surface wherever the
sidecar is first forced — in "Clash.CircuitContext.Waveform", before either
file is written, and in place of the test failure the capture was meant to
show.
-}
dumpVCDSW ::
  (Int, Int) ->
  TraceData ->
  ProbeMap ->
  IO (Either String (Text, Json.Value))
dumpVCDSW slice@(offset, nSamples) traces probes = do
  vcd <- dumpVCDC slice traces probes
  case vcd of
    Left err -> pure (Left err)
    Right txt -> do
      parts <- mapM renderable (sidecarParts (offset + nSamples) traces probes)
      pure (Right (txt, assemble (catMaybes parts)))
 where
  -- Encoding a part forces everything the final encoding of the sidecar will
  -- read from it, and since the kept part IS the forced one (shared, not
  -- rebuilt), the sidecar assembled from it cannot throw later.
  renderable (path, part) = do
    rendered <- try (evaluate (ByteStringLazy.length (Json.encode (partJson part))))
    case rendered of
      Right _ -> pure (Just part)
      -- Cancellation is not a broken descriptor; let it through.
      Left err | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
      Left (err :: SomeException) -> do
        hPutStrLn
          stderr
          ( "clash-circuit-context: "
              <> path
              <> ": left out of the typed-waveform sidecar, because its"
              <> " clash-shockwaves description failed to render (the VCD"
              <> " still has its bits): "
              <> takeWhile (/= '\n') (displayException err)
          )
        pure Nothing

{- | The sidecar alone, in @clash-shockwaves@' schema:
@{ signals, types, luts }@ — the same three keys its own @dumpVCD@ emits, so
its Surfer plugin consumes this unmodified.

@signals@ maps a signal's full VCD scope path ('vcdPath') to its type name, @types@ carries each type's
translator (the ADT structure), and @luts@ the lookup-table entries for the
values that actually occurred.

Signals whose payload type has no 'Clash.Shockwaves.Waveform' instance simply
have no descriptor and are absent from @signals@ — they still appear in the VCD
as plain bits. That covers both a payload that never got an instance and
one 'Clash.Shockwaves.Waveform' cannot describe at all, which
'Clash.CircuitContext.Core.traceSignalC' and 'Clash.CircuitContext.Core.probe'
record anyway (see 'Clash.CircuitContext.Core.traceSignalSW' and
'Clash.CircuitContext.Core.probeSW').

Unlike 'dumpVCDSW', this does not guard against a broken
'Clash.Shockwaves.Waveform' instance: forcing the result throws its 'error'.
-}
adtSidecar ::
  {- | One past the last cycle the paired VCD emits (for a
  @'dumpVCDC' (offset, nSamples)@ dump: @offset + nSamples@). LUT entries
  must cover every value the VCD shows, and the VCD's final cycles are
  drained from 'teRest', not read from 'teRuns'.
  -}
  Int ->
  TraceData ->
  ProbeMap ->
  Json.Value
adtSidecar end traces probes = assemble (map snd (sidecarParts end traces probes))

{- | One described signal's share of the sidecar: its @signals@ entry, the
types its translator references, and the LUT entries for the values it took.
Kept apart per signal so that 'dumpVCDSW' can drop the share of a broken
descriptor without losing the rest.
-}
data Part = Part SignalMap TypeMap LUTMap

-- | The sidecar the parts make up together.
assemble :: [Part] -> Json.Value
assemble parts =
  Json.object
    [ "signals" .= Map.unions [s | Part s _ _ <- parts]
    , "types" .= Map.unions [t | Part _ t _ <- parts]
    , -- Left-biased at both levels, as 'addValueT' itself is within a
      -- signal: the first translation of a pattern stands.
      "luts" .= Map.unionsWith Map.union [l | Part _ _ l <- parts]
    ]

-- | One part on its own, in the sidecar's schema; encoding it forces it.
partJson :: Part -> Json.Value
partJson part = assemble [part]

{- | The parts of every described signal, each with its full VCD scope path
('vcdPath'): a viewer looks a var up by that path, which for a dot-free key
includes the @logic@ scope 'dumpVCDC' declares it under.
-}
sidecarParts :: Int -> TraceData -> ProbeMap -> [(String, Part)]
sidecarParts end traces probes =
  [ (vcd, describe vcd adt width values)
  | (path, (Just adt, width, values)) <- Map.toList recorded
  , let vcd = vcdPath path
  ]
 where
  -- The same key set and the same transform 'dumpVCDC' applies (left-biased
  -- union, traces first), so these ARE the VCD's names.
  recorded =
    disambiguate (Map.union (Map.map ofTrace traces) (Map.map ofProbe probes))

  ofTrace entry = (teAdt entry, teWidth entry, dumpedValues entry)
  ofProbe entry =
    (peAdt entry, peWidth entry, [value | (_, _, value) <- peRuns entry])

  describe vcd (tyName, tr) width values =
    Part
      (Map.singleton vcd tyName)
      (addTypesT tr Map.empty)
      (lut tr width values)

  {- LUT entries are value-driven: a translator using the lookup-table approach
  needs one per bit pattern the VCD shows. That is the committed runs PLUS the
  cells 'dumpVCDC' drains from 'teRest' — at least the LAST forced cycle of
  every signal, which is never in 'teRuns' (see 'teForced'). A pattern that
  first occurs there (for a counterexample, typically the failure cycle) needs
  its entry like any other, or the viewer decodes every cycle except the one
  the capture exists to show. A probe has no such tail: it commits the cycle it
  fires on. Translators that are not LUT-shaped contribute nothing
  ('addValueT' returns no updates for them), so they are skipped before their
  history is walked at all. -}
  lut :: Translator -> Int -> [Value] -> LUTMap
  lut tr width values
    | hasGeneratedLutT tr =
        foldl'
          (\acc f -> f acc)
          Map.empty
          [update | value <- values, update <- addValueT tr (toBitList width value)]
    | otherwise = Map.empty

  -- The values the paired VCD emits for one trace: committed runs, then the
  -- drained tail of @[0, end)@. Draining forces exactly the packed cells
  -- 'dumpVCDC' forces for the same window.
  dumpedValues entry =
    [value | (_, _, value) <- teRuns entry]
      ++ take (end - covered) (teRest entry)
   where
    covered = case teRuns entry of
      [] -> 0
      (_, c, _) : _ -> c + 1

{- | The recorder's packed @(mask, value)@ plus a width is exactly shockwaves'
dynamically sized bit list, and both use the same convention — a set mask bit
means that position is undefined — so this is a re-wrapping, not a conversion.
-}
toBitList :: Int -> Value -> BitList
toBitList width (mask, value) = BL mask value width
