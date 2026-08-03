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
'teAdt' — and travels with the trace. 'adtSidecar' then keys descriptors by the
same 'disambiguate'd paths 'dumpVCDC' writes into the VCD, so a descriptor lands
on exactly the name the VCD declares, sibling suffixes (@name_0@, @name_1@)
included. No name guessing, and nothing to keep in sync.

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

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Data.Aeson ((.=))
import qualified Data.Aeson as Json

import Clash.CircuitContext.Core (
  ProbeMap,
  TraceData,
  TraceEntry (..),
  disambiguate,
  dumpVCDC,
 )

import Clash.Shockwaves.Internal.BitList (BitList (BL))
import Clash.Shockwaves.Internal.Translator (addTypesT, addValueT)
import Clash.Shockwaves.Internal.Types (LUTMap, SignalMap, TypeMap)

{- | 'dumpVCDC' plus the typed-waveform sidecar: a hierarchical VCD, and the
ADT description of the signals in it ready for @Clash.Shockwaves.writeFileJSON@.
-}
dumpVCDSW ::
  (Int, Int) ->
  TraceData ->
  ProbeMap ->
  IO (Either String (Text, Json.Value))
dumpVCDSW slice@(offset, nSamples) traces probes = do
  vcd <- dumpVCDC slice traces probes
  pure (fmap (\t -> (t, adtSidecar (offset + nSamples) traces)) vcd)

{- | The sidecar alone, in @clash-shockwaves@' schema:
@{ signals, types, luts }@ — the same three keys its own @dumpVCD@ emits, so
its Surfer plugin consumes this unmodified.

@signals@ maps a signal's VCD path to its type name, @types@ carries each type's
translator (the ADT structure), and @luts@ the lookup-table entries for the
values that actually occurred.

Signals whose payload type has no 'Clash.Shockwaves.Waveform' instance simply
have no descriptor and are absent from @signals@ — they still appear in the VCD
as plain bits.
-}
adtSidecar ::
  {- | One past the last cycle the paired VCD emits (for a
  @'dumpVCDC' (offset, nSamples)@ dump: @offset + nSamples@). LUT entries
  must cover every value the VCD shows, and the VCD's final cycles are
  drained from 'teRest', not read from 'teRuns'.
  -}
  Int ->
  TraceData ->
  Json.Value
adtSidecar end traces =
  Json.object ["signals" .= signals, "types" .= types, "luts" .= luts]
 where
  -- The same transform 'dumpVCDC' applies, so these ARE the VCD's names.
  recorded = disambiguate traces

  described = [(path, adt, entry) | (path, entry) <- Map.toList recorded, Just adt <- [teAdt entry]]

  signals :: SignalMap
  signals = Map.fromList [(path, tyName) | (path, (tyName, _), _) <- described]

  types :: TypeMap
  types = foldl' (\acc (_, (_, tr), _) -> addTypesT tr acc) Map.empty described

  {- LUT entries are value-driven: a translator using the lookup-table approach
  needs one per bit pattern the VCD shows. That is the committed runs PLUS the
  cells 'dumpVCDC' drains from 'teRest' — at least the LAST forced cycle of
  every signal, which is never in 'teRuns' (see 'teForced'). A pattern that
  first occurs there (for a counterexample, typically the failure cycle) needs
  its entry like any other, or the viewer decodes every cycle except the one
  the capture exists to show. Translators that are not LUT-shaped contribute
  nothing ('addValueT' returns no updates for them). -}
  luts :: LUTMap
  luts =
    foldl'
      (\acc f -> f acc)
      Map.empty
      [ update
      | (_, (_, tr), entry) <- described
      , value <- dumpedValues entry
      , update <- addValueT tr (toBitList (teWidth entry) value)
      ]

  -- The values the paired VCD emits for one signal: committed runs, then the
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
