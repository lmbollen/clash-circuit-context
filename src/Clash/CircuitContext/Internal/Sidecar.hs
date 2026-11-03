{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The @clash-shockwaves@ half of a waveform: a JSON sidecar describing what the
traced bits /mean/, next to the hierarchical VCD describing where they /are/.

The two libraries know different things, and neither subsumes the other:

* This package knows /where/ a signal is — design hierarchy, one scope per
  component instance, per-simulation isolation, and a three-state value model
  (@z@ never sampled, @x@ undefined, defined bits kept either way). It has no
  notion of what the bits mean: a @WishboneM2S@ is 72 anonymous bits.
* @clash-shockwaves@ knows /what/ a signal is — constructors, fields, bit ranges
  and styles, derived from the payload type via 'Clash.Shockwaves.Waveform'.
  Its own recorder is flat (a single @$scope module logic@) and lives in one
  process-global 'Data.IORef.IORef', so it cannot express hierarchy or keep
  concurrent simulations apart.

Combining them needs no coordination when the files are written, because the
descriptor is captured where the payload type is still known — at
registration, into 'signalAdt' — and travels with the recording. The caller
passes each signal with the full scope path the VCD declares it under, so a
descriptor lands on exactly that var: sibling suffixes (@name_0@, @name_1@)
included, and the @logic@ scope of a dot-free name.

Traces AND probes. A probe is how an FSM state is seen at all (it is state,
never a wire), so probes are precisely the signals whose constructors a
reader most wants named.
-}
module Clash.CircuitContext.Internal.Sidecar (
  sidecar,
) where

import Control.Exception (
  SomeAsyncException,
  SomeException,
  displayException,
  evaluate,
  fromException,
  throwIO,
  try,
 )
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import System.IO (Handle, hPutStrLn, stderr)

import qualified Data.ByteString.Lazy as ByteStringLazy

import Data.Aeson ((.=))
import qualified Data.Aeson as Json

import Clash.Shockwaves.Internal.BitList (BitList (BL))
import Clash.Shockwaves.Internal.Translator (
  addTypesT,
  addValueT,
  hasGeneratedLutT,
 )
import Clash.Shockwaves.Internal.Types (LUTMap, SignalMap, Translator, TypeMap)

import Clash.CircuitContext.Internal.Recorder (
  Change (..),
  RecordedSignal (..),
  Value,
  readChanges,
 )

{- | The sidecar of these signals, each given with its full VCD scope path,
in @clash-shockwaves@' schema: @{ signals, types, luts }@ — the same three
keys its own @dumpVCD@ emits, so its Surfer plugin reads this unmodified.
@signals@ maps a path to its type name, @types@ carries each type's
translator (the ADT structure), and @luts@ the lookup-table entries for the
values that occurred. The values are read through the spill file's handle.

Signals without a description are left out: their payload type has no
'Clash.Shockwaves.Waveform' instance, or one that cannot describe it. The VCD
still has their bits.

The sidecar cannot take the VCD down with it. A descriptor is rendered by the
payload type's 'Clash.Shockwaves.Waveform' instance, and @clash-shockwaves@
reports a broken one by calling 'error' — a hand-written instance whose
translator width differs from the type's 'BitSize', most plainly. Each
signal's share of the sidecar is therefore forced here, on its own, and one
that throws is left out and reported on stderr, naming the signal: the VCD
still records its bits, every other signal keeps its description, and the
result is safe to force.
-}
sidecar :: Handle -> [(String, RecordedSignal)] -> IO Json.Value
sidecar spill named = do
  parts <- mapM part [(path, s, adt) | (path, s) <- named, Just adt <- [signalAdt s]]
  pure (assemble (catMaybes parts))
 where
  part (path, s, (tyName, tr)) = do
    changes <- readChanges spill s
    renderable
      path
      ( Part
          (Map.singleton path tyName)
          (addTypesT tr Map.empty)
          (lut tr (signalWidth s) [v | Change _ (Just v) <- changes])
      )

  -- Encoding a part forces everything the final encoding of the sidecar will
  -- read from it, and since the kept part IS the forced one (shared, not
  -- rebuilt), the sidecar assembled from it cannot throw later.
  renderable path p = do
    rendered <- try (evaluate (ByteStringLazy.length (Json.encode (assemble [p]))))
    case rendered of
      Right _ -> pure (Just p)
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

{- | One described signal's share of the sidecar: its @signals@ entry, the
types its translator references, and the LUT entries for the values it took.
Kept apart per signal so that a broken descriptor loses only its own share.
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

{- | LUT entries are value-driven: a translator using the lookup-table
approach needs one per bit pattern the VCD shows. Translators that are not
LUT-shaped contribute nothing ('addValueT' returns no updates for them), so
they are skipped before their history is read at all.
-}
lut :: Translator -> Int -> [Value] -> LUTMap
lut tr width values
  | hasGeneratedLutT tr =
      List.foldl'
        (\acc f -> f acc)
        Map.empty
        [update | value <- values, update <- addValueT tr (toBitList width value)]
  | otherwise = Map.empty

{- | The recorder's packed @(mask, value)@ plus a width is exactly shockwaves'
dynamically sized bit list, and both use the same convention — a set mask bit
means that position is undefined — so this is a re-wrapping, not a conversion.
-}
toBitList :: Int -> Value -> BitList
toBitList width (mask, value) = BL mask value width
