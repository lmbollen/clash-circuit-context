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
registration, rendered to JSON ('signalDescription') — and travels with the
recording, as do the lookup-table entries of the values that occurred
('recordingLuts'). Both are in a spill file too, so a stopped run's sidecar
is complete. The caller
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

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString as ByteString

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Data.Aeson.KeyMap as KeyMap

import Clash.CircuitContext.Internal.Recorder (
  Description (..),
  RecordedSignal (..),
 )

{- | The sidecar of these signals, each given with its full VCD scope path,
and of these lookup-table entries, in @clash-shockwaves@' schema:
@{ signals, types, luts }@ — the same three keys its own @dumpVCD@ emits, so
its Surfer plugin reads this unmodified. @signals@ maps a path to its type
name, @types@ carries each type's translator (the ADT structure), and @luts@
the lookup-table entries for the values that occurred.

Signals without a description are left out: their payload type has no
'Clash.Shockwaves.Waveform' instance, or one that cannot describe it. The
waveform still has their bits.

The sidecar cannot take the waveform down with it. A descriptor is rendered
by the payload type's 'Clash.Shockwaves.Waveform' instance, and
@clash-shockwaves@ reports a broken one by calling 'error' — a hand-written
instance whose translator width differs from the type's 'BitSize', most
plainly. The recorder renders each signal's description on its own and keeps
the error of one that throws ('Broken'); such a signal is left out here and
reported on stderr, naming it: the waveform still records its bits, and
every other signal keeps its description.
-}
sidecar :: [(String, RecordedSignal)] -> [ByteString.ByteString] -> IO Json.Value
sidecar named luts = do
  mapM_
    ( \(path, err) ->
        hPutStrLn
          stderr
          ( "clash-circuit-context: "
              <> path
              <> ": left out of the typed-waveform sidecar, because its"
              <> " clash-shockwaves description failed to render (the waveform"
              <> " still has its bits): "
              <> err
          )
    )
    [(path, err) | (path, s) <- named, Broken err <- [signalDescription s]]
  pure
    ( Json.object
        [ "signals" .= Map.fromList [(path, tyName) | (path, tyName, _) <- described]
        , "types"
            .= List.foldl' KeyMap.union KeyMap.empty [object types | (_, _, types) <- described]
        , -- Left-biased at both levels, as clash-shockwaves' own addValueT is
          -- within a signal: the first translation of a pattern stands.
          "luts" .= List.foldl' (KeyMap.unionWith inner) KeyMap.empty (map object luts)
        ]
    )
 where
  described =
    [ (path, tyName, types)
    | (path, s) <- named
    , Described tyName types <- [signalDescription s]
    ]
  object bs = case Json.decodeStrict bs of
    Just (Json.Object o) -> o
    _ -> KeyMap.empty
  inner (Json.Object a) (Json.Object b) = Json.Object (KeyMap.union a b)
  inner a _ = a
