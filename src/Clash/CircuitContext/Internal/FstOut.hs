{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The FST writer: a 'Recording' as an FST file, written as its records are
read, as "Clash.CircuitContext.Internal.Vcd" writes a VCD. The two show the
same signals under the same names, in the same scope order, with the same
aliases, over the same window; FST times are in ps. The FST is compressed
(zlib, by @libfst@), and much smaller than the VCD.
-}
module Clash.CircuitContext.Internal.FstOut (
  writeFst,
) where

import Control.Exception (onException)
import Control.Monad (foldM, forM_, when)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, removePathForcibly, renameFile)
import System.FilePath (takeDirectory, takeFileName, (<.>))
import System.IO (hClose, openTempFile)

import Clash.CircuitContext.Internal.Bits (bits)
import qualified Clash.CircuitContext.Internal.Fst as Fst
import Clash.CircuitContext.Internal.Recorder (
  Change (..),
  Kind (..),
  RecordedSignal (..),
  Recording (..),
  readChanges,
  timescale,
  withReader,
 )
import Clash.CircuitContext.Internal.Vcd (
  aliasMap,
  ioLeft,
  namedSignals,
  naturalKey,
  shownWindow,
  sourceOrder,
  splitPath,
  vcdPath,
  writeSidecar,
 )

{- | Write a recording's waveform over the window @[start, stop)@, in units
of its timescale (for a single domain: cycles): the FST at this path, with
this comment, and its typed-waveform sidecar beside it, with extension
@.json@. Creates the directory.

As 'Clash.CircuitContext.Internal.Vcd.writeFiles' does: both files go
through a temporary file and a rename, the sidecar lands first, and on
'Left' (see 'shownWindow'; also when a file cannot be written) neither is
written. The FST ends at @stop@, as the VCD does.
-}
writeFst :: String -> (Int, Int) -> Recording -> FilePath -> IO (Either String ())
writeFst comment window rec path = case shownWindow rec window of
  Left err -> pure (Left err)
  Right shown -> ioLeft (Right () <$ writeShown shown)
 where
  named = namedSignals rec
  dir = takeDirectory path
  scale = timescale rec

  writeShown (start, stop) = withReader rec $ \records -> do
    createDirectoryIfMissing True dir
    aliasOf <- aliasMap records [(n, s) | (n, s) <- named, signalKind s == TraceKind]
    -- libfst writes to a path, so the temporary file only reserves a name.
    (fTmp, fH) <- openTempFile dir (takeFileName path <.> "tmp")
    hClose fH
    flip onException (removePathForcibly fTmp) $ do
      let
        order = sourceOrder named
        tree =
          List.foldl'
            (\t (n, s) -> insert (splitPath (vcdPath n)) (n, s) t)
            (Tree Map.empty [])
            (List.sortOn (order . fst) named)
        groupOf n = Map.findWithDefault n n aliasOf
      w <- Fst.create fTmp Fst.Zlib comment
      flip onException (Fst.close w) $ do
        handles <- declareTree w groupOf tree
        let reps = [(h, s) | (n, s) <- named, Map.notMember n aliasOf, Just h <- [Map.lookup n handles]]
        streams <- mapM (\(h, s) -> (,) (h, s) <$> readChanges records s) reps
        let starts = [(h, s, startAt Nothing (events s cs)) | ((h, s), cs) <- streams]
        Fst.timeChange w (start * scale)
        forM_ starts $ \(h, s, (v, _)) -> Fst.valueChange w h (bits (signalWidth s) v)
        merge
          w
          ( Map.fromList
              [ ((t, h), (v, s, mv, rest))
              | (h, s, (v, (t, mv) : rest)) <- starts
              , t < stop
              ]
          )
        -- The end of the window, so that a viewer shows its last cycle whole.
        Fst.timeChange w (stop * scale)
      Fst.close w
      writeSidecar rec path
      renameFile fTmp path
   where
    -- A signal's records as (time, value), then 'Nothing' where it ends.
    events s cs =
      [(c * k, mv) | Change c mv <- cs] ++ [(signalEnd s * k, Nothing)]
     where
      k = signalPeriod s `div` scale

    -- The value at @start@, and what comes after it.
    startAt _ ((t, mv) : rest) | t <= start = startAt mv rest
    startAt v rest = (v, rest)

    -- The changes of all signals, by time, then by var: a k-way merge of
    -- their streams, each read as it goes.
    merge w = go Nothing
     where
      go lastT q = case Map.minViewWithKey q of
        Nothing -> pure ()
        Just (((t, h), (cur, s, mv, rest)), q') -> do
          let next = case rest of
                (t2, mv2) : rest2 | t2 < stop -> Map.insert (t2, h) (mv, s, mv2, rest2) q'
                _ -> q'
          if mv == cur
            then go lastT next
            else do
              when (lastT /= Just t) (Fst.timeChange w (t * scale))
              Fst.valueChange w h (bits (signalWidth s) mv)
              go (Just t) next

-- | Scopes by name, and the vars declared directly in a scope, in order.
data Tree = Tree (Map.Map String Tree) [(String, (String, RecordedSignal))]

-- | Insert a var along its scope path; the last segment names it.
insert :: [String] -> (String, RecordedSignal) -> Tree -> Tree
insert [] _ t = t
insert [leaf] v (Tree cs vs) = Tree cs (vs ++ [(leaf, v)])
insert (seg : rest) v (Tree cs vs) =
  Tree (Map.alter (Just . insert rest v . fromMaybe (Tree Map.empty [])) seg cs) vs

{- | Declare every var, scope by scope: the stock @logic@ scope first, then
the others in 'naturalKey' order, as the VCD does. Returns each name's var;
the first var of an alias group declared holds the group's values, and the
others are declared as its aliases.
-}
declareTree :: Fst.Writer -> (String -> String) -> Tree -> IO (Map.Map String Fst.Handle)
declareTree w groupOf (Tree top _) = foldM scope Map.empty (topOrder (Map.toList top))
 where
  topOrder cs =
    [c | c@("logic", _) <- cs]
      ++ List.sortOn (naturalKey . fst) [c | c@(k, _) <- cs, k /= "logic"]
  scope hs (nm, Tree cs vs) = do
    Fst.scope w nm
    hs' <- foldM var1 hs vs
    hs'' <- foldM scope hs' (List.sortOn (naturalKey . fst) (Map.toList cs))
    Fst.upscope w
    pure hs''
  -- 'hs' maps a name to its var, and an alias group's representative to the
  -- group's var.
  var1 hs (leaf, (n, s)) = case Map.lookup (groupOf n) hs of
    Just h -> do
      _ <- Fst.var w leaf (signalWidth s) (Just h)
      pure (Map.insert n h hs)
    Nothing -> do
      h <- Fst.var w leaf (signalWidth s) Nothing
      pure (Map.insert n h (Map.insert (groupOf n) h hs))
