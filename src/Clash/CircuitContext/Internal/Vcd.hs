{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The VCD writer: a 'Recording' as a hierarchical VCD, written as its records
are read, so neither the history nor the text is ever in memory as a whole.

The output is the one 'Clash.Signal.Trace.dumpVCD' gives for the same
signals, with three differences: dotted names are nested @$scope@s, signals
with the same history share one identifier code, and a signal from a slower
domain changes at its own clock edges (stock @dumpVCD@ shifts them).
Identifier codes are assigned as stock @dumpVCD@ assigns them: by period,
then by name, from the last name to the first.
-}
module Clash.CircuitContext.Internal.Vcd (
  writeFiles,
  namedSignals,
  disambiguate,
  vcdPath,
  renderVCD,

  -- * Shared with the FST writer
  shownWindow,
  writeSidecar,
  ioLeft,
  aliasMap,
  sourceOrder,
  parseSeg,
  parseLoc,
  splitPath,
  naturalKey,
) where

import Control.Exception (IOException, displayException, onException, try)
import Data.Char (isDigit, ord)
import Data.List (intercalate, sortOn)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Ord (Down (..))
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (
  createDirectoryIfMissing,
  removeFile,
  removePathForcibly,
  renameFile,
 )
import System.FilePath (takeDirectory, takeFileName, (-<.>), (<.>))
import System.IO (
  BufferMode (BlockBuffering),
  Handle,
  hClose,
  hSetBuffering,
  openBinaryTempFileWithDefaultPermissions,
 )

import qualified Data.Aeson as Json
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteStringLazy

import Clash.CircuitContext.Internal.Bits (bits)
import Clash.CircuitContext.Internal.Sidecar (sidecar)

import Clash.CircuitContext.Internal.Recorder (
  Change (..),
  Kind (..),
  Reader,
  RecordedSignal (..),
  Recording (..),
  Value,
  readChanges,
  ticks,
  timescale,
  withReader,
 )

{- | Write a recording's waveform over a window (see 'renderVCD'): the VCD at
this path, and its typed-waveform sidecar beside it, with extension
@.json@. Creates the directory.

Both files go through a temporary file and a rename, and the SIDECAR LANDS
FIRST. The temporary files get the default permissions (the umask), not the
owner-only ones of 'openTempFile', so a waveform is as readable as any other
file the process writes: if the @.vcd@ is there, its @.json@ is there and complete. (Not that
the two are from the same run: a crash between the renames leaves a new
@.json@ beside the previous run's @.vcd@.) On 'Left' (see 'shownWindow';
also when a file cannot be written) neither is written.
-}
writeFiles :: String -> (Int, Int) -> Recording -> FilePath -> IO (Either String ())
writeFiles comment window0 rec path = case shownWindow rec window0 of
  Left err -> pure (Left err)
  Right window -> ioLeft $ withReader rec $ \records -> do
    createDirectoryIfMissing True dir
    (vTmp, vH) <- openBinaryTempFileWithDefaultPermissions dir (takeFileName path <.> "tmp")
    flip onException (hClose vH >> removePathForcibly vTmp) $ do
      hSetBuffering vH (BlockBuffering Nothing)
      rendered <- renderVCD records comment window rec vH
      hClose vH
      case rendered of
        Left err -> removeFile vTmp >> pure (Left err)
        Right () -> do
          writeSidecar rec path
          renameFile vTmp path
          pure (Right ())
 where
  dir = takeDirectory path

{- | Write the typed-waveform sidecar of the waveform at this path: beside
it, with extension @.json@, through a temporary file and a rename.
-}
writeSidecar :: Recording -> FilePath -> IO ()
writeSidecar rec path = do
  meta <- sidecar [(vcdPath n, s) | (n, s) <- namedSignals rec] (recordingLuts rec)
  (jTmp, jH) <-
    openBinaryTempFileWithDefaultPermissions dir (takeFileName sidecarPath <.> "tmp")
  flip onException (hClose jH >> removePathForcibly jTmp) $ do
    ByteStringLazy.hPut jH (Json.encode meta)
    hClose jH
    renameFile jTmp sidecarPath
 where
  dir = takeDirectory path
  sidecarPath = path -<.> "json"

{- | The part of a window @[start, stop)@, in units of the recording's
'timescale', that a waveform shows: it ends where the simulation stopped, at
the latest. 'Left', with the reason, when it shows nothing.
-}
shownWindow :: Recording -> (Int, Int) -> Either String (Int, Int)
shownWindow rec (start, stop)
  | null (recordingSignals rec) = Left "nothing was traced or probed in this run"
  | end <= 0 = Left "the run evaluated no cycle"
  | start < 0 || stop <= start = Left ("the window " <> shown <> " is empty")
  | start >= end =
      Left
        ( "the window "
            <> shown
            <> " starts after the end of the run, at "
            <> show end
            <> " (in units of the timescale, "
            <> show (timescale rec)
            <> " ps)"
        )
  | otherwise = Right (start, min stop end)
 where
  end = ticks rec (recordingEndPs rec)
  shown = "[" <> show start <> ", " <> show stop <> ")"

-- | An action's 'IOException' as a 'Left'.
ioLeft :: IO (Either String a) -> IO (Either String a)
ioLeft act = either (\(e :: IOException) -> Left (displayException e)) id <$> try act

{- | Every signal of a recording with its name in the VCD: its key with the
instance tags resolved by 'disambiguate'. In name order.
-}
namedSignals :: Recording -> [(String, RecordedSignal)]
namedSignals rec =
  Map.toList (disambiguate (Map.fromList [(signalKey s, s) | s <- recordingSignals rec]))

{- | Write the VCD of a recording over the window @[start, stop)@, in units
of its timescale (for a single domain: cycles), to the handle. The records
are read through the 'Reader' (see "Clash.CircuitContext.Internal.Recorder").
The window must be one 'shownWindow' gives. The comment goes in the
@$comment@ line. The last timestamp is @stop@, so that a viewer shows the
last cycle whole, as in the FST.

'Left', and nothing written, when there is no signal, or when a name has a
character VCD does not allow (only printable ASCII is).
-}
renderVCD ::
  Reader -> String -> (Int, Int) -> Recording -> Handle -> IO (Either String ())
renderVCD records comment (start, stop) rec out
  | null named = pure (Left "nothing was traced or probed in this run")
  | nm : _ <- filter (any (not . printable)) (map fst named) =
      pure
        ( Left
            ( "the name '"
                <> nm
                <> "' has characters other than printable ASCII, which VCD does not allow"
            )
        )
  | otherwise = do
      aliasOf <- aliasMap records [(n, s) | (n, s) <- named, signalKind s == TraceKind]
      now <- getCurrentTime
      let
        emitted =
          sortOn
            (\(n, s) -> (signalPeriod s, Down n))
            [(n, s) | (n, s) <- named, Map.notMember n aliasOf]
        labelled = zip labels emitted
        -- Each alias is declared right after its representative, last name first.
        aliasesFor = Map.fromListWith (++) [(r, [a]) | (a, r) <- Map.toList aliasOf]
        vars =
          sortOn
            sourceOrder'
            ( concat
                [ (n, signalWidth s, l) : [(a, signalWidth s, l) | a <- Map.findWithDefault [] n aliasesFor]
                | (l, (n, s)) <- labelled
                ]
            )
      streams <- mapM (\(l, (_, s)) -> (,,) l s <$> readChanges records s) labelled
      let
        starts = [(l, s, startAt Nothing (events s cs)) | (l, s, cs) <- streams]
      Builder.hPutBuilder out $
        header (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now)
          <> renderScopes (List.foldl' (flip insertVar) emptyScope vars)
          <> "$enddefinitions $end\n#"
          <> Builder.intDec start
          <> "\n$dumpvars\n"
          <> foldMap (\(l, s, (v, _)) -> valueLine s l v) starts
          <> "$end\n"
          <> merge
            ( Map.fromList
                [ ((t, i), (v, l, s, mv, rest))
                | (i, (l, s, (v, (t, mv) : rest))) <- zip [0 :: Int ..] starts
                , t < stop
                ]
            )
          <> "#"
          <> Builder.intDec stop
          <> "\n"
      pure (Right ())
 where
  named = namedSignals rec

  -- 'insertVar' appends, so sorting all vars first sorts every scope.
  sourceOrder' (n, _, _) = sourceOrder named n
  scale = timescale rec

  header date =
    "$date "
      <> Builder.string7 date
      <> " $end\n$version Generated by Clash (CircuitContext hierarchical dump) $end\n$comment "
      <> Builder.string7 comment
      <> " $end\n$timescale "
      <> Builder.intDec scale
      <> "ps $end\n"

  -- A signal's records as (time, value), then 'Nothing' where it ends.
  events s cs =
    [(c * k, mv) | Change c mv <- cs] ++ [(signalEnd s * k, Nothing)]
   where
    k = signalPeriod s `div` scale

  -- The value at @start@, and what comes after it.
  startAt _ ((t, mv) : rest) | t <= start = startAt mv rest
  startAt v rest = (v, rest)

  -- The changes of all signals, by time, then by identifier code: a k-way
  -- merge of their streams, each read as it goes.
  merge = go Nothing
   where
    go lastT q = case Map.minViewWithKey q of
      Nothing -> mempty
      Just (((t, i), (cur, l, s, mv, rest)), q') ->
        let
          next = case rest of
            (t2, mv2) : rest2 | t2 < stop -> Map.insert (t2, i) (mv, l, s, mv2, rest2) q'
            _ -> q'
         in
          if mv == cur
            then go lastT next
            else
              (if lastT == Just t then mempty else "#" <> Builder.intDec t <> "\n")
                <> valueLine s l mv
                <> go (Just t) next

{- | The order a scope declares its signals in, as a sort key for a name of
'namedSignals': the order they are written in the design, by the call site
of their recorder (for a plugin-made trace or probe, where the binding is
written). The fields of a record share the call site of the record, and
follow in declaration order (their 'P' positions, see 'parseLoc'). Signals
with one call site and no position (the elements of a @Vec@) or with none
follow in 'naturalKey' order of their names.
-}
sourceOrder ::
  [(String, RecordedSignal)] ->
  String ->
  (Bool, Maybe ([(Word, Int, Int, Int)], [Int]), ([Either Integer String], String))
sourceOrder named = \n ->
  let site = Map.findWithDefault Nothing n sites
   in (isNothing site, site, naturalKey (last (splitPath n)))
 where
  sites = Map.fromList [(n, siteOf (signalKey s)) | (n, s) <- named]
  siteOf raw = let (_, _, loc) = parseSeg (last (splitPath raw)) in parseLoc loc

-- | One value line: @0!@ for one bit, @b0x1 !@ for more.
valueLine :: RecordedSignal -> String -> Maybe Value -> Builder.Builder
valueLine s l mv = case signalWidth s of
  1 -> Builder.byteString (bits 1 mv) <> Builder.string7 l <> "\n"
  w -> "b" <> Builder.byteString (bits w mv) <> " " <> Builder.string7 l <> "\n"

-- | Identifier codes, as stock @dumpVCD@ makes them: @!@ to @~@, then @!!@, @!"@, …
labels :: [String]
labels = concatMap (\s -> map (\c -> s ++ [c]) ['!' .. '~']) ([] : labels)

printable :: Char -> Bool
printable c = 33 <= ord c && ord c <= 126

{- | Traces with the same history, as alias → representative (by VCD name).
A VCD declares every name but writes the values once, under one identifier
code.

The same electrical net is auto-traced at every level it passes through: an
interconnect's vector element, a component's port, a composite half, an
internal binding. In a bittide firmware trace that was THIRTEEN names for one
72-bit bus, and 67% of all value-change bytes were such copies.

Grouping is by what was recorded, not by object identity: tracing rebuilds
every stream it taps, so no runtime identity can see that two traces are one
net. Signals are grouped by width, period, end and the hash of their records,
and a group is then split by comparing the records themselves, so a hash
collision cannot merge two signals. Comparing reads only the signals that
share a hash, which are nearly always real copies. The representative is the
signal with the smallest key, so the result does not depend on the order of
registration. Probes are not aliased.
-}
aliasMap :: Reader -> [(String, RecordedSignal)] -> IO (Map.Map String String)
aliasMap records traces = do
  groups <- concat <$> mapM classes (Map.elems buckets)
  pure (Map.fromList [(a, r) | (r, _) : as <- groups, (a, _) <- as])
 where
  buckets =
    Map.fromListWith
      (flip (++))
      [ ((signalWidth s, signalPeriod s, signalEnd s, signalHash s), [(n, s)])
      | (n, s) <- sortOn (signalKey . snd) traces
      ]
  classes [] = pure []
  classes (x : rest) = do
    same <- mapM (\y -> (,) y <$> sameHistory (snd x) (snd y)) rest
    ((x : [y | (y, True) <- same]) :) <$> classes [y | (y, False) <- same]
  sameHistory a b = do
    as <- readChanges records a
    bs <- readChanges records b
    pure $! as == bs

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

{- | A path segment: name, instance ordinal (identity key) and encoded call
site (design-side ordering key), where tagged.
-}
type Seg = (String, Maybe Int, String)

{- | Split @name\@ordinal\@loc@ into its fields; untagged segments pass
through.
-}
parseSeg :: String -> Seg
parseSeg s = case splitOn '@' s of
  [nm, ds] | isOrd ds -> (nm, Just (read ds), "")
  [nm, ds, loc] | isOrd ds -> (nm, Just (read ds), loc)
  _ -> (s, Nothing, "")
 where
  isOrd ds = not (null ds) && all isDigit ds

{- | Parse an encoded call site into a sort key: one or more call sites
@L\<line\>C\<col\>F\<hash\>@, each maybe followed by @R\<n\>@ when it was
reached @n@ times in a row (the calls that led to the recorder, see
'Clash.CircuitContext.Core.callPath', then the recorder's own), each as
(file hash, line, col, times), then the record field positions @P\<i\>@
(see 'Clash.CircuitContext.Core.fieldLoc').
-}
parseLoc :: String -> Maybe ([(Word, Int, Int, Int)], [Int])
parseLoc s0 = case sites s0 of
  Just (ss@(_ : _), rest) -> (,) ss <$> fields rest
  _ -> Nothing
 where
  sites ('L' : s1)
    | (l@(_ : _), 'C' : s2) <- span isDigit s1
    , (c@(_ : _), 'F' : s3) <- span isDigit s2
    , (f@(_ : _), s4) <- span isDigit s3 = do
        let (n, s5) = case s4 of
              'R' : s6 | (r@(_ : _), s7) <- span isDigit s6 -> (read r, s7)
              _ -> (1, s4)
        (ss, rest) <- sites s5
        Just ((read f, read l, read c, n) : ss, rest)
    | otherwise = Nothing
  sites s = Just ([], s)
  fields "" = Just []
  fields ('P' : s)
    | (p@(_ : _), rest) <- span isDigit s = (read p :) <$> fields rest
  fields _ = Nothing

{- | Strip instance tags from all keys. Groups siblings by (parent, name): a
name with a single instance keeps its clean name; a name with several gets
@name_i@ suffixes ordered by instantiation call site — a DESIGN property —
falling back to first-registration order only for instances born at the
same call site (bare 'fmap' replication; use
'Clash.CircuitContext.Core.imapComponents' instead).
-}
disambiguate :: forall v. Map.Map String v -> Map.Map String v
disambiguate m =
  Map.fromList
    [ (intercalate "." path, v)
    | (path, v) <- go [(map parseSeg (splitPath k), v) | (k, v) <- Map.toList m]
    ]
 where
  go :: [([Seg], v)] -> [([String], v)]
  go entries =
    concat
      [ [([name'], v) | ([], v) <- grp]
          ++ [(name' : p, v) | (p, v) <- go [(t, v) | (t@(_ : _), v) <- grp]]
      | (name, byOrd) <- Map.toAscList grouped
      , let idents = sortOn identKey (Map.toAscList byOrd)
      , let n = length idents
      , (i, (_ord, (_loc, grp))) <- zip [(0 :: Int) ..] idents
      , let name' = if n <= 1 then name else name <> "_" <> show i
      ]
   where
    -- Call site first (design order); located instances before unlocated;
    -- registration ordinal as the last resort.
    identKey (ord', (loc, _)) =
      let pl = parseLoc loc in (isNothing pl, pl, ord')

    grouped :: Map.Map String (Map.Map (Maybe Int) (String, [([Seg], v)]))
    grouped =
      Map.fromListWith
        (Map.unionWith (\(l, xs) (_, ys) -> (l, xs <> ys)))
        [ (sn, Map.singleton ord' (loc, [(rest, v)]))
        | ((sn, ord', loc) : rest, v) <- entries
        ]

{- | The full scope path a signal is declared under, given its name from
'disambiguate'. A dotted name is already that path, since each segment
becomes a nested @$scope@. A dot-free name has no scope of its own, so it
lands in the stock @logic@ scope, as in 'Clash.Signal.Trace.dumpVCD'.

Anything that names VCD vars from outside the file (the typed-waveform
sidecar) must use this, not the bare name: a viewer looks a var up by its
full path.
-}
vcdPath :: String -> String
vcdPath key
  | '.' `elem` key = key
  | otherwise = rootScope <> "." <> key

-- | The scope dot-free names are declared under; see 'vcdPath'.
rootScope :: String
rootScope = "logic"

splitPath :: String -> [String]
splitPath = splitOn '.'

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, _ : b) -> a : splitOn c b
  (a, []) -> [a]

--------------------------------------------------------------------------------
-- Scopes
--------------------------------------------------------------------------------

-- | A declared var: its leaf name, width and identifier code.
type Var = (String, Int, String)

-- | Child scopes and the vars declared directly at this level.
data ScopeTree = ScopeTree (Map.Map String ScopeTree) [Var]

emptyScope :: ScopeTree
emptyScope = ScopeTree Map.empty []

-- | Insert a var along its dot-separated path; the leaf names it in its scope.
insertVar :: Var -> ScopeTree -> ScopeTree
insertVar (path, w, l) = go (splitPath path)
 where
  go [] t = t
  go [leaf] (ScopeTree cs vs) = ScopeTree cs (vs ++ [(leaf, w, l)])
  go (seg : rest) (ScopeTree cs vs) =
    ScopeTree (Map.alter (Just . go rest . fromMaybe emptyScope) seg cs) vs

{- | The declarations. Dot-free vars keep the stock @logic@ scope ('vcdPath'
states the same rule for anything that names them from outside); each
top-level path segment becomes its own top scope.
-}
renderScopes :: ScopeTree -> Builder.Builder
renderScopes (ScopeTree cs vs) =
  (if null vs then mempty else scope 0 rootScope (ScopeTree Map.empty vs))
    <> foldMap (uncurry (scope 0)) (inOrder cs)
 where
  scope :: Int -> String -> ScopeTree -> Builder.Builder
  scope depth nm (ScopeTree cs' vs') =
    indent depth
      <> "$scope module "
      <> Builder.string7 nm
      <> " $end\n"
      <> foldMap (var (depth + 1)) vs'
      <> foldMap (uncurry (scope (depth + 1))) (inOrder cs')
      <> indent depth
      <> "$upscope $end\n"
  var depth (leaf, w, l) =
    indent depth
      <> "$var wire "
      <> Builder.intDec w
      <> " "
      <> Builder.string7 l
      <> " "
      <> Builder.string7 leaf
      <> " $end\n"
  indent depth = foldMap (const "  ") [1 .. depth]
  -- Child scopes in 'naturalKey' order.
  inOrder = sortOn (naturalKey . fst) . Map.toList

{- | A sort key for a name that compares runs of digits as numbers, so that
@lane_2@ comes before @lane_10@. The name itself is the last component, so
that names equal under the numeric comparison (@a01@, @a1@) still have one
order.
-}
naturalKey :: String -> ([Either Integer String], String)
naturalKey name = (go name, name)
 where
  go [] = []
  go str@(c : _)
    | isDigit c, (ds, rest) <- span isDigit str = Left (read ds) : go rest
    | otherwise, (cs, rest) <- break isDigit str = Right cs : go rest
