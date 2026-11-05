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
  Origin (..),
  originComment,
  Unshown (..),
  windowShown,
  shownWindow,
  placeWithSidecar,
  ioLeft,
  aliasMap,
  sourceOrder,
  parseSeg,
  parseLoc,
  splitPath,
  naturalKey,
) where

import Control.Exception (IOException, displayException, onException, try)
import Control.Monad (when)
import Data.Char (isDigit, ord)
import Data.List (intercalate, sortOn)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (
  copyFile,
  createDirectoryIfMissing,
  doesFileExist,
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
import Clash.CircuitContext.Internal.Sidecar (Origin (..), sidecar)

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

{- | Write a recording's waveform over a window (see 'renderVCD'), for this
reason: the VCD at this path, and its typed-waveform sidecar beside it, with
extension @.json@. Creates the directory.

Both files go through a temporary file and a rename, and the SIDECAR LANDS
FIRST ('placeWithSidecar'). The temporary files get the default permissions
(the umask), not the owner-only ones of 'openTempFile', so a waveform is as
readable as any other file the process writes: if the @.vcd@ is there, its
@.json@ is there and complete. (Not that the two are from the same run: a
crash between the renames leaves a new @.json@ beside the previous run's
@.vcd@, and a copy of the previous @.json@.) On 'Left' (see 'shownWindow';
also when a file cannot be written) neither is written, and earlier files are
as they were.
-}
writeFiles :: Origin -> (Int, Int) -> Recording -> FilePath -> IO (Either String ())
writeFiles origin window0 rec path = case shownWindow rec window0 of
  Left err -> pure (Left err)
  Right window -> ioLeft $ withReader rec $ \records -> do
    createDirectoryIfMissing True dir
    (vTmp, vH) <- openBinaryTempFileWithDefaultPermissions dir (takeFileName path <.> "tmp")
    flip onException (hClose vH >> removePathForcibly vTmp) $ do
      hSetBuffering vH (BlockBuffering Nothing)
      rendered <- renderVCD records (originComment origin) window rec vH
      hClose vH
      case rendered of
        Left err -> removeFile vTmp >> pure (Left err)
        Right () -> do
          placeWithSidecar origin "vcd" rec vTmp path
          pure (Right ())
 where
  dir = takeDirectory path

{- | Put a waveform in place at this path, from its temporary file, with its
typed-waveform sidecar beside it (extension @.json@), for a waveform written
for this reason, with this extension. The sidecar goes through a temporary
file too, and lands first. Both land or, when this throws, neither: an
earlier sidecar is put back, and the sidecar's temporary file is removed (the
waveform's is the caller's).
-}
placeWithSidecar :: Origin -> String -> Recording -> FilePath -> FilePath -> IO ()
placeWithSidecar origin ext rec wTmp path = do
  meta <-
    sidecar origin ext [(vcdPath n, s) | (n, s) <- namedSignals rec] (recordingLuts rec)
  (jTmp, jH) <-
    openBinaryTempFileWithDefaultPermissions dir (takeFileName sidecarPath <.> "tmp")
  let earlier = jTmp <.> "earlier"
  flip onException (hClose jH >> removePathForcibly jTmp) $ do
    ByteStringLazy.hPut jH (Json.encode meta)
    hClose jH
    -- A copy of the earlier sidecar, to put back if the waveform does not
    -- land. Copied, not moved: the earlier waveform keeps its sidecar
    -- meanwhile.
    hadEarlier <- doesFileExist sidecarPath
    when hadEarlier (copyFile sidecarPath earlier)
    let dropEarlier = when hadEarlier (removePathForcibly earlier)
    renameFile jTmp sidecarPath `onException` dropEarlier
    renameFile wTmp path
      `onException` ( if hadEarlier
                        then renameFile earlier sidecarPath
                        else removePathForcibly sidecarPath
                    )
    dropEarlier
 where
  dir = takeDirectory path
  sidecarPath = path -<.> "json"

{- | The comment of a waveform written for this reason: a 'Failure' says it
shows the run of a failing test.
-}
originComment :: Origin -> String
originComment Requested = "No comment"
originComment Failure = "clash-circuit-context: the run of a failing test"

-- | Why a window shows nothing of a recording.
data Unshown
  = -- | Nothing was traced or probed.
    NothingRecorded
  | -- | The run evaluated no cycle.
    NoCycle
  | -- | The window is empty.
    EmptyWindow
  | -- | The window starts at or after the end of the run, in ps.
    AfterEnd Int

{- | The part of a window @[start, stop)@, in units of the recording's
'timescale', that a waveform shows: it ends where the simulation stopped, at
the latest. 'Left', with the reason, when it shows nothing.
-}
windowShown :: Recording -> (Int, Int) -> Either Unshown (Int, Int)
windowShown rec (start, stop)
  | null (recordingSignals rec) = Left NothingRecorded
  | end <= 0 = Left NoCycle
  | start < 0 || stop <= start = Left EmptyWindow
  | start >= end = Left (AfterEnd (recordingEndPs rec))
  | otherwise = Right (start, min stop end)
 where
  end = ticks rec (recordingEndPs rec)

{- | 'windowShown', with the reason in words, and the window and the end of
the run in units of the timescale.
-}
shownWindow :: Recording -> (Int, Int) -> Either String (Int, Int)
shownWindow rec window@(start, stop) = case windowShown rec window of
  Right inUnits -> Right inUnits
  Left NothingRecorded -> Left "nothing was traced or probed in this run"
  Left NoCycle -> Left "the run evaluated no cycle"
  Left EmptyWindow -> Left ("the window " <> shown <> " is empty")
  Left (AfterEnd endPs) ->
    Left
      ( "the window "
          <> shown
          <> " starts at or after the end of the run, at "
          <> show (ticks rec endPs)
          <> " (in units of the timescale, "
          <> show (timescale rec)
          <> " ps)"
      )
 where
  shown = "[" <> show start <> ", " <> show stop <> ")"

-- | An action's 'IOException' as a 'Left'.
ioLeft :: IO (Either String a) -> IO (Either String a)
ioLeft act = either (\(e :: IOException) -> Left (displayException e)) id <$> try act

{- | Every signal of a recording with its name in the VCD: its key with the
instance tags resolved by 'disambiguate'. In name order.

Two signals can have one key: the key of a probe is its scope, its name, and
the ordinal and call site of the instance it is in ('pcPath'), and two
probe contexts with one 'pcPath' each register their own (see
'Clash.CircuitContext.Core.ProbeCtx'). So the signals go to 'disambiguate'
as a list, never through a map of keys: such signals are two instances of
their name, in registration order.
-}
namedSignals :: Recording -> [(String, RecordedSignal)]
namedSignals rec =
  Map.toList (disambiguate [(signalKey s, s) | s <- recordingSignals rec])

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
'Clash.CircuitContext.Core.imapComponents' instead). A signal is an
instance of its own: two signals with one key are two instances, in the
order of the list.

Every name in a scope is unique, also when a suffix makes a name that the
scope already has (two @x@ beside the elements @x_0@ and @x_1@ of a pair
@x@). The instances of a name get @name_i@ only when none of these names is
in the scope yet; otherwise they get @name__i@ (two underscores), or as many
underscores as it takes. Only a clean name, or a name that another group got
with more underscores, can take such a name: two groups with one underscore
never make the same name, as the digits after the last underscore are the
suffix. The groups of a scope get their names in name order, after all clean
names, so the names depend only on the keys.

The top level has one more name: the dot-free names are declared in the
stock scope @logic@ (see 'vcdPath'), so when there are any, @logic@ is a
name the top level has. A component named @logic@ is then numbered as a
second instance of it (@logic_0@), and the dot-free names keep the scope
that 'Clash.Signal.Trace.dumpVCD' gives them. A dot-free signal named
@logic@ is declared in that scope, not beside it, so it is not numbered.

So no two signals get one name. The map would keep one of them, so that is
an error.
-}
disambiguate :: forall v. [(String, v)] -> Map.Map String v
disambiguate signals =
  Map.fromListWithKey
    (\k _ _ -> error ("clash-circuit-context: two signals are named " <> k <> " (a bug)"))
    [ (intercalate "." path, v)
    | (path, v) <- go rootTaken entries
    ]
 where
  entries = [(map parseSeg (splitPath k), (j, v)) | (j, (k, v)) <- zip [0 :: Int ..] signals]
  -- The stock scope of the dot-free names, when there are any.
  rootTaken = Set.fromList [rootScope | any ((== 1) . length . fst) entries]

  -- A level, with the names it has besides its own (at the top, rootTaken).
  go :: Set.Set String -> [([Seg], (Int, v))] -> [([String], v)]
  go reserved level =
    concat
      [ [([name'], v) | ([], (_, v)) <- grp]
          ++ [(name' : p, v) | (p, v) <- go Set.empty [(t, x) | (t@(_ : _), x) <- grp]]
      | (name', grp) <- concat (snd (List.mapAccumL named (clean <> reserved) instances))
      ]
   where
    -- Each name with the groups of its instances, in identKey order.
    instances =
      [ (name, map snd (sortOn identKey (Map.toAscList byInstance)))
      | (name, byInstance) <- Map.toAscList grouped
      ]
    -- A name keeps itself when it has one instance, and that is not a scope
    -- beside a scope of the same name that the level has.
    keeps (name, [(_, grp)]) = name `Set.notMember` reserved || all (null . fst) grp
    keeps _ = False
    clean = Set.fromList [name | g@(name, _) <- instances, keeps g]

    -- A name's instances with their names, and the names taken after them.
    named taken g@(name, insts)
      | keeps g = (taken, [(name, grp) | (_, grp) <- insts])
      | otherwise = (foldr Set.insert taken names, zip names (map snd insts))
     where
      names = free (1 :: Int)
      free k
        | all (`Set.notMember` taken) ns = ns
        | otherwise = free (k + 1)
       where
        ns = [name <> replicate k '_' <> show i | i <- [0 .. length insts - 1]]

    -- Call site first (design order); located instances before unlocated;
    -- registration ordinal, then the place in the list, as the last resort.
    identKey (inst, (loc, _)) =
      let pl = parseLoc loc in (isNothing pl, pl, inst)

    -- An instance is the ordinal of its segment, and for a signal (the last
    -- segment of a key) also its place in the list.
    grouped ::
      Map.Map String (Map.Map (Maybe Int, Maybe Int) (String, [([Seg], (Int, v))]))
    grouped =
      Map.fromListWith
        (Map.unionWith (\(l, xs) (_, ys) -> (l, xs <> ys)))
        [ (sn, Map.singleton (ord', leaf) (loc, [(rest, (j, v))]))
        | ((sn, ord', loc) : rest, (j, v)) <- level
        , let leaf = if null rest then Just j else Nothing
        ]

{- | The full scope path a signal is declared under, given its name from
'disambiguate'. A dotted name is already that path, since each segment
becomes a nested @$scope@. A dot-free name has no scope of its own, so it
lands in the stock @logic@ scope, as in 'Clash.Signal.Trace.dumpVCD'. No
dotted name starts with that scope when there are dot-free names: a
component named @logic@ is then @logic_0@ (see 'disambiguate').

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
