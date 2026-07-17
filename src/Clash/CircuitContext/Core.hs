{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Scoped simulation context: hierarchy, per-simulation trace map, and
probes inside mealy machines — all layered on UNMODIFIED clash-prelude.

Everything hangs off one implicit parameter @?circuitContext@:

* 'component' pushes a hierarchy segment; trace/probe names are qualified by
  the path where they are created.
* 'traceSignalC' is 'Clash.Signal.Trace.traceSignal' against the context's
  own trace map instead of the global 'traceMap#' — possible without touching
  clash-prelude because the stock internals ('traceSignal#', 'dumpVCD#', …)
  are already parameterized over an @IORef TraceMap@; only the public
  wrappers hard-code the global one.
* 'mealyProbed' + 'probe' record values of expressions INSIDE a mealy step
  function. The step function has no cycle identity of its own, so the
  wrapper derives one from a companion counter register and binds a
  per-application @?probe@ context; writes are keyed by (name, cycle) and
  hence idempotent — safe under lazy re-ordering and speculative sparks, and
  an expression that is never forced simply records nothing.

Tracing is optional by construction: with 'noCircuitContext' (both refs 'Nothing')
every combinator collapses to identity.

Multiple instances of the same circuit in the same component are all
recorded. Instance identity is HEAP identity: each 'component' (and
'mealyProbed') application allocates a fresh hierarchy cell, which is
resolved to a small ordinal at registration time via its 'StableName' — no
global state and no id minting in pure code. Map keys carry
@name\@ordinal\@loc@ tags (the @\'\@\'@ character is reserved — don't use it
in names); 'dumpVCDC' strips them and, where siblings genuinely collide,
renames them @name_0@, @name_1@, … DESIGN-deterministically: ordered by
instantiation call site, never by evaluation order. Instances born at the
same call site (bare 'fmap' replication) can't be design-ordered — name
those by structural position with 'imapComponents' (or explicit 'component'
names).
-}
module Clash.CircuitContext.Core (
  -- * Context
  CircuitContext (..),
  HasCircuitContext,
  noCircuitContext,
  withCircuitContext,

  -- * Hierarchy
  HierSeg (..),
  component,
  imapComponents,
  qualifyName,

  -- * Scoped signal tracing
  traceSignalC,
  dumpVCDC,

  -- * Probes inside mealy machines
  ProbeMap,
  ProbeCtx (..),
  HasProbe,
  probe,
  mealyProbed,
) where

import Clash.Explicit.Signal (delay, register)
import Clash.Prelude (
  BitPack (..),
  Clock,
  Enable,
  Index,
  KnownDomain,
  KnownNat,
  Reset,
  Signal,
  SNat (..),
  Vec,
  enableGen,
  imap,
  snatToNum,
  unbundle,
 )
import Clash.Signal.Internal (knownDomain, SDomainConfiguration (..))
import Clash.Signal.Trace (
  DeclarationCommand (..),
  SimulationCommand (..),
  TraceMap,
  Value,
  ValueChange (..),
  Var (..),
  VCDFile (..),
  dumpVCD1#,
  traceSignal#,
 )
import Clash.Sized.Internal.BitVector (BitVector (BV))
import Clash.XException (NFDataX, isX)

import Data.Bits (testBit)
import qualified Data.ByteString.Lazy as ByteStringLazy
import Data.Char (isDigit)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl', intercalate, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import GHC.Stack (
  CallStack,
  HasCallStack,
  callStack,
  getCallStack,
  srcLocFile,
  srcLocStartCol,
  srcLocStartLine,
 )
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import GHC.Natural (Natural)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, hashStableName, makeStableName)

-- | name → (clock period in ps, bit width, cycle → packed value)
type ProbeMap = Map.Map String (Int, Int, IntMap.IntMap Value)

{- | One hierarchy level: user name plus the encoded source location of the
instantiation. The location is design information — colliding sibling
instances are ordered by it, so the @_0@/@_1@ names depend only on the
design, never on evaluation order.
-}
data HierSeg = HierSeg
  { hsName :: String
  , hsLoc :: String
  -- ^ Encoded call site (@L\<line\>C\<col\>F\<file-hash\>@), or @""@.
  }

data CircuitContext = CircuitContext
  { ccHier :: [HierSeg]
  -- ^ Hierarchy path, innermost first. Each 'component' application
  -- allocates a fresh list cell here; that heap object is the component
  -- INSTANCE's identity (see 'ordinalFor').
  , ccTracer :: Maybe (IORef TraceMap)
  -- ^ 'Nothing' disables signal tracing
  , ccProbes :: Maybe (IORef ProbeMap)
  -- ^ 'Nothing' disables mealy probes
  , ccOrdinals :: Maybe (IORef OrdinalMap)
  -- ^ Per-simulation instance-ordinal registry; 'Nothing' disables
  -- instance tagging (fine while nothing is recorded)
  }

type HasCircuitContext = ?circuitContext :: CircuitContext

-- | Tracing and probing disabled; hierarchy still works.
noCircuitContext :: CircuitContext
noCircuitContext = CircuitContext [] Nothing Nothing Nothing

--------------------------------------------------------------------------------
-- Instance identities
--------------------------------------------------------------------------------

{- | Per-simulation instance-ordinal registry: next free ordinal, plus a
'StableName'-keyed table (bucketed by hash) mapping hierarchy-path heap
objects to their ordinal. No global state: the registry is created by
'withCircuitContext' and all allocation happens inside registration IO.
-}
type OrdinalMap = (Int, IntMap.IntMap [(StableName [HierSeg], Int)])

emptyOrdinals :: OrdinalMap
emptyOrdinals = (0, IntMap.empty)

{- | The ordinal of the instance identified by this hierarchy-path object,
assigning the next free one on first sight. Identity is HEAP identity: every
'component' application allocates a fresh path cell, so distinct instances
resolve to distinct ordinals — while a circuit expression the compiler
shared really is one circuit, and correctly counts as one instance. The
ordinal is only an identity key; NAMING order comes from the call site.
-}
ordinalFor :: IORef OrdinalMap -> [HierSeg] -> IO Int
ordinalFor ref path = do
  sn <- path `seq` makeStableName path
  atomicModifyIORef' ref $ \om@(next, m) ->
    let h = hashStableName sn
        bucket = IntMap.findWithDefault [] h m
     in case lookup sn bucket of
          Just i -> (om, i)
          Nothing -> ((next + 1, IntMap.insert h ((sn, next) : bucket) m), next)

-- | A registration-unique ordinal (used for trace leaves).
freshOrdinal :: IORef OrdinalMap -> IO Int
freshOrdinal ref = atomicModifyIORef' ref (\(next, m) -> ((next + 1, m), next))

-- | Resolve a hierarchy (innermost first) to @name\@ordinal\@loc@ segments.
resolveHier :: IORef OrdinalMap -> [HierSeg] -> IO [String]
resolveHier ref = go
 where
  go [] = pure []
  go l@(HierSeg nm loc : rest) =
    (:) <$> (taggedLoc nm loc <$> ordinalFor ref l) <*> go rest

-- | Tag a name with an instance ordinal (and call site, if known):
-- @name\@i\@loc@. The @\'\@\'@ character is reserved for this; don't use it
-- in component/trace/probe names.
taggedLoc :: String -> String -> Int -> String
taggedLoc nm loc i =
  nm <> "@" <> show i <> (if null loc then "" else "@" <> loc)

{- | Encode the nearest call site as a design-side sort key, using only
characters safe inside hierarchy segments (no dots, no \@): line, column and
a file hash. Empty when no call stack is available.
-}
callLoc :: CallStack -> String
callLoc cs = case getCallStack cs of
  (_, l) : _ ->
    "L"
      <> show (srcLocStartLine l)
      <> "C"
      <> show (srcLocStartCol l)
      <> "F"
      <> show (fileHash (srcLocFile l))
  [] -> ""
 where
  fileHash :: String -> Word
  fileHash = foldl' (\h c -> h * 31 + fromIntegral (fromEnum c)) 5381

{- | Run a simulation action under a fresh context and return whatever it
produced together with the collected traces and probes. The action must
/force/ the parts of the circuit it wants traced (sampling does), exactly as
with the stock global-map API.
-}
withCircuitContext :: (HasCircuitContext => IO r) -> IO (r, TraceMap, ProbeMap)
withCircuitContext k = do
  tRef <- newIORef Map.empty
  pRef <- newIORef Map.empty
  oRef <- newIORef emptyOrdinals
  r <- let ?circuitContext = CircuitContext [] (Just tRef) (Just pRef) (Just oRef) in k
  (,,) r <$> readIORef tRef <*> readIORef pRef

{- | @component "fifo" circuit@: everything traced or probed inside
@circuit@ is qualified by @…fifo.@.

Every application is a distinct /instance/: it allocates a fresh hierarchy
cell whose heap identity distinguishes it from every other instance of the
same name (resolved to ordinals at registration, see 'ordinalFor'), so
multiple instances of the same (sub)circuit under one parent never collide —
all are recorded, and 'dumpVCDC' disambiguates downstream (@fifo_0@,
@fifo_1@, …). OPAQUE so the cell is allocated per application rather than
floated and shared.
-}
component :: (HasCallStack, HasCircuitContext) => String -> (HasCircuitContext => r) -> r
component nm k = let ?circuitContext = ctx' in k
 where
  ctx0 = ?circuitContext
  ctx' = ctx0{ccHier = HierSeg nm (callLoc callStack) : ccHier ctx0}
{-# OPAQUE component #-}

{- | Replicate a named instance by structural position: element @i@ of the
vector is built under component @name_i@. Use this (or explicit 'component'
names) instead of bare 'fmap'\/'traverse'\/'Data.Foldable' combinators when
replicating a traced circuit: position is design information that the
functorial APIs cannot carry, so it has to be threaded explicitly.
-}
imapComponents ::
  forall n a b.
  (HasCircuitContext, KnownNat n) =>
  String ->
  (HasCircuitContext => Index n -> a -> b) ->
  Vec n a ->
  Vec n b
imapComponents nm f = imap (\i x -> component (nm <> "_" <> show i) (f i x))

-- | The fully qualified name for @nm@ at the current hierarchy.
qualifyName :: CircuitContext -> String -> String
qualifyName ctx nm = intercalate "." (reverse (nm : map hsName (ccHier ctx)))

{- | 'Clash.Signal.Trace.traceSignal' against the scoped map, qualified by
the current hierarchy. Identity when tracing is off. Registration happens —
as with the stock function — when the returned signal is first forced.

Every registration gets its own instance id, so re-using a trace name (e.g.
by applying the same traced subcircuit twice in one 'component') records all
instances instead of erroring; 'dumpVCDC' disambiguates downstream.
-}
traceSignalC ::
  forall dom a.
  (HasCallStack, HasCircuitContext, KnownDomain dom, BitPack a, NFDataX a, Typeable a) =>
  String ->
  Signal dom a ->
  Signal dom a
traceSignalC nm sig = case (ccTracer ctx, ccOrdinals ctx) of
  (Just ref, Just ordRef) ->
    -- The only unsafePerformIO in the design: lazy one-shot registration,
    -- exactly like stock 'Clash.Signal.Trace.traceSignal1'. Instance
    -- ordinals are resolved inside this same action — no id minting in
    -- pure code.
    unsafePerformIO $ do
      segs <- resolveHier ordRef (ccHier ctx)
      leafI <- freshOrdinal ordRef
      let key = intercalate "." (reverse (taggedLoc nm loc leafI : segs))
      traceSignal# ref period key sig
  _ -> sig
 where
  ctx = ?circuitContext
  loc = callLoc callStack
  period :: Int
  period = case knownDomain @dom of
    SDomainConfiguration{sPeriod} -> snatToNum sPeriod
{-# OPAQUE traceSignalC #-}

{- | Dump the context's traces AND probes to VCD text over
@(offset, nSamples)@.

Unlike the stock 'Clash.Signal.Trace.dumpVCD', which flattens every trace
into a single @$scope module logic@ block (the dots in a qualified name are
then just characters in a wire name), this renders the dotted paths produced
by 'component' as properly nested @$scope module … $upscope@ blocks, so
viewers show the design hierarchy.

Probes are merged in as ordinary wires: a cycle where the probed expression
was never forced shows as @x@.

Downstream disambiguation: map keys carry per-instance @\@uid@ tags (see
'component', 'traceSignalC', 'probe'). The tags are stripped here; where
multiple instances share a (parent, name) the siblings are renamed
@name_0@, @name_1@, … in ascending id (construction) order, so all
instances appear in the VCD with coherent per-instance subtrees.
-}
dumpVCDC :: (Int, Int) -> TraceMap -> ProbeMap -> IO (Either String Text.Text)
dumpVCDC slice@(offset, nSamples) tm pm = do
  now <- getCurrentTime
  pure
    ( renderVCDHier now
        <$> dumpVCD1# slice (disambiguate (Map.union tm (probeTraces pm)))
    )
 where
  -- Synthesize a TraceMap entry per probe. dumpVCD1# transposes the value
  -- lists (ragged lists would misalign columns), so densify to exactly the
  -- requested window, X-filling cycles that recorded nothing. The TypeRep
  -- field only matters for replay, which synthesized entries don't support.
  probeTraces = Map.map toTrace
  toTrace (per, w, vs) =
    ( ByteStringLazy.empty
    , per
    , w
    , [ IntMap.findWithDefault (xVal w) cyc vs
      | cyc <- [0 .. offset + nSamples - 1]
      ]
    )
  xVal w = ((2 :: Natural) ^ w - 1, 0)

--------------------------------------------------------------------------------
-- Downstream disambiguation of instance ids
--------------------------------------------------------------------------------

-- | A path segment: name, instance ordinal (identity key) and encoded call
-- site (design-side ordering key), where tagged.
type Seg = (String, Maybe Int, String)

-- | Split @name\@ordinal\@loc@ into its fields; untagged segments pass
-- through.
parseSeg :: String -> Seg
parseSeg s = case splitOn '@' s of
  [nm, ds] | isOrd ds -> (nm, Just (read ds), "")
  [nm, ds, loc] | isOrd ds -> (nm, Just (read ds), loc)
  _ -> (s, Nothing, "")
 where
  isOrd ds = not (null ds) && all isDigit ds

-- | Parse an encoded call site @L\<line\>C\<col\>F\<hash\>@ into a sort key
-- (file hash, line, col).
parseLoc :: String -> Maybe (Word, Int, Int)
parseLoc ('L' : s0)
  | (l@(_ : _), 'C' : s1) <- span isDigit s0
  , (c@(_ : _), 'F' : f@(_ : _)) <- span isDigit s1
  , all isDigit f =
      Just (read f, read l, read c)
parseLoc _ = Nothing

{- | Strip instance tags from all keys. Groups siblings by (parent, name): a
name with a single instance keeps its clean name; a name with several gets
@name_i@ suffixes ordered by instantiation call site — a DESIGN property —
falling back to first-registration order only for instances born at the
same call site (bare 'fmap' replication; use 'imapComponents' instead).
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
    identKey (ord, (loc, _)) =
      let pl = parseLoc loc in (isNothing pl, pl, ord)

    grouped :: Map.Map String (Map.Map (Maybe Int) (String, [([Seg], v)]))
    grouped =
      Map.fromListWith
        (Map.unionWith (\(l, xs) (_, ys) -> (l, xs <> ys)))
        [ (sn, Map.singleton ord (loc, [(rest, v)]))
        | ((sn, ord, loc) : rest, v) <- entries
        ]

--------------------------------------------------------------------------------
-- Hierarchical VCD rendering
--------------------------------------------------------------------------------

-- | Child scopes and the vars declared directly at this level.
data ScopeTree = ScopeTree (Map.Map String ScopeTree) [Var]

emptyScope :: ScopeTree
emptyScope = ScopeTree Map.empty []

-- | Insert a var along its dot-separated reference path; the leaf segment
-- becomes the var's reference inside its scope.
insertVar :: Var -> ScopeTree -> ScopeTree
insertVar v = go (splitPath (varReference v))
 where
  go [] t = t
  go [leaf] (ScopeTree cs vs) = ScopeTree cs (vs ++ [v{varReference = leaf}])
  go (seg : rest) (ScopeTree cs vs) =
    ScopeTree (Map.alter (Just . go rest . fromMaybe emptyScope) seg cs) vs

splitPath :: String -> [String]
splitPath = splitOn '.'

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, _ : b) -> a : splitOn c b
  (a, []) -> [a]

-- | Render like 'Clash.Signal.Trace.dumpVCD0#', but with nested scopes.
renderVCDHier :: UTCTime -> VCDFile -> Text.Text
renderVCDHier now (VCDFile decCmds simCmds) =
  Text.unlines $
    [ "$date " <> Text.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now) <> " $end"
    , "$version Generated by Clash (CircuitContext hierarchical dump) $end"
    , "$comment No comment $end"
    ]
      ++ concatMap renderDec decCmds
      ++ "$enddefinitions $end"
      : renderSims simCmds
 where
  renderDec (TimeScale t u) =
    ["$timescale " <> Text.pack (shows t (show u)) <> " $end"]
  renderDec (Vars vs) =
    renderRoot (foldl' (flip insertVar) emptyScope vs)

  -- Unqualified (dot-free) vars keep the stock @logic@ scope; each
  -- top-level path segment becomes its own top scope.
  renderRoot (ScopeTree cs vs) =
    (if null vs then [] else renderScope 0 "logic" (ScopeTree Map.empty vs))
      ++ concat [renderScope 0 nm t | (nm, t) <- Map.toList cs]

  renderScope depth nm (ScopeTree cs vs) =
    [indent depth <> "$scope module " <> Text.pack nm <> " $end"]
      ++ map (renderVar (depth + 1)) vs
      ++ concat [renderScope (depth + 1) c t | (c, t) <- Map.toList cs]
      ++ [indent depth <> "$upscope $end"]

  renderVar depth v =
    indent depth
      <> Text.pack
        (unwords ["$var wire", show (varSize v), varIDCode v, varReference v, "$end"])

  indent depth = Text.replicate depth "  "

  -- dumpVCD1# stamps the transition INTO sample k+1 with time k, colliding
  -- with $dumpvars (sample 0) at #0 and hiding every signal's first value.
  -- Shift the timestamps after $dumpvars so the transition into sample k
  -- lands at #k.
  renderSims = go False
   where
    go _ [] = []
    go _ (DumpVars vars : cmds) =
      "$dumpvars" : map renderVC vars ++ "$end" : go True cmds
    go dumped (SimulationTime t : cmds) =
      Text.pack ('#' : show (if dumped then t + 1 else t)) : go dumped cmds
    go dumped (SimulationValueChange vc : cmds) =
      renderVC vc : go dumped cmds

  renderVC (ValueChange 1 idCode (0, 0)) = Text.pack ('0' : idCode)
  renderVC (ValueChange 1 idCode (0, 1)) = Text.pack ('1' : idCode)
  renderVC (ValueChange 1 idCode (1, _)) = Text.pack ('x' : idCode)
  renderVC (ValueChange 1 idCode v) =
    error ("dumpVCDC: bad 1-bit value " <> show v <> " for " <> show idCode)
  renderVC (ValueChange w idCode (mask, val)) =
    Text.pack ('b' : map digit (reverse [0 .. w - 1]) ++ ' ' : idCode)
   where
    digit d = case (testBit mask d, testBit val d) of
      (False, False) -> '0'
      (False, True) -> '1'
      (True, _) -> 'x'

--------------------------------------------------------------------------------
-- Probes inside mealy machines
--------------------------------------------------------------------------------

data ProbeCtx = ProbeCtx
  { pcCtx :: CircuitContext
  , pcPeriod :: !Int
  -- ^ Clock period (ps) of the probed mealy's domain; carried so probe
  -- values can be dumped to VCD alongside traced signals.
  , pcPath :: [HierSeg]
  -- ^ Identity object of the 'mealyProbed' application (a per-application
  -- hierarchy cell, carrying the mealy's call site): probes from distinct
  -- instances of the same mealy stay distinct even under identical names.
  , pcCycle :: !Int
  }

type HasProbe = ?probe :: ProbeCtx

{- | Record the value of any expression inside a probed step function:

> f :: HasProbe => Int -> Int -> (Int, Int)
> f acc i = let acc' = probe "acc" (acc + i) in (acc', acc)

Returns its argument. The write happens when the expression is forced, keyed
by (qualified name, cycle): idempotent under re-evaluation and sparks; never
forced → never recorded. An X value is stored as an all-undefined (masked)
entry rather than an exception.
-}
probe :: forall a. (HasProbe, BitPack a, NFDataX a) => String -> a -> a
probe nm a = case (ccProbes ctx, ccOrdinals ctx) of
  (Just ref, Just ordRef) ->
    unsafePerformIO $ do
      segs <- resolveHier ordRef (ccHier ctx)
      mI <- ordinalFor ordRef (pcPath ?probe)
      let qual = intercalate "." (reverse (taggedLoc nm mLoc mI : segs))
      atomicModifyIORef' ref $ \m ->
        (Map.insertWith merge qual (per, w, IntMap.singleton cyc val) m, ())
      pure a
  _ -> a
 where
  ctx = pcCtx ?probe
  mLoc = case pcPath ?probe of
    HierSeg _ loc : _ -> loc
    [] -> ""
  per = pcPeriod ?probe
  cyc = pcCycle ?probe
  w = snatToNum (SNat @(BitSize a))
  val = case isX (pack a) of
    Right (BV mask v) -> (mask, v)
    Left _ -> (fullMask, 0)
  fullMask = (2 :: Natural) ^ w - 1
  merge (_, _, new) (p0, w0, old) = (p0, w0, IntMap.union old new)
{-# NOINLINE probe #-}

{- | Stock 'Clash.Explicit.Mealy.mealy', except the step function may 'probe'
its internals. Implemented purely with stock combinators: a companion counter
register provides the cycle identity, and the step is applied under a
per-cycle @?probe@ binding. One extra register per probed mealy; with probes
disabled the only residue is that (dead) counter.
-}
mealyProbed ::
  forall dom s i o.
  (HasCallStack, HasCircuitContext, KnownDomain dom, NFDataX s) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  (HasProbe => s -> i -> (s, o)) ->
  s ->
  Signal dom i ->
  Signal dom o
mealyProbed clk rst ena f s0 inp = o
 where
  ctx = ?circuitContext
  -- A fresh cell per application: this heap object identifies the mealy
  -- INSTANCE (see 'ordinalFor'); OPAQUE keeps it per-application.
  path = HierSeg "mealyProbed" (callLoc callStack) : ccHier ctx
  period :: Int
  period = case knownDomain @dom of
    SDomainConfiguration{sPeriod} -> snatToNum sPeriod
  -- Deliberately immune to rst and ena: the key must be the SAMPLE index,
  -- not the architectural cycle, or probe values land at the wrong VCD
  -- times whenever reset/enable stalls the state register.
  cnt = delay clk enableGen (0 :: Int) ((+ 1) <$> cnt)
  step n s i = let ?probe = ProbeCtx ctx period path n in f s i
  (s', o) = unbundle (step <$> cnt <*> s <*> inp)
  s = register clk rst ena s0 s'
{-# OPAQUE mealyProbed #-}
