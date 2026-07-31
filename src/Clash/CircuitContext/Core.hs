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
-- 'knownDomain' is matched via the 'SDomainConfiguration' GADT; without
-- MonoLocalBinds (implied by TypeFamilies) those matches are fragile.
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Scoped simulation context: hierarchy, per-simulation trace map, and
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
  withoutCircuitContext,
  withCircuitContext,
  withCircuitContextWindow,
  withCircuitContextWindowE,
  withCircuitContextWindowM,

  -- * Hierarchy
  HierSeg (..),
  component,
  imapComponents,
  qualifyName,

  -- * Scoped signal tracing
  traceSignalC,
  dumpVCDC,

  -- * Recorded data (change-compressed)
  Run,
  Runs,
  TraceData,
  TraceEntry (..),
  recordedCycles,
  aliasGroups,

  -- * Probes inside mealy machines
  ProbeMap,
  ProbeCtx (..),
  HasProbe,
  probe,
  mealyProbed,
  mealyBProbed,
  mooreProbed,
  mooreBProbed,
  probeFmap,
) where

import Clash.Explicit.Mealy (mealy)
import Clash.Explicit.Moore (moore)
import Clash.Explicit.Signal (delay, register)
import Clash.Magic (clashSimulation)
import Clash.Prelude (
  BitPack (..),
  Bundle (Unbundled, bundle),
  Clock,
  Enable,
  Index,
  KnownDomain,
  KnownNat,
  Reset,
  SNat (..),
  Signal,
  Vec,
  enableGen,
  imap,
  sample_lazy,
  snatToNum,
  unbundle,
 )
import Clash.Signal.Internal (SDomainConfiguration (..), Signal ((:-)), knownDomain)
import Clash.Signal.Trace (
  DeclarationCommand (..),
  SimulationCommand (..),
  VCDFile (..),
  Value,
  ValueChange (..),
  Var (..),
  dumpVCD1#,
 )
import Clash.Sized.Internal.BitVector (BitVector (BV))
import Clash.XException (NFDataX)

import Control.Exception (SomeException, evaluate, throwIO, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits (testBit, xor, (.&.))
import qualified Data.ByteString.Lazy as ByteStringLazy
import Data.Char (isDigit)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl', intercalate, partition, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.Natural (Natural)
import GHC.Stack (
  CallStack,
  HasCallStack,
  callStack,
  getCallStack,
  srcLocFile,
  srcLocStartCol,
  srcLocStartLine,
 )
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)
import System.Mem.StableName (StableName, hashStableName, makeStableName)

{- | A maximal span of consecutive cycles holding one packed value:
@(firstCycle, lastCycle, value)@.
-}
type Run = (Int, Int, Value)

{- | Run-length encoded signal history, NEWEST run first.

This is the central memory-design decision of the recorder: accumulators are
sized by the number of VALUE CHANGES, not by the number of simulated cycles —
the same compression a VCD applies on output. Storing per-cycle history
(a list or map entry per cycle, as earlier versions did) makes memory linear
in simulation length for every traced signal and probe, which for a real
design (dozens of signals, windows of 10⁵–10⁶ cycles) is gigabytes of
mostly-repeated values: a space leak by construction, no matter how strictly
it is forced.
-}
type Runs = [Run]

{- | Record @cycle = v@ into a run history. Recording is monotonic by
construction at every call site — the trace tap walks a list spine in order,
and probes fire in simulation order — so this only ever extends the newest
run or starts a new one; re-recording an already-covered cycle (lazy
re-evaluation) is ignored, keeping writes idempotent. A gap before @cyc@
deliberately starts a NEW run even for an equal value: for probes a gap means
"never forced" and must stay observable (rendered @x@), not be absorbed.
-}
addCycle :: Int -> Value -> Runs -> Runs
addCycle cyc v runs = case runs of
  (s, e, v0) : rest
    | cyc <= e -> runs
    | cyc == e + 1 && v == v0 -> (s, cyc, v0) : rest
  _ -> (cyc, cyc, v) : runs

{- | A run history bounded to a trailing capture window, as a two-chunk gap
buffer: @WRuns flipCycle current previous@, where @current@ covers cycles
@[flipCycle, now]@ and @previous@ the preceding chunk. When @current@ grows
past the window size, @previous@ is dropped and the chunks flip — O(1)
amortized, and the retained history always spans at least the last window's
worth of cycles (at most two windows). This is the logic-analyzer model:
full-fidelity recording of everything, forever, is intrinsically unbounded
(a CPU's program counter changes every cycle — run-length encoding cannot
compress it), so a recorder that must not dominate memory needs a window.
An unlimited recorder is just @window = maxBound@ (the chunks never flip).
-}
data WRuns = WRuns !Int !Runs !Runs

-- | Empty windowed history.
emptyWRuns :: WRuns
emptyWRuns = WRuns 0 [] []

-- | 'addCycle' into a windowed history; @w@ is the window size in cycles.
addCycleW :: Int -> Int -> Value -> WRuns -> WRuns
addCycleW w cyc v (WRuns flipC cur prev)
  | cyc >= flipC + w = WRuns cyc [(cyc, cyc, v)] cur
  | otherwise = WRuns flipC (addCycle cyc v cur) prev

-- | Flatten to a plain (newest-first) run history.
wrunsToRuns :: WRuns -> Runs
wrunsToRuns (WRuns _ cur prev) = cur ++ prev

-- | name → (clock period in ps, bit width, change-compressed history)
type ProbeMap = Map.Map String (Int, Int, Runs)

{- | Live, per-signal trace accumulator. Cycles @[0, 'ttNext')@ have been
consumed (packed and recorded into 'ttRuns'); 'ttRest' is the not-yet-consumed
packed tail of the signal. Keeping the REST rather than the HEAD of the
packed stream is what releases the raw signal history: as the design forces
the traced signal forward, the tap records each cycle's compact packed value
and drops its reference to the consumed prefix, so the raw per-cycle design
values (full records, unevaluated closures) become garbage immediately
instead of being pinned until the dump.
-}
data TraceTap
  = {- | @TraceTap next runs rest@: cycles @[0, next)@ have been consumed
    (packed and recorded into the windowed @runs@); @rest@ is the
    not-yet-consumed packed tail.
    -}
    TraceTap !Int !WRuns [Value]

-- | Live trace registry: name → (period ps, bit width, accumulator).
type LiveTraces = Map.Map String (Int, Int, IORef TraceTap)

-- | Frozen snapshot of one trace, as returned by 'withCircuitContext'.
data TraceEntry = TraceEntry
  { tePeriod :: !Int
  , teWidth :: !Int
  , teRuns :: Runs
  -- ^ Change-compressed history of the cycles the simulation forced.
  , teRest :: [Value]
  {- ^ Packed continuation for cycles the simulation never forced; 'dumpVCDC'
  drains it on demand when the dump window extends past 'teRuns'.
  -}
  }

-- | name → recorded trace, as returned by 'withCircuitContext'.
type TraceData = Map.Map String TraceEntry

{- | One past the last cycle any trace or probe recorded — the number of
cycles the simulation ACTUALLY ran.

Use this as the dump window for a /live/ capture: a consumer that stops
early (an assertion that found its answer) bounds the recording, and
@'dumpVCDC' (0, 'recordedCycles' …)@ then renders exactly what was simulated
without draining any signal past that point. Passing a larger, fixed window
instead makes the dump itself simulate the remaining cycles — which is
wasted compute whenever the simulation was cut short deliberately.
-}
recordedCycles :: TraceData -> ProbeMap -> Int
recordedCycles td pm = maximum (0 : traceEnds ++ probeEnds)
 where
  traceEnds = [e + 1 | TraceEntry _ _ ((_, e, _) : _) _ <- Map.elems td]
  probeEnds = [e + 1 | (_, _, (_, e, _) : _) <- Map.elems pm]

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
  {- ^ Hierarchy path, innermost first. Each 'component' application
  allocates a fresh list cell here; that heap object is the component
  INSTANCE's identity (see 'ordinalFor').
  -}
  , ccTracer :: Maybe (IORef LiveTraces)
  -- ^ 'Nothing' disables signal tracing
  , ccProbes :: Maybe (IORef LiveProbes)
  -- ^ 'Nothing' disables mealy probes
  , ccWindow :: !Int
  {- ^ Trailing capture window in cycles ('maxBound' = unlimited): recorders
  keep at least this many trailing cycles of history and may drop older
  ones. See 'WRuns'.
  -}
  , ccOrdinals :: Maybe (IORef OrdinalMap)
  {- ^ Per-simulation instance-ordinal registry; 'Nothing' disables
  instance tagging (fine while nothing is recorded)
  -}
  }

type HasCircuitContext = ?circuitContext :: CircuitContext

-- | Tracing and probing disabled; hierarchy still works.
noCircuitContext :: CircuitContext
noCircuitContext = CircuitContext [] Nothing Nothing maxBound Nothing

{- | Run an instrumented ('HasCircuitContext'-constrained) computation with a
disabled context, /without/ the caller having to acquire the constraint itself.

'HasCircuitContext' is a viral constraint: instrumenting a deep function forces
it onto every transitive caller. At a boundary where you do NOT want to thread a
context — most often a synthesis-facing caller — wrap the call:

> outputs = withoutCircuitContext (myComponent clk rst inp)

This replaces the hand-written shim idiom

> myComponentNC clk rst inp = let ?circuitContext = noCircuitContext in myComponent clk rst inp

which otherwise has to duplicate @myComponent@'s entire type signature just to
drop the constraint. Because instrumentation is HDL-transparent, the wrapped
call is identical — in both simulation and synthesis — to calling the
uninstrumented function directly.
-}
withoutCircuitContext :: ((HasCircuitContext) => r) -> r
withoutCircuitContext k = let ?circuitContext = noCircuitContext in k
{-# INLINE withoutCircuitContext #-}

{- | Is this context fully disabled (no recording)? When so, all
instrumentation is a no-op and the bookkeeping (hierarchy push, instance
ordinals, cycle counter, per-cycle @?probe@ binding) is skipped — so a
simulation that isn't collecting waveforms pays essentially nothing. The
ordinal registry is the tell: 'withCircuitContext' installs it, and it is
the one ref every recording path needs.
-}
ccDisabled :: CircuitContext -> Bool
ccDisabled ctx = isNothing (ccOrdinals ctx)

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

{- | Tag a name with an instance ordinal (and call site, if known):
@name\@i\@loc@. The @\'\@\'@ character is reserved for this; don't use it
in component/trace/probe names.
-}
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
withCircuitContext :: ((HasCircuitContext) => IO r) -> IO (r, TraceData, ProbeMap)
withCircuitContext = withCircuitContextWindow maxBound

{- | 'withCircuitContext' with a trailing capture window: recorders keep (at
least) the last @window@ cycles of history and drop older data, bounding
memory for signals that change every cycle (program counters, busses) — see
'WRuns'. Cycles before the retained window render @x@ in the dump.
-}
withCircuitContextWindow ::
  Int -> ((HasCircuitContext) => IO r) -> IO (r, TraceData, ProbeMap)
withCircuitContextWindow window k = do
  (r, traces, probes) <- withCircuitContextWindowE window k
  r' <- either throwIO pure r
  pure (r', traces, probes)

{- | 'withCircuitContextWindow' that survives a throwing action.

Whatever the action recorded BEFORE it threw is still returned, so a failing
test can be dumped — which is the whole point of capturing a waveform on
failure. 'withCircuitContextWindow' cannot do this: an exception escapes
before it reads the recorder refs, taking the recording with it.
-}
withCircuitContextWindowE ::
  Int ->
  ((HasCircuitContext) => IO r) ->
  IO (Either SomeException r, TraceData, ProbeMap)
withCircuitContextWindowE window k = do
  (ctx, freeze) <- newRecorders window
  r <- try (let ?circuitContext = ctx in k)
  (traces, probes) <- freeze
  pure (r, traces, probes)

{- | 'withCircuitContextWindow' for a simulation consumed in some monad over
IO rather than in IO itself — a hedgehog @PropertyT@, say, where the
assertions that force the circuit live in the property monad.

No 'try' here, deliberately: this is for monads that report failure as a
/value/ (hedgehog's @Failure@), so the action always returns and the recorder
refs are always read. A monad that reports failure by throwing needs
'withCircuitContextWindowE', which catches; an escaping exception here takes
the recording with it.
-}
withCircuitContextWindowM ::
  (MonadIO m) => Int -> ((HasCircuitContext) => m r) -> m (r, TraceData, ProbeMap)
withCircuitContextWindowM window k = do
  (ctx, freeze) <- liftIO (newRecorders window)
  r <- let ?circuitContext = ctx in k
  (traces, probes) <- liftIO freeze
  pure (r, traces, probes)

{- | Fresh recorder refs wrapped in a context, paired with the action that
reads them back out. Split out so the variants above differ only in how they
run the action, not in what recording means.
-}
newRecorders :: Int -> IO (CircuitContext, IO (TraceData, ProbeMap))
newRecorders window = do
  tRef <- newIORef Map.empty
  pRef <- newIORef Map.empty
  oRef <- newIORef emptyOrdinals
  let freeze = do
        live <- readIORef tRef
        traces <- traverse freezeTrace live
        probes <- readIORef pRef >>= traverse freezeProbe
        pure (traces, probes)
  pure (CircuitContext [] (Just tRef) (Just pRef) window (Just oRef), freeze)
 where
  freezeTrace (per, w, tapRef) = do
    TraceTap _ runs rest <- readIORef tapRef
    pure (TraceEntry per w (wrunsToRuns runs) rest)
  freezeProbe (per, w, accRef) = (,,) per w . wrunsToRuns <$> readIORef accRef

{- | @component "fifo" circuit@: everything traced or probed inside
@circuit@ is qualified by @…fifo.@.

Every application is a distinct /instance/: it allocates a fresh hierarchy
cell whose heap identity distinguishes it from every other instance of the
same name (resolved to ordinals at registration, see 'ordinalFor'), so
multiple instances of the same (sub)circuit under one parent never collide —
all are recorded, and 'dumpVCDC' disambiguates downstream (@fifo_0@,
@fifo_1@, …).

Instrumentation-transparent to Clash: during HDL generation
('clashSimulation' is 'False') this is exactly @k@ — the whole hierarchy
machinery is dropped by Clash's dead-code elimination, so a design stays
synthesizable with 'component' calls left in. In simulation it delegates to
'simComponent', which is where the (OPAQUE, per-application) cell allocation
happens.
-}
component ::
  (HasCallStack, HasCircuitContext) => String -> ((HasCircuitContext) => r) -> r
component nm k
  | clashSimulation = simComponent nm k
  | otherwise = k
{-# INLINE component #-}

{- | Simulation worker for 'component'. OPAQUE so each application allocates a
fresh @ccHier@ cell (its heap identity is the instance key, see 'ordinalFor')
rather than being floated and shared. Skips the push entirely when the
context is disabled, so uninstrumented-in-effect simulation is free.
-}
simComponent ::
  (HasCallStack, HasCircuitContext) => String -> ((HasCircuitContext) => r) -> r
simComponent nm k
  | ccDisabled ctx0 = k
  | otherwise = let ?circuitContext = ctx' in k
 where
  ctx0 = ?circuitContext
  ctx' = ctx0{ccHier = HierSeg nm (callLoc callStack) : ccHier ctx0}
{-# OPAQUE simComponent #-}

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
  ((HasCircuitContext) => Index n -> a -> b) ->
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

Unlike stock 'Clash.Signal.Trace.traceSignal' there is no 'Typeable'
requirement: the 'TypeRep' field of a trace only feeds clash's replay
machinery, which maps created here never reach — and requiring it would
exclude size-polymorphic payloads (@Unsigned n@ under a @KnownNat n@ given,
say) from tracing inside polymorphic components.
-}
traceSignalC ::
  forall dom a.
  (HasCallStack, HasCircuitContext, KnownDomain dom, BitPack a, NFDataX a) =>
  String ->
  Signal dom a ->
  Signal dom a
traceSignalC nm sig
  | clashSimulation = simTraceSignalC nm sig
  | otherwise = sig
{-# INLINE traceSignalC #-}

{- | Simulation worker for 'traceSignalC'. OPAQUE, holding the sole
'unsafePerformIO' in the design (lazy one-shot registration, exactly like
stock 'Clash.Signal.Trace.traceSignal1'); identity when tracing is off.
-}
simTraceSignalC ::
  forall dom a.
  (HasCallStack, HasCircuitContext, KnownDomain dom, BitPack a, NFDataX a) =>
  String ->
  Signal dom a ->
  Signal dom a
simTraceSignalC nm sig = case (ccTracer ctx, ccOrdinals ctx) of
  (Just ref, Just ordRef) ->
    -- Instance ordinals are resolved inside this same action — no id minting
    -- in pure code.
    unsafePerformIO $ do
      segs <- resolveHier ordRef (ccHier ctx)
      leafI <- freshOrdinal ordRef
      let key = intercalate "." (reverse (taggedLoc nm loc leafI : segs))
      registerTrace (ccWindow ctx) ref period key sig
  _ -> sig
 where
  ctx = ?circuitContext
  loc = callLoc callStack
  period :: Int
  period = case knownDomain @dom of
    SDomainConfiguration{sPeriod} -> snatToNum sPeriod
{-# OPAQUE simTraceSignalC #-}

{- | Register a trace and return the signal TAPPED: forcing element /i/ of the
returned signal (which the design does as the simulation advances) also packs
that cycle's value and records it — change-compressed — into the trace's
accumulator, after which the raw value is garbage.

This lockstep recording is the space-leak fix that has to live in the
infrastructure, not the user's code. The alternative (what stock
'Clash.Signal.Trace.traceSignal#' does, and what this function used to do) is
to store a lazy @map packMaskValue (sample_lazy sig)@ and force it at dump
time — but holding that list's HEAD while the design advances the shared
signal retains every cycle's raw design value until the dump: memory linear
in simulation length, with full-record-sized elements. The tap instead keeps
only the change-compressed 'Runs' plus the un-consumed packed TAIL
('ttRest'), which references no history.

Value semantics are unchanged: @pack@ per sample via 'packMaskValue'
(undefined bits masked, exceptions rendered @x@), and 'sample_lazy' so a
partial binding is never forced outside the guard. Cycles the design never
forces are drained from 'ttRest' by 'dumpVCDC' on demand, exactly as the
dump-time forcing used to do. No duplicate-key check: keys carry fresh leaf
ordinals by construction.
-}
registerTrace ::
  forall dom a.
  (KnownDomain dom, BitPack a, NFDataX a) =>
  Int ->
  IORef LiveTraces ->
  Int ->
  String ->
  Signal dom a ->
  IO (Signal dom a)
registerTrace window ref period key sig = do
  tapRef <- newIORef (TraceTap 0 emptyWRuns packed)
  atomicModifyIORef' ref $ \m ->
    (Map.insert key (period, width, tapRef) m, ())
  pure (tap window tapRef 0 packed sig)
 where
  width = snatToNum (SNat @(BitSize a))
  packed = map packMaskValue (sample_lazy sig)

  -- Walk the raw signal and its packed view in lockstep, recording cycle /i/
  -- when the TAIL of cell /i/ (i.e. cell /i+1/) is first forced — cons cells
  -- are memoized by lazy evaluation, so the write happens once per cycle, in
  -- order.
  --
  -- The one-cell delay is essential, not cosmetic. Packing cycle /i/ while
  -- PRODUCING cell /i/ deadlocks on combinational feedback: in a
  -- value-level knot (clash-protocols ties @m2s@/@s2m@ through 'Circuit'
  -- fixpoints), fully evaluating value /i/ can demand — through the other
  -- half of the knot — spine cell /i/ of this very signal, which is exactly
  -- the cell under construction: a blackhole re-entry, surfacing as
  -- @<<loop>>@ single-threaded and as a silent hang under the threaded RTS.
  -- By the time the design forces the tail, cell /i/ is fully handed over
  -- and no evaluation is suspended inside it; packing value /i/ then demands
  -- at most already-memoized (or causally earlier) structure. A value can
  -- never demand its own FUTURE spine, so recording at the tail cannot
  -- re-enter the cell being produced.
  tap :: Int -> IORef TraceTap -> Int -> [Value] -> Signal dom a -> Signal dom a
  tap window tapRef !cyc pk s = case (pk, s) of
    (v0 : pkRest, a :- as) ->
      a
        :- unsafePerformIO
          ( do
              !v <- evaluate v0
              atomicModifyIORef' tapRef $ \(TraceTap _ runs _) ->
                (TraceTap (cyc + 1) (addCycleW window cyc v runs) pkRest, ())
              pure (tap window tapRef (cyc + 1) pkRest as)
          )
    _ -> s -- unreachable: both streams are infinite

{- | Force a value to its packed @(mask, value)@ VCD representation, mapping ANY
exception to an all-undefined (@x@) entry.

This deliberately catches more than 'Clash.XException.XException' (genuinely
undefined bits): a design routinely contains bindings that are only /defined/
under a condition — e.g. @fromJust@ / @head@ on a signal that is @Nothing@ /
empty until a link comes up. Normal simulation never forces such a binding
outside that condition, but auto-tracing samples it every cycle. A tracer must
never decide whether the simulation crashes, so a sample that throws is
recorded as @x@ — exactly how a stock Clash trace renders undefined bits — and
the offending cycles simply show undefined in the waveform.
-}
packMaskValue :: forall a. (BitPack a) => a -> (Natural, Natural)
packMaskValue a =
  -- 'unsafeDupablePerformIO': this guard is a pure computation (no side
  -- effects to protect from re-execution), so the per-sample cost of
  -- 'unsafePerformIO's duplicate-suppression would buy nothing.
  unsafeDupablePerformIO $ do
    -- Canonicalise: clear every @val@ bit that @mask@ marks undefined, so an
    -- evaluated-but-undefined bit is ALWAYS @(mask=1, val=0)@. Clash's 'pack'
    -- does not promise a particular @val@ under the mask, and we reserve the
    -- @(mask=1, val=1)@ encoding exclusively for the NOT-evaluated (gap) case
    -- (see 'expandRunsX'/'renderVC'). Without this a masked value bit set to 1
    -- would render as @z@ (not-evaluated) when it is really @x@ (undefined).
    r <-
      try
        ( evaluate
            ( case pack a of
                BV mask val ->
                  let v' = val .&. (fullMask `xor` mask)
                   in mask `seq` v' `seq` (mask, v')
            )
        )
    pure $ case r of
      Right t -> t
      Left (_ :: SomeException) -> (fullMask, 0)
 where
  fullMask = 2 ^ (snatToNum (SNat @(BitSize a)) :: Int) - 1 :: Natural
{-# NOINLINE packMaskValue #-}

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

Returns @Left@ (never throws) when nothing was recorded — an empty run, or
one where no traced\/probed signal was ever forced. Stock 'dumpVCD1#'
/errors/ on an empty map; a run collecting no data is normal for
instrumentation left in place (e.g. a property test case with zero cycles),
so it must not bring the simulation down.
-}
dumpVCDC :: (Int, Int) -> TraceData -> ProbeMap -> IO (Either String Text.Text)
dumpVCDC slice@(offset, nSamples) td pm = do
  now <- getCurrentTime
  let combined =
        disambiguate (Map.union (Map.map traceVals td) (Map.map probeVals pm))
      -- Signals with an identical recorded history share ONE identifier code
      -- (see 'aliasGroups'): only representatives are handed to 'dumpVCD1#', so
      -- their changes are emitted once, and the copies are re-attached
      -- afterwards as extra '$var' declarations pointing at the same code.
      --
      -- Aliases are dropped only AFTER 'disambiguate'. Removing them earlier
      -- would change the key set it numbers siblings from, silently renaming
      -- unrelated signals' @_0@\/@_1@ suffixes.
      emitted = Map.withoutKeys combined (Map.keysSet aliasByRep')
  pure $
    if Map.null combined
      then Left "dumpVCDC: nothing was traced or probed in this run"
      else renderVCDHier now . withAliases <$> dumpVCD1# slice emitted
 where
  end = offset + nSamples

  -- Recover the raw-key -> emitted-path mapping by running the SAME transform
  -- over a map whose values are its own keys.
  --
  -- It must be keyed on the SAME set 'combined' is, traces AND probes: sibling
  -- numbering is a function of the whole key set, so disambiguating the traces
  -- alone yields paths that disagree with 'combined' wherever a probe shares a
  -- name — and removing a path that names a DIFFERENT signal drops it from the
  -- dump entirely (measured: 11 of 240 declarations silently lost).
  cleanOf =
    Map.fromList
      [ (raw, clean)
      | (clean, raw) <- Map.toList (disambiguate (Map.mapWithKey (\k _ -> k) sameKeys))
      ]
   where
    sameKeys = Map.union (() <$ td) (() <$ pm)

  -- alias path -> representative path
  aliasByRep' =
    Map.fromList
      [ (a, r)
      | grp <- aliasGroups td
      , Just (r : as) <- [traverse (`Map.lookup` cleanOf) grp]
      , a <- as
      ]

  -- representative path -> its alias paths
  aliasesFor =
    Map.fromListWith (++) [(r, [a]) | (a, r) <- Map.toList aliasByRep']

  withAliases (VCDFile decs sims) = VCDFile (map go decs) sims
   where
    go (Vars vs) = Vars (concatMap expand vs)
    go d = d
    expand v =
      v
        : [ v{varReference = a}
          | a <- Map.findWithDefault [] (varReference v) aliasesFor
          ]

  -- Expand the change-compressed stores back to the dense per-cycle lists
  -- 'dumpVCD1#' expects (it transposes; ragged lists would misalign
  -- columns). The expansion is lazy and consumed streaming by the render —
  -- the dense form never has to be resident, which is the point of storing
  -- runs. The TypeRep field only matters for replay, which these entries
  -- don't support.

  -- The dump window may extend past what the simulation forced, in which
  -- case the remaining cycles are drained (packed on demand) from the
  -- stored continuation — exactly the dump-time forcing the pre-runs
  -- implementation did. Cycles dropped by a trailing capture window (and
  -- any probe-style gaps) densify to @x@.
  traceVals (TraceEntry per w runs rest) =
    ( ByteStringLazy.empty
    , per
    , w
    , expandRunsX w covered runs ++ take (end - covered) rest
    )
   where
    covered = case runs of
      [] -> 0
      (_, e, _) : _ -> e + 1

  -- A probe cycle that was never reached stays a gap in the runs; densify
  -- with @x@ for those cycles, exactly as a missing key rendered before.
  probeVals (per, w, runs) =
    (ByteStringLazy.empty, per, w, expandRunsX w end runs)

  -- Densify a run history to per-cycle values on @[0, upto)@, filling gaps
  -- (never-forced probe cycles, window-dropped history) with @x@. Stops at
  -- the end of the recorded data if that comes first.
  expandRunsX w upto runs = go 0 (reverse runs)
   where
    -- Gap fill = NOT evaluated / not retained (never-forced probe cycle,
    -- window-dropped history). Encoded @(fullMask, fullMask)@ so it renders
    -- @z@ — distinct from an evaluated-but-undefined sample @(fullMask, 0)@
    -- which renders @x@. This is the not-evaluated vs. undefined distinction:
    -- @z@ means "we never looked", @x@ means "we looked and it was undefined".
    xv = let m = (2 :: Natural) ^ w - 1 in (m, m)
    go c [] = replicate (upto - c) xv
    go c ((s, e, v) : rs) =
      replicate (s - c) xv ++ replicate (e - s + 1) v ++ go (e + 1) rs

{- | Group trace keys whose recorded history is IDENTICAL, so a dump can declare
every name but emit the values only once (VCD allows several @$var@
declarations to share one identifier code).

This is worth doing because the same electrical net is auto-traced at every
level it passes through — an interconnect's vector element, a component's
interface port, its intermediate port, a composite half, an internal binding.
In a bittide firmware trace that reached THIRTEEN names for one 72-bit bus, and
67% of all value-change bytes were such copies.

Grouping is observational (equal width, period and 'Runs') rather than
structural, and that is a measured decision, not a shortcut. Runtime object
identity was tried twice, both times against a real firmware DUT:

* 'System.Mem.StableName' of the signal at REGISTRATION — recovered 0.003% of
  the bytes. The copies are distinct THUNKS (circuit-notation's port
  indirections, tuple-half selectors, 'Vec' element selection), so a stable name
  sees thirteen objects rather than one wire.
* 'System.Mem.StableName' of the CONS CELL, taken inside 'tap' where the signal
  is already in WHNF so nothing extra is forced — recovered 1%, and merged 0 of
  424 known duplicate members.

The second result is conclusive rather than disappointing, and the reason is
worth keeping: 'tap' REBUILDS the stream (@a :- unsafePerformIO …@), so a traced
signal derived from another traced signal observes the cell its parent's tap
produced, never a shared one. Tracing destroys exactly the sharing that would
identify a duplicate, so no runtime identity can see through it. Detecting these
statically is possible in principle — @port_Fwd = \<lambda binder\>@ and
@b = a@ are visible to the renamer before any tap exists — and that, not object
identity, is the route to a structural criterion.

Aliasing on equal history loses nothing: within the dumped range those signals
genuinely carried the same values, sharing an identifier is precisely VCD's
encoding for that, and a viewer still lists each name separately. It does mean
two unrelated nets that happen to agree over the captured window share a code —
which changes no displayed value, only the encoding.

Returned groups have more than one member, ordered so the first is the
representative whose changes are emitted. Keys absent from the result are
unique.
-}
aliasGroups :: TraceData -> [[String]]
aliasGroups td =
  [ map fst cls
  | bucket <- Map.elems buckets
  , cls <- classes bucket
  , length cls > 1
  ]
 where
  {- Key a 'Map' on the whole history. That sounds expensive and is not: list
  'Ord' is lexicographic, so two distinct signals almost always differ in their
  FIRST run and the comparison exits there; only genuine duplicates pay
  full-length comparisons. Measured on a 544-signal, 900k-run firmware trace:
  77 ms inside a ~1.5 s dump.

  Hashing each history once and confirming with '==' was implemented and
  measured too, on the theory that it removes a data-dependent cliff (many
  signals sharing long identical prefixes). It came out 2.7x SLOWER -- 205 ms --
  because it must touch every run of every signal, where early exit touches
  almost none. Kept the simpler one; the cliff remains hypothetical and this
  note exists so the trade is not re-litigated from first principles. -}
  buckets =
    Map.fromListWith
      (flip (++))
      [((teWidth e, tePeriod e, teRuns e), [(k, e)]) | (k, e) <- Map.toList td]

  {- Entries sharing a key are already equal, so a bucket IS one class; the
  'partition' pass is kept only because 'Map' equality on the key tuple is what
  established that, and it keeps the head-is-representative invariant explicit.
  Ascending-key order from 'Map.toList' is preserved, which keeps identifier
  assignment (and the goldens) deterministic. -}
  classes [] = []
  classes (x@(_, e) : rest) =
    let (same, diff) = partition (sameHistory e . snd) rest
     in (x : same) : classes diff

  sameHistory a b =
    teWidth a == teWidth b
      && tePeriod a == tePeriod b
      && teRuns a == teRuns b

--------------------------------------------------------------------------------
-- Downstream disambiguation of instance ids
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

{- | Parse an encoded call site @L\<line\>C\<col\>F\<hash\>@ into a sort key
(file hash, line, col).
-}
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

{- | Insert a var along its dot-separated reference path; the leaf segment
becomes the var's reference inside its scope.
-}
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
  renderVC (ValueChange 1 idCode (1, 0)) = Text.pack ('x' : idCode)
  renderVC (ValueChange 1 idCode (1, 1)) = Text.pack ('z' : idCode)
  renderVC (ValueChange 1 idCode v) =
    error ("dumpVCDC: bad 1-bit value " <> show v <> " for " <> show idCode)
  renderVC (ValueChange w idCode (mask, val)) =
    Text.pack ('b' : map digit (reverse [0 .. w - 1]) ++ ' ' : idCode)
   where
    -- (mask, val) per bit: 0/1 = defined; @x@ = evaluated-undefined
    -- (mask set, val clear — canonicalised in 'packMaskValue'); @z@ =
    -- not-evaluated / not-retained gap (mask set, val set — only ever
    -- produced by 'expandRunsX').
    digit d = case (testBit mask d, testBit val d) of
      (False, False) -> '0'
      (False, True) -> '1'
      (True, False) -> 'x'
      (True, True) -> 'z'

--------------------------------------------------------------------------------
-- Probes inside mealy machines
--------------------------------------------------------------------------------

data ProbeCtx = ProbeCtx
  { pcCtx :: CircuitContext
  , pcPeriod :: !Int
  {- ^ Clock period (ps) of the probed mealy's domain; carried so probe
  values can be dumped to VCD alongside traced signals.
  -}
  , pcPath :: [HierSeg]
  {- ^ Identity object of the 'mealyProbed' application (a per-application
  hierarchy cell, carrying the mealy's call site): probes from distinct
  instances of the same mealy stay distinct even under identical names.
  -}
  , pcCycle :: !Int
  , pcCache :: IORef (Map.Map String (IORef WRuns))
  {- ^ Per-INSTANCE cache: probe name → its accumulator. A probe fires every
  cycle, so its per-cycle cost must not include name qualification —
  resolving the hierarchy (StableName hashing per segment), building the
  qualified-name string, and inserting into a global map keyed by long
  strings are each cheap once but ruinous a million times per probe. The
  first firing resolves and registers; every later cycle is a lookup of a
  SHORT name in a map with a handful of entries plus one 'IORef' update.
  -}
  }

-- | Live probe registry: qualified name → (period ps, bit width, accumulator).
type LiveProbes = Map.Map String (Int, Int, IORef WRuns)

{- | A fresh probe cache for one mealy\/moore\/'probeFmap' INSTANCE. The
argument ties the allocation to the instance's identity object ('pcPath'):
a closed @unsafePerformIO (newIORef …)@ would be floated to the top level by
full laziness and SHARED across all instances, silently merging their probes
(observed: sibling instances losing wires in the golden). The free variable
pins the thunk inside each application; NOINLINE keeps the allocation out of
reach of inlining-enabled CSE.
-}
newProbeCache :: [HierSeg] -> IORef (Map.Map String (IORef WRuns))
newProbeCache p = unsafePerformIO (p `seq` newIORef Map.empty)
{-# NOINLINE newProbeCache #-}

type HasProbe = ?probe :: ProbeCtx

{- | A disabled probe context: supplied to a step function on the paths where
probing does nothing (HDL generation, and probe-free simulation), so
'probe' has a @?probe@ to resolve but never records. Never forced on those
paths — 'probe' is identity there — so its fields are irrelevant.
-}
noProbe :: ProbeCtx
noProbe = ProbeCtx noCircuitContext 0 [] 0 (unsafePerformIO (newIORef Map.empty))
{-# NOINLINE noProbe #-}

{- | Record the value of any expression inside a probed step function:

> f :: HasProbe => Int -> Int -> (Int, Int)
> f acc i = let acc' = probe "acc" (acc + i) in (acc', acc)

Returns its argument. The write happens when the expression is forced, keyed
by (qualified name, cycle): idempotent under re-evaluation and sparks; never
forced → never recorded. An X value is stored as an all-undefined (masked)
entry rather than an exception.

Transparent to Clash: during HDL generation this is exactly @a@, so a probed
step function synthesizes as if the 'probe' calls were not there.
-}
probe :: forall a. (HasProbe, BitPack a, NFDataX a) => String -> a -> a
probe nm a
  | clashSimulation = simProbe nm a
  | otherwise = a
{-# INLINE probe #-}

{- | Simulation worker for 'probe'. NOINLINE, holding the 'unsafePerformIO'
write; identity when probing is off.
-}
simProbe :: forall a. (HasProbe, BitPack a, NFDataX a) => String -> a -> a
simProbe nm a = case (ccProbes ctx, ccOrdinals ctx) of
  (Just liveRef, Just ordRef) ->
    unsafePerformIO $ do
      -- Hot path: this runs EVERY cycle. Only the first firing of a probe
      -- resolves its hierarchy and qualified name (see 'pcCache'); after
      -- that a cycle costs one small-map lookup and one 'IORef' update.
      cache <- readIORef (pcCache ?probe)
      accRef <- case Map.lookup nm cache of
        Just accRef -> pure accRef
        Nothing -> do
          segs <- resolveHier ordRef (ccHier ctx)
          mI <- ordinalFor ordRef (pcPath ?probe)
          let qual = intercalate "." (reverse (taggedLoc nm mLoc mI : segs))
          accRef <- newIORef emptyWRuns
          atomicModifyIORef' (pcCache ?probe) $ \c ->
            (Map.insert nm accRef c, ())
          atomicModifyIORef' liveRef $ \m ->
            (Map.insert qual (per, w, accRef) m, ())
          pure accRef
      -- Force the packed value NOW, releasing the design value 'a', and
      -- accumulate change-compressed and STRICT ('$!'): a probe fires every
      -- cycle, so per-cycle storage (or a lazy merge) makes memory linear in
      -- simulation length — see 'Runs'. Probes fire in simulation order, and
      -- a cycle the probed expression never reaches simply leaves a gap in
      -- the runs, rendered @x@ by 'dumpVCDC'.
      let !v = val
      atomicModifyIORef' accRef $ \runs ->
        let !rs = addCycleW (ccWindow ctx) cyc v runs in (rs, ())
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
  -- Robust to undefined bits AND partial bindings; see 'packMaskValue'.
  val = packMaskValue a
{-# NOINLINE simProbe #-}

{- | Stock 'Clash.Explicit.Mealy.mealy', except the step function may 'probe'
its internals. Implemented purely with stock combinators: a companion counter
register provides the cycle identity, and the step is applied under a
per-cycle @?probe@ binding.

Transparent to Clash /and/ overhead-free when not recording: during HDL
generation, and in probe-free simulation, this reduces to a plain
'Clash.Explicit.Mealy.mealy' — no companion counter register, no @?probe@
plumbing (so no extra hardware in the netlist, and no per-cycle cost in a
simulation that isn't collecting waveforms). The counter and probe binding
appear only when a live context is actually recording (see 'simMealyProbed').
-}
mealyProbed ::
  forall dom s i o.
  (HasCallStack, HasCircuitContext, KnownDomain dom, NFDataX s) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  ((HasProbe) => s -> i -> (s, o)) ->
  s ->
  Signal dom i ->
  Signal dom o
mealyProbed clk rst ena f s0 inp
  | clashSimulation = simMealyProbed clk rst ena f s0 inp
  | otherwise = mealy clk rst ena (\s i -> let ?probe = noProbe in f s i) s0 inp
{-# INLINE mealyProbed #-}

{- | Simulation worker for 'mealyProbed'. OPAQUE so each application allocates
a fresh @path@ cell identifying the mealy INSTANCE (see 'ordinalFor').
Falls back to a plain 'mealy' — no counter — when the context is disabled.
-}
simMealyProbed ::
  forall dom s i o.
  (HasCallStack, HasCircuitContext, KnownDomain dom, NFDataX s) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  ((HasProbe) => s -> i -> (s, o)) ->
  s ->
  Signal dom i ->
  Signal dom o
simMealyProbed clk rst ena f s0 inp
  | ccDisabled ctx = mealy clk rst ena (\st i -> let ?probe = noProbe in f st i) s0 inp
  | otherwise = o
 where
  ctx = ?circuitContext
  -- A fresh cell per application: this heap object identifies the mealy
  -- INSTANCE (see 'ordinalFor'); OPAQUE keeps it per-application.
  path = HierSeg "mealyProbed" (callLoc callStack) : ccHier ctx
  -- One name-resolution cache per INSTANCE (see 'pcCache' and
  -- 'newProbeCache'); like @path@, allocated once per application by the
  -- enclosing OPAQUE worker.
  cache = newProbeCache path
  period :: Int
  period = case knownDomain @dom of
    SDomainConfiguration{sPeriod} -> snatToNum sPeriod
  -- Deliberately immune to rst and ena: the key must be the SAMPLE index,
  -- not the architectural cycle, or probe values land at the wrong VCD
  -- times whenever reset/enable stalls the state register.
  cnt = delay clk enableGen (0 :: Int) ((+ 1) <$> cnt)
  step n st i = let ?probe = ProbeCtx ctx period path n cache in f st i
  (s', o) = unbundle (step <$> cnt <*> s <*> inp)
  s = register clk rst ena s0 s'
{-# OPAQUE simMealyProbed #-}

{- | Stock 'Clash.Explicit.Moore.moore', except the /transition/ function may
'probe' its internals. The mechanism mirrors 'mealyProbed': a companion counter
supplies the cycle identity and the transition runs under a per-cycle @?probe@
binding. The output projection is intentionally left unprobed — in a Moore
machine it is usually a trivial selector, and keeping it out avoids a second
cycle key. Note that a Moore machine registers its output, so a probe recorded
at cycle @n@ (the transition computing the state for @n+1@) precedes the output
it produces by one cycle.

Transparent to Clash /and/ overhead-free when not recording: during HDL
generation, and in probe-free simulation, this reduces to a plain
'Clash.Explicit.Moore.moore' — no companion counter, no @?probe@ plumbing.
-}
mooreProbed ::
  forall dom s i o.
  (HasCallStack, HasCircuitContext, KnownDomain dom, NFDataX s) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  -- | Transition function; may 'probe' its internals.
  ((HasProbe) => s -> i -> s) ->
  -- | Output projection (not probed).
  (s -> o) ->
  s ->
  Signal dom i ->
  Signal dom o
mooreProbed clk rst ena t out s0 inp
  | clashSimulation = simMooreProbed clk rst ena t out s0 inp
  | otherwise = moore clk rst ena (\s i -> let ?probe = noProbe in t s i) out s0 inp
{-# INLINE mooreProbed #-}

{- | Simulation worker for 'mooreProbed'. OPAQUE so each application allocates a
fresh @path@ cell identifying the moore INSTANCE (see 'ordinalFor'). Falls
back to a plain 'moore' — no counter — when the context is disabled.
-}
simMooreProbed ::
  forall dom s i o.
  (HasCallStack, HasCircuitContext, KnownDomain dom, NFDataX s) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  ((HasProbe) => s -> i -> s) ->
  (s -> o) ->
  s ->
  Signal dom i ->
  Signal dom o
simMooreProbed clk rst ena t out s0 inp
  | ccDisabled ctx = moore clk rst ena (\st i -> let ?probe = noProbe in t st i) out s0 inp
  | otherwise = out <$> s
 where
  ctx = ?circuitContext
  path = HierSeg "mooreProbed" (callLoc callStack) : ccHier ctx
  cache = newProbeCache path
  period :: Int
  period = case knownDomain @dom of
    SDomainConfiguration{sPeriod} -> snatToNum sPeriod
  cnt = delay clk enableGen (0 :: Int) ((+ 1) <$> cnt)
  step n st i = let ?probe = ProbeCtx ctx period path n cache in t st i
  s' = step <$> cnt <*> s <*> inp
  s = register clk rst ena s0 s'
{-# OPAQUE simMooreProbed #-}

{- | Bundled 'mealyProbed': the step function's input and output are ordinary
tuples (or other 'Bundle' types) rather than a single packed type, mirroring
'Clash.Prelude.mealyB'. Drop-in replacement for @mealyB@ that additionally
probes the step function's internals. Use for the common
@(a, b) = mealyB go s (in0, in1)@ idiom.
-}
mealyBProbed ::
  forall dom s i o.
  (HasCallStack, HasCircuitContext, KnownDomain dom, NFDataX s, Bundle i, Bundle o) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  ((HasProbe) => s -> i -> (s, o)) ->
  s ->
  Unbundled dom i ->
  Unbundled dom o
mealyBProbed clk rst ena f s0 = unbundle . mealyProbed clk rst ena f s0 . bundle
{-# INLINE mealyBProbed #-}

{- | Bundled 'mooreProbed', mirroring 'Clash.Prelude.mooreB'. See
'mealyBProbed'.
-}
mooreBProbed ::
  forall dom s i o.
  (HasCallStack, HasCircuitContext, KnownDomain dom, NFDataX s, Bundle i, Bundle o) =>
  Clock dom ->
  Reset dom ->
  Enable dom ->
  ((HasProbe) => s -> i -> s) ->
  (s -> o) ->
  s ->
  Unbundled dom i ->
  Unbundled dom o
mooreBProbed clk rst ena t out s0 = unbundle . mooreProbed clk rst ena t out s0 . bundle
{-# INLINE mooreBProbed #-}

{- | Probe the internals of a /combinational/ function applied over a signal.

Clash designs express combinational logic as pure functions lifted over
signals with @\<$\>@ \/ @\<*\>@ \/ 'liftA2' — e.g. @route \<$\> masterS \<*\> slavesS@
inside a 'Circuit'. The named @where@\/@let@ bindings inside such a function
(@toSlaves@, @oneHotSelected@, …) are exactly the internal wires a developer
wants in the waveform, but they are neither 'Signal's (so 'traceSignalC'
cannot reach them) nor inside a 'mealyProbed' step (so 'probe' has no cycle
context). 'probeFmap' supplies that context: it is 'fmap' that additionally
binds a per-cycle @?probe@, so 'probe' calls inside @f@ — including the ones
the plugin injects when @f@ carries a 'HasProbe' signature — record.

Cycle identity comes from a companion counter on @clk@ (the function is
stateless, so there is nothing else to derive it from). Rewrite a multi-input
application by bundling: @f \<$\> a \<*\> b@ becomes
@probeFmap clk (\\(x, y) -> f x y) (bundle (a, b))@.

Transparent to Clash and overhead-free when not recording: during HDL
generation, and in probe-free simulation, this reduces to @f \<$\> sig@ with a
disabled probe context (no counter, no @?probe@ plumbing, no extra hardware).
-}
probeFmap ::
  forall dom a b.
  (HasCallStack, HasCircuitContext, KnownDomain dom) =>
  Clock dom ->
  ((HasProbe) => a -> b) ->
  Signal dom a ->
  Signal dom b
probeFmap clk f inp
  | clashSimulation = simProbeFmap clk f inp
  | otherwise = (\a -> let ?probe = noProbe in f a) <$> inp
{-# INLINE probeFmap #-}

{- | Simulation worker for 'probeFmap'. OPAQUE so each application allocates a
fresh @path@ cell identifying this combinational site (see 'ordinalFor').
Falls back to a plain 'fmap' — no counter — when the context is disabled.
-}
simProbeFmap ::
  forall dom a b.
  (HasCallStack, HasCircuitContext, KnownDomain dom) =>
  Clock dom ->
  ((HasProbe) => a -> b) ->
  Signal dom a ->
  Signal dom b
simProbeFmap clk f inp
  | ccDisabled ctx = (\a -> let ?probe = noProbe in f a) <$> inp
  | otherwise = step <$> cnt <*> inp
 where
  ctx = ?circuitContext
  path = HierSeg "probeFmap" (callLoc callStack) : ccHier ctx
  cache = newProbeCache path
  period :: Int
  period = case knownDomain @dom of
    SDomainConfiguration{sPeriod} -> snatToNum sPeriod
  cnt = delay clk enableGen (0 :: Int) ((+ 1) <$> cnt)
  step n a = let ?probe = ProbeCtx ctx period path n cache in f a
{-# OPAQUE simProbeFmap #-}
