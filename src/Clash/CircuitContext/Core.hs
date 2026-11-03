{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
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

Scoped simulation context: hierarchy, a per-simulation recorder, and
probes inside mealy machines — all layered on UNMODIFIED clash-prelude.

Everything hangs off one implicit parameter @?circuitContext@:

* 'component' pushes a hierarchy segment; trace/probe names are qualified by
  the path where they are created.
* 'traceSignalC' is 'Clash.Signal.Trace.traceSignal' against the context's
  own recorder instead of the global 'traceMap#'. The recorder writes value
  changes to a spill file as the simulation runs
  ("Clash.CircuitContext.Internal.Recorder"), so the recorded history is
  not kept in memory. (The taps still keep raw design values alive while a
  run records, so its memory still grows with its length: issue #38.)
* 'mealyProbed' + 'probe' record values of expressions INSIDE a mealy step
  function. The step function has no cycle identity of its own, so the
  wrapper derives one from a companion counter register and binds a
  per-application @?probe@ context; writes are keyed by (name, cycle) and
  hence idempotent — safe under lazy re-ordering and speculative sparks, and
  an expression that is never forced simply records nothing.
* 'traceSignalSW' and 'probeSW' are 'traceSignalC' and 'probe' plus the
  payload type's @clash-shockwaves@ descriptor. They are separate functions,
  not extra constraints on the plain ones, because recording and describing
  have different requirements: a size-polymorphic payload is routinely
  something 'BitPack' accepts and 'Clash.Shockwaves.Waveform' does not.

Tracing is optional by construction: with 'noCircuitContext' (no recorder)
every combinator collapses to identity.

Multiple instances of the same circuit in the same component are all
recorded. Instance identity is HEAP identity: each 'component' (and
'mealyProbed') application allocates a fresh hierarchy cell, which is
resolved to a small ordinal at registration time via its 'StableName' — no
global state and no id minting in pure code. Map keys carry
@name\@ordinal\@loc@ tags (the @\'\@\'@ character is reserved — don't use it
in names); the VCD writer strips them and, where siblings genuinely collide,
renames them @name_0@, @name_1@, …, ordered by the design call site of
'component' (see 'callLoc'). Instances born at the same call site are
numbered in the order the simulation first reaches them: bare 'fmap'
replication, and every instance of one plugin-made component (the plugin
writes @component "f"@ inside @f@). Name those by structural position with
'imapComponents', or call 'component' by hand at each site.
-}
module Clash.CircuitContext.Core (
  -- * Context
  CircuitContext (..),
  HasCircuitContext,
  noCircuitContext,
  withoutCircuitContext,
  withRecording,
  withRecordingFrom,

  -- * Hierarchy
  HierSeg (..),
  CircuitContextAnnotation (..),
  component,
  imapComponents,
  qualifyName,

  -- * Scoped signal tracing
  traceSignalC,
  traceSignalSW,

  -- * Probes inside mealy machines
  ProbeCtx (..),
  HasProbe,
  probe,
  probeSW,
  mealyProbed,
  mealyBProbed,
  mooreProbed,
  mooreBProbed,
  probeFmap,
) where

import Data.Data (Data)

import Clash.Annotations.Primitive (Primitive (InlineYamlPrimitive), hasBlackBox)
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
import Clash.Sized.Internal.BitVector (BitVector (BV))
import Clash.XException (NFDataX)

import Clash.Shockwaves.Internal.Types (Translator, TypeName)
import Clash.Shockwaves.Internal.Waveform (Waveform (typeName), tRef)

import Clash.CircuitContext.Internal.Recorder (
  Kind (..),
  Recorder,
  Recording,
  Sig,
  Value,
  closeRecorder,
  newRecorder,
  newSig,
  recordCycle,
  recorderEnd,
  sigPeriod,
 )

import Control.Exception (
  SomeAsyncException,
  SomeException,
  evaluate,
  fromException,
  throwIO,
  try,
 )
import Control.Monad (forM, forM_)
import Data.Bits (xor, (.&.))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.IntMap.Strict as IntMap
import Data.List (intercalate, isPrefixOf)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import GHC.Natural (Natural)
import System.Directory (removePathForcibly)
import GHC.Stack (
  CallStack,
  HasCallStack,
  callStack,
  getCallStack,
  srcLocFile,
  srcLocModule,
  srcLocStartCol,
  srcLocStartLine,
 )
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)
import System.Mem.StableName (StableName, hashStableName, makeStableName)

{- | A simulation's recorder, as the context carries it: where the values go
(see "Clash.CircuitContext.Internal.Recorder"), and the taps of the traced
signals, which record one cell behind the simulation and so have to be
finished when it stops (see 'withRecording').
-}
data Live = Live
  { liveRecorder :: Recorder
  , liveTaps :: IORef [(Sig, IORef TraceTap)]
  }

{- | The state of one trace's tap: @TraceTap touched next rest@. Cycles
@[0, next)@ are recorded, and @rest@ is the packed signal from cycle @next@
on, not consumed yet. Keeping the REST rather than the HEAD of the packed
stream is what releases the raw signal history: as the design forces the
traced signal forward, the tap records each cycle and drops its reference to
the consumed prefix, so the raw per-cycle design values (full records,
unevaluated closures) become garbage at once instead of being kept until the
run ends.

@touched@ says whether the consumer forced the traced signal at all.
Recording runs one cell behind the simulation (see 'registerTrace'), so
after a run that forced ONLY the first cell the counters are the same as
for a signal nobody looked at, and that one cycle would be lost.
-}
data TraceTap = TraceTap !Bool !Int [Value]
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
  , ccRecorder :: Maybe Live
  -- ^ 'Nothing' disables tracing and probing
  , ccOrdinals :: Maybe (IORef OrdinalMap)
  {- ^ Per-simulation instance-ordinal registry; 'Nothing' disables
  instance tagging (fine while nothing is recorded)
  -}
  , ccFields :: [Int]
  {- ^ The position of the record field being traced, outermost first:
  @[1, 0]@ is field 0 of field 1. The leaves of one record trace share its
  call site, so this is what puts them in DECLARATION order in the VCD (see
  'fieldLoc'). Empty outside a record trace.
  -}
  }

type HasCircuitContext = ?circuitContext :: CircuitContext

-- | Tracing and probing disabled; hierarchy still works.
noCircuitContext :: CircuitContext
noCircuitContext = CircuitContext [] Nothing Nothing []

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
ordinal registry is the tell: 'withRecording' installs it, and it is
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
'withRecording' and all allocation happens inside registration IO.
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

{- | Encode the nearest call site IN THE DESIGN as a sort key, using only
characters safe inside hierarchy segments (no dots, no \@): line, column and
a file hash. Empty when no call stack is available.

Frames inside this library are skipped. Every public recorder reaches the
function that calls this through more 'HasCallStack' functions of its own
(@component@ calls @simComponent@, @autoTrace@ calls @traceSignalSW@, …), so
the newest frame is always one of those internal calls. Taking it gave every
instance the same call site, and sibling instances were then numbered in
evaluation order after all.

Frames of a generated default method (@$dm…@) are skipped too. An empty
@instance Traceable T@ uses the default 'traceNamed', and GHC records that
call at the instance declaration, which is in the design's module but is
not where the record is traced.
-}
callLoc :: CallStack -> String
callLoc cs = case dropWhile skipped (getCallStack cs) of
  (_, l) : _ ->
    "L"
      <> show (srcLocStartLine l)
      <> "C"
      <> show (srcLocStartCol l)
      <> "F"
      <> show (fileHash (srcLocFile l))
  [] -> ""
 where
  skipped (fn, l) =
    "$dm" `isPrefixOf` fn || "Clash.CircuitContext." `isPrefixOf` srcLocModule l
  fileHash :: String -> Word
  fileHash = List.foldl' (\h c -> h * 31 + fromIntegral (fromEnum c)) 5381

{- | A call site with the position of a record field added, as @P\<i\>@ per
level (see 'ccFields'). A call site without a location stays empty.
-}
fieldLoc :: [Int] -> String -> String
fieldLoc fields loc
  | null loc = loc
  | otherwise = loc <> concatMap (\i -> 'P' : show i) fields

{- | Run a simulation action under a fresh recording context, and return
what the action returned (or threw) with the 'Recording'. The recording's
values go to a spill file at this path while the simulation runs, so the
recorded history is not kept in memory. Delete the file with
'Clash.CircuitContext.Recording.discardRecording' when done.

The action must /force/ the parts of the circuit it wants recorded (sampling
does). When it stops, every trace is finished up to the time the furthest
recorded signal got, by evaluating the cycles it is missing: a trace records
a cycle when the NEXT cell is forced (see 'registerTrace'), and one signal
can be forced less far than another (a register's input, for one).

What the action recorded before it threw is in the recording too: that is
what the waveform of a failing test is made of. An asynchronous exception
(a cancelled test) is not caught: the spill file is deleted and the
exception goes on.
-}
withRecording ::
  FilePath ->
  ((HasCircuitContext) => IO r) ->
  IO (Either SomeException r, Recording)
withRecording = withRecordingFrom 0

{- | 'withRecording' that records only the cycles in progress at or after
this time, in ps: a domain's period times a cycle number. The earlier cycles
are evaluated, but nothing is written for them.
-}
withRecordingFrom ::
  Int ->
  FilePath ->
  ((HasCircuitContext) => IO r) ->
  IO (Either SomeException r, Recording)
withRecordingFrom start path k = do
  rec <- newRecorder path start
  taps <- newIORef []
  ords <- newIORef emptyOrdinals
  r <-
    try
      ( let ?circuitContext = CircuitContext [] (Just (Live rec taps)) (Just ords) []
         in k
      )
  case r of
    Left err | Just (_ :: SomeAsyncException) <- fromException err -> do
      _ <- closeRecorder rec 0
      removePathForcibly path
      throwIO err
    _ -> (,) r <$> finishRecording rec taps

{- | Finish every trace up to the end of the run, then close the recorder.

The end is the furthest time any signal was forced to. A trace's last forced
cycle is still in its tap ('TraceTap'), so it counts one cycle more than it
recorded. Finishing evaluates cycles the run did not force; a cycle that
throws ends that trace there (its later cycles show as not evaluated).
-}
finishRecording :: Recorder -> IORef [(Sig, IORef TraceTap)] -> IO Recording
finishRecording rec tapsRef = do
  taps <- readIORef tapsRef
  forced <- forM taps $ \(sig, ref) -> do
    TraceTap touched next _ <- readIORef ref
    pure (if touched then (next + 1) * sigPeriod sig else 0)
  recorded <- recorderEnd rec
  let end = maximum (recorded : forced)
  forM_ taps $ \(sig, ref) -> do
    TraceTap _ next rest <- readIORef ref
    let upto = (end + sigPeriod sig - 1) `div` sigPeriod sig
    try (forM_ (zip [next .. upto - 1] rest) (\(c, v) -> evaluate v >>= recordCycle rec sig c))
      >>= \(_ :: Either SomeException ()) -> pure ()
  closeRecorder rec end

{- | Declarations of intent the plugin reads from an @ANN@ pragma.

@ANN@ rather than a new pragma or a marker constraint because a Clash design
already annotates this way (@{\-\# ANN topEntity (Synthesize …) \#-\}@), and
because a mistyped constructor is a compile error — which is the point: a
marker that silently fails to register would reintroduce exactly the silence
these annotations exist to remove.

> {\-\# ANN runSystem NoCircuitScope \#-\}
> runSystem :: HasCircuitContext => … -- deliberately no OPAQUE
-}
data CircuitContextAnnotation
  = {- | This binder carries 'HasCircuitContext' and has no @OPAQUE@ pragma
    ON PURPOSE, so stop reporting it.

    The shape is a real one: a test harness or simulation driver takes the
    constraint in order to pass it down, and wants the design tree rooted
    at the waveform's top rather than nested under a driver's scope. It is
    indistinguishable, to the plugin, from the mistake of forgetting the
    pragma — so the difference has to be written down. Suppressing
    @-Wx-circuit-context-uninstrumented@ for the module would hide the
    mistake too; this hides only the binder you vouched for.
    -}
    NoCircuitScope
  deriving (Data, Eq, Show)

{- | @component "fifo" circuit@: everything traced or probed inside
@circuit@ is qualified by @…fifo.@.

Every application is a distinct /instance/: it allocates a fresh hierarchy
cell whose heap identity distinguishes it from every other instance of the
same name (resolved to ordinals at registration, see 'ordinalFor'), so
multiple instances of the same (sub)circuit under one parent never collide —
all are recorded, and the VCD writer disambiguates downstream (@fifo_0@,
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
instances instead of erroring; the VCD writer disambiguates downstream.

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
  | clashSimulation = simTraceSignalC Nothing nm sig
  | otherwise = sig
{-# OPAQUE traceSignalC #-}
{-# ANN traceSignalC hasBlackBox #-}
{-# ANN traceSignalC (InlineYamlPrimitive [minBound ..] (unlines
  [ "BlackBox:"
  , "  name: Clash.CircuitContext.Core.traceSignalC"
  , "  kind: Expression"
  , "  type: |-"
  , "      (HasCallStack, HasCircuitContext, KnownDomain dom, BitPack a, NFDataX a) => String -> Signal dom a -> Signal dom a"
  , "  template: ~ARG[6]"
  , "  workInfo: Never"
  ])) #-}

{- | 'traceSignalC' that also records the payload type's @clash-shockwaves@
descriptor, so a typed-waveform viewer shows constructors and fields rather
than raw bits (see "Clash.CircuitContext.Shockwaves").

Separate from 'traceSignalC' rather than replacing it, for the same reason
as 'probeSW': a payload 'BitPack' accepts is not always one
'Clash.Shockwaves.Waveform' accepts (its @Index n@ instance wants @1 <= n@,
@BitPack@'s only @KnownNat n@), and requiring a description would stop such
a signal from being recorded at all. The plugin's auto-tracing picks between
the two per payload type, so nothing has to be written to get descriptions;
see @Clash.CircuitContext.Auto.Describable@.
-}
traceSignalSW ::
  forall dom a.
  ( HasCallStack
  , HasCircuitContext
  , KnownDomain dom
  , BitPack a
  , NFDataX a
  , Waveform a
  ) =>
  String ->
  Signal dom a ->
  Signal dom a
traceSignalSW nm sig
  | clashSimulation = simTraceSignalC (Just (typeName @a, tRef @a)) nm sig
  | otherwise = sig
{-# OPAQUE traceSignalSW #-}
{-# ANN traceSignalSW hasBlackBox #-}
{-# ANN traceSignalSW (InlineYamlPrimitive [minBound ..] (unlines
  [ "BlackBox:"
  , "  name: Clash.CircuitContext.Core.traceSignalSW"
  , "  kind: Expression"
  , "  type: |-"
  , "      (HasCallStack, HasCircuitContext, KnownDomain dom, BitPack a, NFDataX a, Waveform a) => String -> Signal dom a -> Signal dom a"
  , "  template: ~ARG[7]"
  , "  workInfo: Never"
  ])) #-}

{- | Simulation worker for 'traceSignalC' and 'traceSignalSW'. OPAQUE, holding the sole
'unsafePerformIO' in the design (lazy one-shot registration, exactly like
stock 'Clash.Signal.Trace.traceSignal1'); identity when tracing is off.
-}
simTraceSignalC ::
  forall dom a.
  (HasCallStack, HasCircuitContext, KnownDomain dom, BitPack a, NFDataX a) =>
  -- | The payload's @clash-shockwaves@ descriptor, if the caller has one
  Maybe (TypeName, Translator) ->
  String ->
  Signal dom a ->
  Signal dom a
simTraceSignalC adt nm sig = case (ccRecorder ctx, ccOrdinals ctx) of
  (Just live, Just ordRef) ->
    -- Instance ordinals are resolved inside this same action — no id minting
    -- in pure code.
    unsafePerformIO $ do
      segs <- resolveHier ordRef (ccHier ctx)
      leafI <- freshOrdinal ordRef
      let key = intercalate "." (reverse (taggedLoc nm loc leafI : segs))
      -- The descriptor travels with the values: only the caller, where the
      -- payload type is still known, could build it.
      registerTrace live period adt key sig
  _ -> sig
 where
  ctx = ?circuitContext
  loc = fieldLoc (ccFields ctx) (callLoc callStack)
  period :: Int
  period = case knownDomain @dom of
    SDomainConfiguration{sPeriod} -> snatToNum sPeriod
{-# OPAQUE simTraceSignalC #-}

{- | Register a trace and return the signal TAPPED: forcing element /i/ of the
returned signal (which the design does as the simulation advances) also packs
that cycle's value and records it, after which the raw value is garbage.

This lockstep recording is the space-leak fix that has to live in the
infrastructure, not the user's code. The alternative (what stock
'Clash.Signal.Trace.traceSignal#' does) is to store a lazy
@map packMaskValue (sample_lazy sig)@ and force it at dump time — but holding
that list's HEAD while the design advances the shared signal retains every
cycle's raw design value until the dump: memory linear in simulation length,
with full-record-sized elements. The tap instead keeps only the not-consumed
packed TAIL ('TraceTap'), which references no history.

Value semantics are unchanged: @pack@ per sample via 'packMaskValue'
(undefined bits masked, exceptions rendered @x@), and 'sample_lazy' so a
partial binding is never forced outside the guard. Cycles the design never
forces are evaluated from the tail when the run ends ('finishRecording'). No
duplicate-key check: keys carry fresh leaf ordinals by construction.
-}
registerTrace ::
  forall dom a.
  (KnownDomain dom, BitPack a, NFDataX a) =>
  Live ->
  Int ->
  Maybe (TypeName, Translator) ->
  String ->
  Signal dom a ->
  IO (Signal dom a)
registerTrace live period adt key sig = do
  rsig <- newSig rec key TraceKind period width adt
  tapRef <- newIORef (TraceTap False 0 packed)
  atomicModifyIORef' (liveTaps live) (\ts -> ((rsig, tapRef) : ts, ()))
  pure (touchOnce tapRef (tap rsig tapRef 0 packed sig))
 where
  rec = liveRecorder live
  width = snatToNum (SNat @(BitSize a))
  packed = map packMaskValue (sample_lazy sig)

  -- Mark the tap TOUCHED when the consumer takes the signal's first cell.
  -- Recording proper runs one cell behind (see 'tap'), so without this a
  -- run that forced ONLY cell 0 leaves the tap in its initial state and
  -- 'finishRecording' cannot tell it from a signal nobody forced — a
  -- one-cycle simulation would capture nothing. Marking demands no design
  -- value, so it cannot re-enter a combinational knot.
  touchOnce :: IORef TraceTap -> Signal dom a -> Signal dom a
  touchOnce tapRef s =
    unsafePerformIO $ do
      atomicModifyIORef' tapRef $ \(TraceTap _ n rest) -> (TraceTap True n rest, ())
      pure s

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
  tap :: Sig -> IORef TraceTap -> Int -> [Value] -> Signal dom a -> Signal dom a
  tap rsig tapRef !cyc pk s = case (pk, s) of
    (v0 : pkRest, a :- as) ->
      a
        :- unsafePerformIO
          ( do
              !v <- evaluate v0
              recordCycle rec rsig cyc v
              atomicModifyIORef' tapRef (\_ -> (TraceTap True (cyc + 1) pkRest, ()))
              pure (tap rsig tapRef (cyc + 1) pkRest as)
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
  , pcCache :: IORef (Map.Map String Sig)
  {- ^ Per-INSTANCE cache: probe name → its accumulator. A probe fires every
  cycle, so its per-cycle cost must not include name qualification —
  resolving the hierarchy (StableName hashing per segment), building the
  qualified-name string, and inserting into a global map keyed by long
  strings are each cheap once but ruinous a million times per probe. The
  first firing resolves and registers; every later cycle is a lookup of a
  SHORT name in a map with a handful of entries plus one 'IORef' update.
  -}
  }

{- | A fresh probe cache for one mealy\/moore\/'probeFmap' INSTANCE. The
argument ties the allocation to the instance's identity object ('pcPath'):
a closed @unsafePerformIO (newIORef …)@ would be floated to the top level by
full laziness and SHARED across all instances, silently merging their probes
(observed: sibling instances losing wires in the golden). The free variable
pins the thunk inside each application; NOINLINE keeps the allocation out of
reach of inlining-enabled CSE.
-}
newProbeCache :: [HierSeg] -> IORef (Map.Map String Sig)
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
  | clashSimulation = simProbe Nothing nm a
  | otherwise = a
{-# OPAQUE probe #-}
{- The blackbox makes 'probe' an identity primitive in HDL, so Clash never
looks inside the @clashSimulation@ guard. Without it the dead 'simProbe'
branch reaches 'inlineOrLiftNonRep', which diverges on a step polymorphic in
a 'KnownNat' -- its dictionaries are non-representable local bindings -- and
HDL generation grows without bound.
OPAQUE keeps GHC from inlining the guard away, so the name is still there for
Clash to match. Simulation is unaffected: the blackbox is HDL-only.
-}
{-# ANN probe hasBlackBox #-}
{-# ANN probe (InlineYamlPrimitive [minBound ..] (unlines
  [ "BlackBox:"
  , "  name: Clash.CircuitContext.Core.probe"
  , "  kind: Expression"
  , "  type: |-"
  , "    probe :: (HasProbe, BitPack a, NFDataX a) => String -> a -> a"
  , "  template: ~ARG[4]"
  , "  workInfo: Never"
  ])) #-}

{- | 'probe' that also records the payload type's @clash-shockwaves@
descriptor, so the recorded cycles render as constructors and fields rather
than as raw bits.

Separate from 'probe' rather than replacing it, because the two requirements
genuinely differ. A probe's job is to see what a trace cannot — the state
INSIDE a mealy step — and that state is routinely a size-polymorphic record
that 'BitPack' accepts and 'Clash.Shockwaves.Waveform' does not (its
@Index n@ instance wants @1 <= n@ where @BitPack@ wants only @KnownNat n@).
Demanding a description of every probe would silence exactly those.

The plugin's auto-probing picks between the two per type, so a probed
binding whose type CAN be described is, without anything to write; see
@Clash.CircuitContext.Auto.Describable@.
-}
probeSW ::
  forall a. (HasProbe, BitPack a, NFDataX a, Waveform a) => String -> a -> a
-- 'tRef', not 'translator', here and in 'traceSignalSW': the reference form
-- is what registers the type itself under its name (a bare translator only
-- pulls in the types it REFERENCES), and it is what shockwaves' own recorder
-- stores per signal.
probeSW nm a
  | clashSimulation = simProbe (Just (typeName @a, tRef @a)) nm a
  | otherwise = a
{-# OPAQUE probeSW #-}
{-# ANN probeSW hasBlackBox #-}
{-# ANN probeSW (InlineYamlPrimitive [minBound ..] (unlines
  [ "BlackBox:"
  , "  name: Clash.CircuitContext.Core.probeSW"
  , "  kind: Expression"
  , "  type: |-"
  , "    probeSW :: (HasProbe, BitPack a, NFDataX a, Waveform a) => String -> a -> a"
  , "  template: ~ARG[5]"
  , "  workInfo: Never"
  ])) #-}

{- | Simulation worker for 'probe'. OPAQUE, holding the 'unsafePerformIO'
write; identity when probing is off.

OPAQUE for uniformity with the other simulation workers only; it is the
blackbox on 'probe' and 'probeSW' above that keeps HDL generation finite
(measured 2026-09-21: OPAQUE here alone changed nothing).
-}
simProbe ::
  forall a.
  (HasProbe, BitPack a, NFDataX a) =>
  Maybe (TypeName, Translator) ->
  String ->
  a ->
  a
simProbe adt nm a = case (ccRecorder ctx, ccOrdinals ctx) of
  (Just live, Just ordRef) ->
    unsafePerformIO $ do
      -- Hot path: this runs EVERY cycle. Only the first firing of a probe
      -- resolves its hierarchy and qualified name (see 'pcCache'); after
      -- that a cycle costs one small-map lookup and one comparison.
      cache <- readIORef (pcCache ?probe)
      rsig <- case Map.lookup nm cache of
        Just rsig -> pure rsig
        Nothing -> do
          segs <- resolveHier ordRef (ccHier ctx)
          mI <- ordinalFor ordRef (pcPath ?probe)
          let qual = intercalate "." (reverse (taggedLoc nm mLoc mI : segs))
          rsig <- newSig (liveRecorder live) qual ProbeKind per w adt
          atomicModifyIORef' (pcCache ?probe) $ \c ->
            (Map.insert nm rsig c, ())
          pure rsig
      -- Force the packed value NOW, releasing the design value 'a'. Probes
      -- fire in simulation order, and a cycle the probed expression never
      -- reaches leaves a gap, rendered @z@.
      let !v = val
      recordCycle (liveRecorder live) rsig cyc v
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
{-# OPAQUE simProbe #-}

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
