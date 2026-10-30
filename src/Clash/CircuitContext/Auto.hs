{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Plugin ABI: everything "Clash.CircuitContext.Plugin" injects by exact 'Name'
lives here; module path and export names are frozen interface.

The 'CanTrace'\/'CanProbe'\/'CanDescribe' families have no equations — they
are decided by the plugin's typechecker half, which answers /"is @'Traceable'
t@ (resp. @'Probeable' t@, @'Describable' t@) solvable right here?"/. All
dictionary construction stays with ordinary instance resolution; a type whose
payload isn't traceable (or a binding the context can't justify) falls back
to identity instead of erroring — the property plain instance selection
cannot provide (no backtracking on instance contexts). The fallback is not
silent: the oracle reports it, and distinguishes a proof that no instance
exists from its own inability to decide (see
"Clash.CircuitContext.Plugin.Diagnostics").

Three questions rather than one, because their answers differ. A payload can
be traceable or probeable and not describable — that is the common case for
size-polymorphic payloads and mealy state — and collapsing the questions
would mean either dropping those signals or leaving every signal as raw bits.
A declined description is reported as @x-circuit-context-undescribed@, not
as a lost signal: the signal is still recorded.

'Traceable' is also the user-facing extension point: give your protocol
type an instance and plugin-instrumented bindings of that type get traced.
-}
module Clash.CircuitContext.Auto (
  -- * Tracing
  CanTrace,
  Traceable (..),
  GTraceable (..),
  AutoTrace (..),
  autoTrace,

  -- * Probing
  CanProbe,
  Probeable (..),
  AutoProbe (..),
  autoProbe,

  -- * Describing a trace or probe
  CanDescribe,
  Describable (..),
  AutoDescribe (..),

  -- * Never recorded
  NonTranslatable,
) where

import Clash.Magic (SimOnly)
import Clash.Prelude (
  BitPack (..),
  DSignal,
  Enable,
  Index,
  KnownDomain,
  KnownNat,
  NFDataX,
  Reset,
  SNat (..),
  SNatLE (..),
  Signal,
  Vec,
  compareSNat,
  d1,
  fromEnable,
  imap,
  snatToInteger,
  succSNat,
  toEnable,
  toSignal,
  unsafeFromReset,
  unsafeFromSignal,
  unsafeToReset,
 )
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Tagged (Tagged (..))
import Data.Type.Bool (type (||))
import Data.Word (Word64)
import GHC.Generics (
  C,
  D,
  Generic (..),
  K1 (..),
  M1 (..),
  S,
  Selector (..),
  U1 (..),
  V1,
  (:*:) (..),
  (:+:) (..),
 )
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.TypeLits (type (+))
import Numeric.Natural (Natural)

import Clash.CircuitContext.Core (
  HasCircuitContext,
  HasProbe,
  probe,
  probeSW,
  traceSignalC,
  traceSignalSW,
 )
import Clash.Shockwaves.Waveform (
  DecSpacer,
  NumberFormat (NFSig, NFUns),
  Waveform,
  WaveformForNumber (..),
 )

--------------------------------------------------------------------------------
-- Tracing
--------------------------------------------------------------------------------

{- | Bool-kinded oracle: reduced exclusively by the plugin, to ''True' iff
@Traceable t@ is solvable in the context of the occurrence.
-}
type family CanTrace (t :: Type) :: Bool

{- | What it means for a type to be traceable; extension point for protocol
types.

For a record of traceable parts (signals, vectors, nested records) the
method derives generically: derive 'GHC.Generics.Generic' and write an
EMPTY instance —

> data Bus dom = Bus
>   { busAddr :: Signal dom (Unsigned 8)
>   , busStrobe :: Signal dom Bool
>   }
>   deriving (Generic)
>
> instance KnownDomain dom => Traceable (Bus dom)

A field @fld@ of a value traced as @nm@ is traced as @nm_fld@ — a sibling
wire, NOT a sub-scope: VCD scopes denote design hierarchy (components), and
a record is not a component. Positional (non-record) fields are named
@nm_0@, @nm_1@, … by position, exactly like 'Vec' elements; for a sum type
the constructor the value actually takes is traversed.

Fields are tolerated **individually**: the generic path asks only for
@'AutoTrace' ('CanTrace' c) c@ per field, which every type satisfies, so an
untraceable field degrades to identity on its own while its traceable
siblings still record. A composite is therefore never all-or-nothing — which
matters for protocol ports, whose halves routinely mix signals with
non-signal payloads (a @clash-protocols-memmap@ device port has
@Bwd (mm, wb) = (MemoryMap, Signal dom WishboneS2M)@; requiring 'Traceable'
of both once dropped that bus response entirely).

One thing to keep in mind for the plugin's auto-tracing of bindings of
such a type: the oracle recognizes the instance by recursing on its
WRITTEN context, so keep that context to ordinary class constraints, as
above — no @Generic@\/@GTraceable@ noise (they are default-method
constraints, not instance constraints, so this comes naturally).
-}
class Traceable t where
  traceNamed :: (HasCallStack, HasCircuitContext) => String -> t -> t
  default traceNamed ::
    (HasCallStack, HasCircuitContext, Generic t, GTraceable (Rep t)) =>
    String ->
    t ->
    t
  -- Freeze the stack so every leaf registers under the call site of the
  -- record trace itself (the auto-traced binding), keeping sibling
  -- disambiguation design-ordered.
  traceNamed nm t = withFrozenCallStack (to (snd (gtraceNamed nm 0 (from t))))

{- | The @AutoDescribe@ member upgrades the trace to a DESCRIBED one
('traceSignalSW') where the payload has a 'Waveform' instance, and constrains
nothing: the ''False' instance matches any payload. So @CanTrace (Signal dom
a)@ means what it always meant — 'KnownDomain', 'BitPack' and 'NFDataX' — and
a payload without 'Waveform' is still traced, as raw bits.
-}
instance
  (KnownDomain dom, BitPack a, NFDataX a, AutoDescribe (CanDescribe a) a) =>
  Traceable (Signal dom a)
  where
  traceNamed = traceDescribedAt @(CanDescribe a) @a

{- $integer
'Integer' and 'Natural' have no 'BitPack' instance, yet Clash translates
both: to 64 bits, two's complement for 'Integer'. A signal (or probe) of
either records exactly that, wrapping where the hardware would, so a design
computing in @Integer@ is still traced. These instances are INCOHERENT so
they can never get in the way of the general ones: they apply to 'Integer'
and 'Natural' themselves, and a payload polymorphic in @a@ keeps the general
instance and its 'BitPack' requirement. Inside a composite payload (@Maybe
Integer@) there is still no 'BitPack', so use @Signed 64@ there.
-}

-- | 'Integer' as Clash's hardware carries it.
newtype Integer64 = Integer64 Integer
  deriving newtype (NFDataX)

instance BitPack Integer64 where
  type BitSize Integer64 = 64
  pack (Integer64 i) = pack (fromInteger i :: Int64)
  unpack = Integer64 . toInteger . unpack @Int64

deriving via
  WaveformForNumber 'NFSig DecSpacer Integer64
  instance
    Waveform Integer64

-- | 'Natural' as Clash's hardware carries it.
newtype Natural64 = Natural64 Natural
  deriving newtype (NFDataX)

instance BitPack Natural64 where
  type BitSize Natural64 = 64
  pack (Natural64 n) = pack (fromIntegral n :: Word64)
  unpack = Natural64 . fromIntegral . unpack @Word64

deriving via
  WaveformForNumber 'NFUns DecSpacer Natural64
  instance
    Waveform Natural64

-- | See $integer.
instance {-# INCOHERENT #-} (KnownDomain dom) => Traceable (Signal dom Integer) where
  traceNamed nm = coerce . traceSignalSW @dom @Integer64 nm . coerce

-- | See $integer.
instance {-# INCOHERENT #-} (KnownDomain dom) => Traceable (Signal dom Natural) where
  traceNamed nm = coerce . traceSignalSW @dom @Natural64 nm . coerce

{- | The other wires: a delayed signal, a reset and an enable each trace as
the 'Signal' underneath them (a reset as its raw level, in the domain's own
polarity). They are newtypes, so this is a coercion and adds no strictness.
-}
instance (Traceable (Signal dom a)) => Traceable (DSignal dom d a) where
  traceNamed nm = unsafeFromSignal . traceNamed nm . toSignal

instance (KnownDomain dom) => Traceable (Reset dom) where
  traceNamed nm = unsafeToReset . traceSignalSW nm . unsafeFromReset

instance (KnownDomain dom) => Traceable (Enable dom) where
  traceNamed nm = toEnable . traceSignalSW nm . fromEnable

{- | Vectors of traceable things trace element-wise, names indexed by
structural position.
-}
instance (KnownNat n, Traceable t) => Traceable (Vec n t) where
  traceNamed nm = imap (\i -> traceNamed (nm <> "_" <> show i))

{- | Circuit-notation support. @clash-protocols@' parse-stage plugin desugars
every @x \<- comp -\< a@ in a @circuit@ block into pattern bindings of
@'Tagged' port (Fwd port)@ \/ @Tagged port (Bwd port)@ values — bindings this
plugin's renamer half already wraps (they carry the original port's source
span). With this instance those ports trace like any 'Signal' binding.

'Tagged' is a newtype: the match below is a coercion, adding no strictness.
That is load-bearing — the notation ties forward references (@-\<@ a port
bound by a later statement) through a plain lazy @let@ knot, so a trace must
never force a value before the design does.
-}
instance (Traceable t) => Traceable (Tagged p t) where
  traceNamed nm (Tagged t) = Tagged (traceNamed nm t)

{- | Nothing to record: @()@ is the @Bwd@ of every @CSignal@-style protocol
and a component of composite port types. The load-bearing property is NOT
matching the unit constructor — matching would force a circuit-notation knot
value before the design does.
-}
instance Traceable () where
  traceNamed _ u = u

{- $tupleInstances
Tuples trace component-wise through the generic default: positional
sub-names @nm_0@, @nm_1@, … (composite protocol ports: @Fwd (a, b) =
(Fwd a, Fwd b)@; @clash-protocols@ defines tuple protocols up to 12).
Demand-equivalent to identity: forcing the traced tuple to WHNF forces
exactly the one constructor match a @let@-pattern selector would force
anyway, and the components stay unforced 'traceNamed' thunks.
-}
instance (AutoTrace (CanTrace a) a, AutoTrace (CanTrace b) b) => Traceable (a, b)
instance (AutoTrace (CanTrace a) a, AutoTrace (CanTrace b) b, AutoTrace (CanTrace c) c) => Traceable (a, b, c)
instance
  (AutoTrace (CanTrace a) a, AutoTrace (CanTrace b) b, AutoTrace (CanTrace c) c, AutoTrace (CanTrace d) d) =>
  Traceable (a, b, c, d)
instance
  (AutoTrace (CanTrace a) a, AutoTrace (CanTrace b) b, AutoTrace (CanTrace c) c, AutoTrace (CanTrace d) d, AutoTrace (CanTrace e) e) =>
  Traceable (a, b, c, d, e)
instance
  (AutoTrace (CanTrace a) a, AutoTrace (CanTrace b) b, AutoTrace (CanTrace c) c, AutoTrace (CanTrace d) d, AutoTrace (CanTrace e) e, AutoTrace (CanTrace f) f) =>
  Traceable (a, b, c, d, e, f)
instance
  ( AutoTrace (CanTrace a) a
  , AutoTrace (CanTrace b) b
  , AutoTrace (CanTrace c) c
  , AutoTrace (CanTrace d) d
  , AutoTrace (CanTrace e) e
  , AutoTrace (CanTrace f) f
  , AutoTrace (CanTrace g) g
  ) =>
  Traceable (a, b, c, d, e, f, g)
instance
  ( AutoTrace (CanTrace a) a
  , AutoTrace (CanTrace b) b
  , AutoTrace (CanTrace c) c
  , AutoTrace (CanTrace d) d
  , AutoTrace (CanTrace e) e
  , AutoTrace (CanTrace f) f
  , AutoTrace (CanTrace g) g
  , AutoTrace (CanTrace h) h
  ) =>
  Traceable (a, b, c, d, e, f, g, h)
instance
  ( AutoTrace (CanTrace a) a
  , AutoTrace (CanTrace b) b
  , AutoTrace (CanTrace c) c
  , AutoTrace (CanTrace d) d
  , AutoTrace (CanTrace e) e
  , AutoTrace (CanTrace f) f
  , AutoTrace (CanTrace g) g
  , AutoTrace (CanTrace h) h
  , AutoTrace (CanTrace i) i
  ) =>
  Traceable (a, b, c, d, e, f, g, h, i)
instance
  ( AutoTrace (CanTrace a) a
  , AutoTrace (CanTrace b) b
  , AutoTrace (CanTrace c) c
  , AutoTrace (CanTrace d) d
  , AutoTrace (CanTrace e) e
  , AutoTrace (CanTrace f) f
  , AutoTrace (CanTrace g) g
  , AutoTrace (CanTrace h) h
  , AutoTrace (CanTrace i) i
  , AutoTrace (CanTrace j) j
  ) =>
  Traceable (a, b, c, d, e, f, g, h, i, j)
instance
  ( AutoTrace (CanTrace a) a
  , AutoTrace (CanTrace b) b
  , AutoTrace (CanTrace c) c
  , AutoTrace (CanTrace d) d
  , AutoTrace (CanTrace e) e
  , AutoTrace (CanTrace f) f
  , AutoTrace (CanTrace g) g
  , AutoTrace (CanTrace h) h
  , AutoTrace (CanTrace i) i
  , AutoTrace (CanTrace j) j
  , AutoTrace (CanTrace k) k
  ) =>
  Traceable (a, b, c, d, e, f, g, h, i, j, k)
instance
  ( AutoTrace (CanTrace a) a
  , AutoTrace (CanTrace b) b
  , AutoTrace (CanTrace c) c
  , AutoTrace (CanTrace d) d
  , AutoTrace (CanTrace e) e
  , AutoTrace (CanTrace f) f
  , AutoTrace (CanTrace g) g
  , AutoTrace (CanTrace h) h
  , AutoTrace (CanTrace i) i
  , AutoTrace (CanTrace j) j
  , AutoTrace (CanTrace k) k
  , AutoTrace (CanTrace l) l
  ) =>
  Traceable (a, b, c, d, e, f, g, h, i, j, k, l)

{- | Generic worker for the default 'traceNamed': walks the
'GHC.Generics.Rep' structure and traces every field under
@\<name\>.\<field\>@. The 'Int' threads the field position left to right
across a constructor, naming positional fields @_0@, @_1@, ….
-}
class GTraceable f where
  gtraceNamed ::
    (HasCallStack, HasCircuitContext) => String -> Int -> f p -> (Int, f p)

instance (GTraceable f) => GTraceable (M1 D meta f) where
  gtraceNamed nm i (M1 x) = M1 <$> gtraceNamed nm i x

instance (GTraceable f) => GTraceable (M1 C meta f) where
  gtraceNamed nm i (M1 x) = M1 <$> gtraceNamed nm i x

instance (GTraceable f, GTraceable g) => GTraceable (f :*: g) where
  gtraceNamed nm i0 (x :*: y) = (i2, x' :*: y')
   where
    (i1, x') = gtraceNamed nm i0 x
    (i2, y') = gtraceNamed nm i1 y

-- | Only the branch the value actually takes is traversed.
instance (GTraceable f, GTraceable g) => GTraceable (f :+: g) where
  gtraceNamed nm i (L1 x) = L1 <$> gtraceNamed nm i x
  gtraceNamed nm i (R1 y) = R1 <$> gtraceNamed nm i y

instance GTraceable U1 where
  gtraceNamed _ i U1 = (i, U1)

instance GTraceable V1 where
  gtraceNamed _ i v = (i, v)

{- | A leaf field: qualify the name with the record selector (or the field
position) and trace it.

Dispatched through 'autoTraceAt' rather than 'traceNamed', so the requirement
is @AutoTrace (CanTrace c) c@ — satisfiable for /every/ @c@ — instead of
@Traceable c@. That makes composites tolerant **per leaf**: an untraceable
field degrades to identity on its own, and its traceable siblings still
record.

The all-or-nothing alternative silently costs real signals. A
@clash-protocols-memmap@ device port is @(mm, wb)@, so its @Bwd@ is
@(MemoryMap, Signal dom WishboneS2M)@ — one non-signal component beside a
perfectly traceable bus response. Requiring @Traceable@ of every field made
@CanTrace@ of the pair 'False, dropping the whole backward half: 11 Wishbone
response buses missing from one bittide waveform, with no error to point at
the cause.
-}
instance (Selector s, AutoTrace (CanTrace c) c) => GTraceable (M1 S s (K1 r c)) where
  gtraceNamed nm i m@(M1 (K1 c)) =
    (i + 1, M1 (K1 (autoTraceAt @(CanTrace c) fieldName c)))
   where
    -- '_', never '.': a dot is the COMPONENT separator, and the VCD renderer
    -- turns every dotted segment into a '$scope'. Qualifying a field with one
    -- would claim the value is a design hierarchy level, which it is not — a
    -- tuple is not a module. It also read badly: 79% of the scopes in
    -- bittide's waveforms were structural rather than components, nearly all
    -- of them a scope wrapping one positional leaf ('wbB0_Fwd' containing only
    -- '_3'). Flat names keep the same information and match the 'Vec' instance
    -- above, which has always used '_'.
    fieldName = case selName m of
      "" -> nm <> "_" <> show i
      fld -> nm <> "_" <> fld

-- | Flag-indexed dispatch; the flag comes from 'CanTrace'.
class AutoTrace (flag :: Bool) t where
  autoTraceAt :: (HasCallStack, HasCircuitContext) => String -> t -> t

instance (Traceable t) => AutoTrace 'True t where
  autoTraceAt = traceNamed

instance AutoTrace 'False t where
  autoTraceAt _ x = x

{- | What the renamer pass wraps around named local declarations in
'HasCircuitContext' functions.
-}
autoTrace ::
  forall t.
  (HasCallStack, HasCircuitContext, AutoTrace (CanTrace t) t) =>
  String ->
  t ->
  t
autoTrace = autoTraceAt @(CanTrace t) @t
{-# INLINE autoTrace #-}

--------------------------------------------------------------------------------
-- Probing
--------------------------------------------------------------------------------

{- | Bool-kinded oracle: reduced exclusively by the plugin, to ''True' iff
@Probeable t@ is solvable in the context of the occurrence.
-}
type family CanProbe (t :: Type) :: Bool

{- | The single universal instance carries exactly 'probe'\'s requirements;
the oracle decides @CanProbe t@ by checking ITS context's solvability.
-}
class Probeable t where
  probeNamed :: (HasProbe) => String -> t -> t

{- | The @AutoDescribe@ member is what upgrades a probe to a DESCRIBED probe
where the payload allows it, and it constrains nothing: it is satisfiable for
every @t@ (the ''False' instance matches anything), exactly like the
per-field @AutoTrace@ in the composite 'Traceable' instances. So @CanProbe t@
still means what it always meant — 'BitPack' and 'NFDataX' — and a probe of a
type 'Waveform' cannot describe still records, as raw bits.
-}
instance
  (BitPack t, NFDataX t, AutoDescribe (CanDescribe t) t) =>
  Probeable t
  where
  probeNamed = probeDescribedAt @(CanDescribe t) @t

-- | See $integer.
instance {-# INCOHERENT #-} Probeable Integer where
  probeNamed nm = coerce . probeSW @Integer64 nm . coerce

-- | See $integer.
instance {-# INCOHERENT #-} Probeable Natural where
  probeNamed nm = coerce . probeSW @Natural64 nm . coerce

{- | An 'SNat' has no 'BitPack' (it is a type-level constant), but its value
is a number: it is probed as the @'Index' (n + 1)@ holding @n@, the narrowest
type that can. @SNat 0@ would be zero bits wide and is not recorded.

There is no instance context: the 'KnownNat' comes out of the 'SNat' itself,
so the oracle has nothing to prove, and an @SNat (len - tap)@ in a step
polymorphic in both probes as readily as a literal one. INCOHERENT for the
same reason as the 'Integer' instance (see $integer).
-}
instance {-# INCOHERENT #-} Probeable (SNat n) where
  probeNamed nm s@SNat = case (compareSNat d1 s, succSNat s) of
    (SNatLE, n1@SNat) -> case compareSNat d1 n1 of
      SNatLE -> probeSW @(Index (n + 1)) nm (fromInteger (snatToInteger s)) `seq` s
      SNatGT -> s
    (SNatGT, _) -> s

class AutoProbe (flag :: Bool) t where
  autoProbeAt :: (HasProbe) => String -> t -> t

instance (Probeable t) => AutoProbe 'True t where
  autoProbeAt = probeNamed

instance AutoProbe 'False t where
  autoProbeAt _ x = x

{- | What the renamer pass wraps around named local declarations in
'HasProbe' functions.
-}
autoProbe ::
  forall t.
  (HasProbe, AutoProbe (CanProbe t) t) =>
  String ->
  t ->
  t
autoProbe = autoProbeAt @(CanProbe t) @t
{-# INLINE autoProbe #-}

--------------------------------------------------------------------------------
-- Never recorded
--------------------------------------------------------------------------------

{- | ''True' for a type Clash cannot translate to hardware, so no trace or
probe can ever record it: a list, a function, @IO@, 'SimOnly'. The plugin
does not warn about a binding it declined to record when the binding's type,
or the requirement it stopped at, reduces to ''True' here — that warning
would have no fix.

Open, and only ever answered ''True' (or passed through a container): a type
nobody declared stays unreduced, and a binding of it that cannot be recorded
still warns, prompting for the instance it is missing. For a type of your
own that Clash cannot translate, declare it next to the type —

> data Config = Config {names :: [String], limit :: Int}
>
> type instance NonTranslatable Config = 'True

'Integer' and 'Natural' are NOT in here: Clash translates both, to 64 bits,
and they are recorded as such (see $integer).

The containers below pass the question on to their contents. Tuples do so up
to eight components; a wider tuple holding a list still warns, and a
@type instance@ for it settles that.
-}
type family NonTranslatable (t :: Type) :: Bool

type instance NonTranslatable [a] = 'True
type instance NonTranslatable (NonEmpty a) = 'True
type instance NonTranslatable (a -> b) = 'True
type instance NonTranslatable (IO a) = 'True

-- | Clash translates 'SimOnly' to nothing at all: it is not hardware.
type instance NonTranslatable (SimOnly a) = 'True

-- Containers Clash translates are untranslatable when their contents are.
type instance NonTranslatable (Signal dom a) = NonTranslatable a
type instance NonTranslatable (DSignal dom d a) = NonTranslatable a
type instance NonTranslatable (Vec n a) = NonTranslatable a
type instance NonTranslatable (Maybe a) = NonTranslatable a
type instance NonTranslatable (Either a b) = NonTranslatable a || NonTranslatable b
type instance NonTranslatable (a, b) = NonTranslatable a || NonTranslatable b
type instance
  NonTranslatable (a, b, c) =
    NonTranslatable a || NonTranslatable b || NonTranslatable c
type instance
  NonTranslatable (a, b, c, d) =
    NonTranslatable a || NonTranslatable b || NonTranslatable c || NonTranslatable d
type instance
  NonTranslatable (a, b, c, d, e) =
    NonTranslatable a || NonTranslatable (b, c, d, e)
type instance
  NonTranslatable (a, b, c, d, e, f) =
    NonTranslatable a || NonTranslatable (b, c, d, e, f)
type instance
  NonTranslatable (a, b, c, d, e, f, g) =
    NonTranslatable a || NonTranslatable (b, c, d, e, f, g)
type instance
  NonTranslatable (a, b, c, d, e, f, g, h) =
    NonTranslatable a || NonTranslatable (b, c, d, e, f, g, h)

--------------------------------------------------------------------------------
-- Describing a trace or probe
--------------------------------------------------------------------------------

{- | Bool-kinded oracle: reduced exclusively by the plugin, to ''True' iff
@Describable t@ is solvable in the context of the occurrence.
-}
type family CanDescribe (t :: Type) :: Bool

{- | A traced or probed payload @clash-shockwaves@ can describe.

Recording and describing are decided SEPARATELY because their requirements
genuinely differ: recording wants 'BitPack' and 'NFDataX', describing wants
'Waveform', and a size-polymorphic payload routinely satisfies the first and
not the second (@Waveform (Index n)@ wants @1 <= n@ where @BitPack@ wants
only @KnownNat n@). Deciding them together would mean either dropping those
signals or leaving every signal undescribed; deciding them apart means each
signal gets as much as its type allows.

The plugin also reads @clash-shockwaves@' 'Waveform' class off this
instance's context, to load that package's instances in modules that never
mention it (see "Clash.CircuitContext.Plugin.Oracle").
-}
class Describable t where
  traceDescribed ::
    (HasCallStack, HasCircuitContext, KnownDomain dom, BitPack t, NFDataX t) =>
    String ->
    Signal dom t ->
    Signal dom t
  probeDescribed :: (HasProbe, BitPack t, NFDataX t) => String -> t -> t

instance (Waveform t) => Describable t where
  traceDescribed = traceSignalSW
  probeDescribed = probeSW

-- | Flag-indexed dispatch; the flag comes from 'CanDescribe'.
class AutoDescribe (flag :: Bool) t where
  traceDescribedAt ::
    (HasCallStack, HasCircuitContext, KnownDomain dom, BitPack t, NFDataX t) =>
    String ->
    Signal dom t ->
    Signal dom t
  probeDescribedAt :: (HasProbe, BitPack t, NFDataX t) => String -> t -> t

instance (Describable t) => AutoDescribe 'True t where
  traceDescribedAt = traceDescribed
  probeDescribedAt = probeDescribed

-- | Recorded, but as bits: 'traceSignalC' and 'probe' carry no descriptor.
instance AutoDescribe 'False t where
  traceDescribedAt = traceSignalC
  probeDescribedAt = probe
