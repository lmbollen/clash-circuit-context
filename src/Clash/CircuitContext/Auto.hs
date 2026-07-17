{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Plugin ABI: everything "Clash.CircuitContext.Plugin" injects by exact 'Name'
lives here; module path and export names are frozen interface.

The 'CanTrace'\/'CanProbe' families have no equations — they are decided by
the plugin's typechecker half, which answers /"is @'Traceable' t@ (resp.
@'Probeable' t@) solvable right here?"/. All dictionary construction stays
with ordinary instance resolution; a type whose payload isn't traceable
(or a binding the context can't justify) silently falls back to identity
instead of erroring — the property plain instance selection cannot provide
(no backtracking on instance contexts).

'Traceable' is also the user-facing extension point: give your protocol
type an instance and plugin-instrumented bindings of that type get traced.
-}
module Clash.CircuitContext.Auto (
  -- * Tracing
  CanTrace,
  Traceable (..),
  AutoTrace (..),
  autoTrace,

  -- * Probing
  CanProbe,
  Probeable (..),
  AutoProbe (..),
  autoProbe,
) where

import Clash.Prelude (
  BitPack,
  KnownDomain,
  KnownNat,
  NFDataX,
  Signal,
  Vec,
  imap,
 )
import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)

import Clash.CircuitContext.Core (HasProbe, HasCircuitContext, probe, traceSignalC)

--------------------------------------------------------------------------------
-- Tracing
--------------------------------------------------------------------------------

-- | Bool-kinded oracle: reduced exclusively by the plugin, to ''True' iff
-- @Traceable t@ is solvable in the context of the occurrence.
type family CanTrace (t :: Type) :: Bool

-- | What it means for a type to be traceable; extension point for protocol
-- types.
class Traceable t where
  traceNamed :: (HasCallStack, HasCircuitContext) => String -> t -> t

instance
  (KnownDomain dom, BitPack a, NFDataX a, Typeable a) =>
  Traceable (Signal dom a)
  where
  traceNamed = traceSignalC

-- | Vectors of traceable things trace element-wise, names indexed by
-- structural position.
instance (KnownNat n, Traceable t) => Traceable (Vec n t) where
  traceNamed nm = imap (\i -> traceNamed (nm <> "_" <> show i))

-- | Flag-indexed dispatch; the flag comes from 'CanTrace'.
class AutoTrace (flag :: Bool) t where
  autoTraceAt :: (HasCallStack, HasCircuitContext) => String -> t -> t

instance Traceable t => AutoTrace 'True t where
  autoTraceAt = traceNamed

instance AutoTrace 'False t where
  autoTraceAt _ x = x

-- | What the renamer pass wraps around named local declarations in
-- 'HasCircuitContext' functions.
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

-- | Bool-kinded oracle: reduced exclusively by the plugin, to ''True' iff
-- @Probeable t@ is solvable in the context of the occurrence.
type family CanProbe (t :: Type) :: Bool

-- | The single universal instance carries exactly 'probe'\'s requirements;
-- the oracle decides @CanProbe t@ by checking ITS context's solvability.
class Probeable t where
  probeNamed :: HasProbe => String -> t -> t

instance (BitPack t, NFDataX t) => Probeable t where
  probeNamed = probe

class AutoProbe (flag :: Bool) t where
  autoProbeAt :: HasProbe => String -> t -> t

instance Probeable t => AutoProbe 'True t where
  autoProbeAt = probeNamed

instance AutoProbe 'False t where
  autoProbeAt _ x = x

-- | What the renamer pass wraps around named local declarations in
-- 'HasProbe' functions.
autoProbe ::
  forall t.
  (HasProbe, AutoProbe (CanProbe t) t) =>
  String ->
  t ->
  t
autoProbe = autoProbeAt @(CanProbe t) @t
{-# INLINE autoProbe #-}
