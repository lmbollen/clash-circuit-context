{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Regression cases from a downstream audit, lifted from the reproducers that
came with it and inverted: each was written to REPRODUCE a defect at pin
@f5431d7@, and asserts the fix here. (Relicensed on the authors' own
instruction — their note says the content is deliberately generic and carries
nothing proprietary. The shapes are theirs; the wording is ours.)

Kept apart from "Test.PluginDiagnostics", which showcases the warning
categories on purpose-built shapes. These are the shapes a real design
actually had, which is a different kind of evidence and worth not blurring:
every one of them was a wire that went missing, or a warning that could not be
turned off without turning off the ones that mattered.

The methodological warning in their notes is worth repeating, because it cost
them real time and it applies to every test in this file: __a module with a
type error emits no @x-circuit-context@ warning either__, so grepping for the
absence of a warning reads a broken module as a passing one. Every assertion
here is therefore either a RECORDED PATH (which cannot be faked by a module
that failed to build, since it would not link) or a check that @check.sh@
performs only after the build succeeded.
-}
module Test.Downstream (
  -- * F1: a satisfiable constraint reported as proved-absent
  f1,
  f1Out,
  planeWidth,
  explicit,

  -- * F2: an @Assert@ that holds at a literal
  f2,
  f2Out,

  -- * F4: a boundary that means to have no scope
  runHarness,
  runHarnessUnvouched,

  -- * F6: the closed-binding silence, and double registration
  signedClosed,
  handWritten,
) where

import qualified Clash.Explicit.Prelude as E
import Clash.Prelude
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))

import Clash.CircuitContext (
  CircuitContextAnnotation (NoCircuitScope),
  HasCircuitContext,
  traceSignalC,
 )
import Clash.Shockwaves.Waveform (Waveform)

--------------------------------------------------------------------------------
-- F1: a satisfiable constraint reported as proved-absent
--------------------------------------------------------------------------------

-- | @BitSize Plain@ = 32 + 1 = 33.
data Plain = Plain
  { p :: Unsigned 32
  , q :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (BitPack, NFDataX, Waveform)

{- | The reported failure and its own control, in one component.

At pin @f5431d7@ this emitted

> not traced: Signal System (Plain, Bool, BitVector 8)
>   no instance for: KnownNat (BitSize Plain)

in the @untraced@ category — whose documented meaning is that the oracle
PROVED there is no instance. It had proved nothing: @BitSize Plain@ is a
type-family application, the oracle did not reduce families, and a @KnownNat@
it could not compute looked exactly like one that cannot hold.

What makes this case airtight is that the disproof is in the same module
under the same flags. @direct@ carries the same record and traced fine, so it
is not the payload type; 'planeWidth' and 'explicit' below compile, so the
constraint is satisfiable. If the oracle's claim had been a fact, neither
would build.

Both bindings must now be recorded — @f1.direct@ and @f1.tup@.
-}
f1 ::
  (HiddenClockResetEnable dom, HasCircuitContext) =>
  Signal dom (BitVector 8) ->
  Signal dom (BitVector 8)
f1 xs = liftA2 combine direct tup
 where
  -- CONTROL: the same record, bare. Records before the fix and after it, so
  -- a failure here means the payload became untraceable rather than the
  -- oracle regressing.
  --
  -- Written to DEPEND on @xs@ on purpose. As supplied it was
  -- @pure Plain{…}@, which is a CLOSED binding: the plugin skips those to
  -- keep GHC's generalisation of polymorphic locals intact, so it was never
  -- wrapped, the oracle was never asked, and the absence of a warning said
  -- nothing at all. A control has to be a binding that actually records.
  direct = fmap (const Plain{p = 1, q = True}) xs

  -- REGRESSION: the same record inside a tuple, which is what drags
  -- @KnownNat (BitSize Plain)@ into BitPack's instance context.
  tup = fmap ((,,) Plain{p = 2, q = False} True) xs

  -- Takes arguments, so it is not a traced binding either way.
  combine d (_, _, z) = resize (pack (p d)) `xor` z
{-# OPAQUE f1 #-}

-- | 'f1' driven, so a recording test needs no clock plumbing of its own.
f1Out :: (HasCircuitContext) => Signal System (BitVector 8)
f1Out = withClockResetEnable clockGen resetGen enableGen (f1 (pure 3))

-- | The constraint the old warning named, discharged. Evaluates to 33.
planeWidth :: Integer
planeWidth = natVal (Proxy @(BitSize Plain))

{- | 'traceSignalC' takes @Traceable@ as a real constraint rather than a
guess, so this compiling is a proof about the very payload the auto-trace
declined.
-}
explicit ::
  (HasCircuitContext) =>
  Signal System (Plain, Bool, BitVector 8) ->
  Signal System (Plain, Bool, BitVector 8)
explicit = traceSignalC "explicit"

--------------------------------------------------------------------------------
-- F2: an Assert that holds at a literal
--------------------------------------------------------------------------------

{- | @Index 16@, reported as

> the oracle could not decide: 1 <= 16

against the consumer's toolchain. @1 <= n@ is GHC's @Assert@ type family
rather than a class, and the oracle gave up on it — at a LITERAL, so "the
payload is monomorphic, therefore it traces" was not a usable rule. It costs
them @cache.heldOffset@ and @processor.bodyPtr@.

A caveat this test carries on its face: __the literal @Index 16@ does not
reproduce on this branch even at the old pin__, because the @clash-shockwaves@
vendored here already relaxes @Waveform (Index n)@ to @BitPack@'s @KnownNat n@,
so the @Assert@ is never asked. Their pin took shockwaves from Hackage, where
it is still @1 <= n@. So this case guards the downstream SHAPE, and
@Test.PluginDiagnostics@'s @grounded@ guards the MECHANISM on a payload whose
@Waveform@ context carries the @Assert@ regardless of which shockwaves is in
scope. Neither alone would have caught the regression; deleting either because
the other exists is how this stops being tested.

@f2.ix@ must be recorded.
-}
f2 ::
  (HiddenClockResetEnable dom, HasCircuitContext) =>
  Signal dom (Unsigned 8) ->
  Signal dom (Unsigned 8)
f2 xs = fmap (fromIntegral . fromEnum) ix
 where
  ix = fmap (\x -> toEnum (fromIntegral x `mod` 16) :: Index 16) xs
{-# OPAQUE f2 #-}

-- | 'f2' driven. See 'f1Out'.
f2Out :: (HasCircuitContext) => Signal System (Unsigned 8)
f2Out = withClockResetEnable clockGen resetGen enableGen (f2 (pure 5))

--------------------------------------------------------------------------------
-- F4: a boundary that means to have no scope
--------------------------------------------------------------------------------

{- | A harness boundary: it carries 'HasCircuitContext' and has no @OPAQUE@ ON
PURPOSE, so the design under it records at the waveform ROOT rather than
nested inside a driver's scope. Taking the warning's advice would add exactly
the scope being avoided.

Their only remedy had been @-Wno-x-circuit-context-uninstrumented@, which also
masks the case the category exists to catch — and they had shipped that very
bug (a component that lost its @OPAQUE@, rooting its whole subtree one level
too high). The annotation says which one this is, so the category stays
promotable to @-Werror@ everywhere else.

Asserted by @check.sh@: no warning names this binder.
-}
{-# ANN runHarness NoCircuitScope #-}
runHarness :: (HasCircuitContext) => Signal System (Unsigned 8)
runHarness = harnessOut
 where
  -- Depends on @driven@, so it is an OPEN binding and records. @driven@
  -- itself is closed and skipped, which is why the reproducer's two-binding
  -- shape is kept rather than inlined: collapsing them leaves the harness
  -- with nothing to record and the test asserting nothing.
  --
  -- Explicit clock rather than the reproducer's hidden one: the harness
  -- property under test has nothing to do with clock plumbing, and a local
  -- @HiddenClockResetEnable System@ signature only draws
  -- @-Wsimplifiable-class-constraints@.
  harnessOut = E.register clockGen resetGen enableGen 0 driven
  driven = pure 7

{- | The same shape with no annotation, which must STILL warn.

The pair is the assertion. A marker that silenced the category rather than the
binder would leave this quiet too, and that is indistinguishable from the
feature working until the day it costs someone a scope.
-}
runHarnessUnvouched :: (HasCircuitContext) => Signal System (Unsigned 8)
runHarnessUnvouched = harnessOut
 where
  harnessOut = E.register clockGen resetGen enableGen 0 driven
  driven = pure 9

--------------------------------------------------------------------------------
-- F6: the closed-binding silence, and double registration
--------------------------------------------------------------------------------

{- | A CLOSED local binding — one with no free local variables — used to be
skipped before the oracle was ever consulted, so it produced neither a wire
nor a warning. That is the blind spot the audit turned up: the warnings are
not a complete inventory of untraced bindings, which is why their one genuine
missing wire was found by diffing a VCD rather than by reading a build log.

The skip exists because GHC generalises closed bindings even under
@MonoLocalBinds@, and the injected constraint carries a @CanTrace@ family
application that GHC will not quantify over in an INFERRED type — the binder
gets monomorphised at its first use and a second use at another type becomes a
baffling error.

Their proposal, adopted here: __an explicit type signature already pins the
type__, so the hazard is absent exactly where the author was explicit. A closed
binding that carries its own signature is wrapped; an unsigned one is still
skipped. It asks for ordinary Haskell rather than another marker, and it
converts silence into a wire precisely where somebody was deliberate enough to
expect one.

@signedClosed.constant@ must be recorded. @signedClosed.polymorphic@ must NOT
be — and, more importantly, the module must COMPILE: that binding is the F9
hazard itself, closed and used at two different types, and it is the case that
decides whether a signature really is enough.
-}
signedClosed :: (HasCircuitContext) => Signal System Int -> Signal System Int
signedClosed inp = out
 where
  out =
    inp
      + (fromIntegral <$> constant)
      + (maybe 0 fromEnum <$> asBool)
      + (fromMaybe 0 <$> asInt)

  -- Closed, monomorphic signature: traced now.
  constant :: Signal System (Unsigned 8)
  constant = pure 7

  -- Closed, POLYMORPHIC signature, used at two types below. Wrapped, and the
  -- oracle declines it (no @BitPack a@ for a skolem), so it falls back to
  -- identity rather than monomorphising the binder. If a signature were NOT
  -- enough, this module would not build.
  polymorphic :: Signal System (Maybe a)
  polymorphic = pure Nothing

  asBool = polymorphic :: Signal System (Maybe Bool)
  asInt = polymorphic :: Signal System (Maybe Int)
{-# OPAQUE signedClosed #-}

{- | Hand-written recording combinators are not doubled.

Once the oracle started deciding payloads it used to decline, four explicit
@traceSignalC@ calls in the consumer's design became redundant and each began
recording its signal TWICE — rendered @name_0@\/@name_1@ — so they were
deleted. They need not have been: a binding whose right-hand side already
applies a recorder under the binder's own name does not get another injected
on top.

The name has to match. @renamed@ below writes a DIFFERENT name, which is two
signals the author asked for, and both are kept — the same reason their
@switch@ kept its @traceSignalC "out"@, which names a function's result rather
than a where-binding.

Recorded: @handWritten.out@ exactly once, plus @handWritten.inner@ and
@handWritten.renamed@.
-}
handWritten :: (HasCircuitContext) => Signal System Int -> Signal System Int
handWritten inp = out + renamed
 where
  out = traceSignalC "out" (inp + 1)
  renamed = traceSignalC "inner" (inp + 2)
{-# OPAQUE handWritten #-}
