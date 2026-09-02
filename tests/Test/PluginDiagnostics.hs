{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- DELIBERATE: the suite's cabal stanza ALREADY enables the plugin, so this
-- pragma enables it a second time and the renamer pass runs twice over this
-- module. It used to nest every component wrap in itself (@switch.switch@);
-- the "scope doubled" check in main is the regression test for the
-- idempotence that replaced it. Do not "clean this up".
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}

{- | What the plugin used to LOSE silently, and what it now SAYS.

Every check here was a clean compile with a wire or a scope quietly missing,
which is the expensive failure mode of an opt-in-by-signature plugin: nothing
to grep for, and a design that looks wrong instead of a build that looks
wrong.

Two halves, both exercised by this one module:

* what the plugin now SEES — a 'HasCircuitContext' behind a constraint
  synonym, local ('LocalCtx') or imported ('Test.DesignCtx.DesignCtx'), still
  makes its function a component; and running the renamer twice (this module
  enables the plugin twice, see the pragma above) changes nothing.

* what the plugin now SAYS — every near-miss below is a GHC warning at
  compile time, and this module holds one live example of each category:
  @x-circuit-context-uninstrumented@ (@noOpaque@, @unsigned@, @bothModes@),
  @x-circuit-context-untraced@ (@label@, @untraceable@) and
  @x-circuit-context-undecided@ (@held@, whose 'Awkward' payload has a
  'Waveform' instance the oracle cannot recurse into). @check.sh@ greps the
  build log for all three, because a diagnostic that stops firing is exactly
  as silent as the bug it reports.
-}
module Main where

import qualified Prelude as P

import Control.Exception (evaluate)
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)

import Clash.Explicit.Prelude

import Clash.CircuitContext
import Clash.Shockwaves.Internal.Waveform (Waveform)
import Test.DesignCtx (DesignCtx)

{- | A constraint synonym declared in the module being compiled: its
right-hand side is read from the group, since it is not in the type
environment yet at renamer stage.
-}
type LocalCtx dom = (KnownDomain dom, HasCircuitContext)

{- | A payload with no 'Clash.Shockwaves.Waveform' (indeed no 'BitPack')
instance: the binding below it is dropped, and says which requirement did it.
-}
newtype Undescribed = Undescribed Int
  deriving (Generic, NFDataX)

{- | A payload whose 'Waveform' instance context is NOT ordinary class
constraints: @1 <= n@ is GHC's @Assert@ type family, which the oracle cannot
recurse into. It does not answer "no instance" — it gives up, which is a
different thing to be told, and the reason
@x-circuit-context-undecided@ is its own category.
-}
newtype Awkward n = Awkward (Unsigned n)
  deriving (Generic, Show)
  deriving newtype (BitPack, NFDataX)

deriving anyclass instance (KnownNat n, 1 <= n) => Waveform (Awkward n)

unAwkward :: (KnownNat n) => Awkward n -> Int
unAwkward (Awkward u) = fromIntegral u

{- | A payload whose @BitSize@ is a TYPE FAMILY application rather than
something the oracle can read off. Clash's tuple 'BitPack' instances carry
@KnownNat (BitSize a)@ in their context, so a tuple containing this asks the
oracle a question it can only answer by reducing the family.

Helios F1: it used to answer "no instance" — reporting a guess as a proof,
and losing the wire. The oracle reduces families now, so this traces.
-}
type family Width a :: Nat

type instance Width Bool = 4

newtype Tagged = Tagged (Unsigned 4)
  deriving (Generic, Show)
  deriving newtype (NFDataX)
  deriving anyclass (Waveform)

instance BitPack Tagged where
  type BitSize Tagged = Width Bool
  pack (Tagged u) = pack u
  unpack = Tagged . unpack

thd :: (a, b, c) -> c
thd (_, _, c) = c

-- | Component via a LOCAL constraint synonym.
viaLocalSyn :: (LocalCtx dom) => Signal dom Int -> Signal dom Int
viaLocalSyn inp = out
 where
  out = inp + 1
{-# OPAQUE viaLocalSyn #-}

{- | Component via an IMPORTED constraint synonym, and the module's two
oracle diagnostics: @untraceable@ has no 'BitPack' instance, @label@ is not a
signal at all.
-}
viaImportedSyn :: (DesignCtx dom) => Int -> Signal dom Int -> Signal dom Int
viaImportedSyn k inp = out
 where
  out = inp + 2 + pure (P.length label)
  label = P.replicate k 'x'
  _forced = untraceable
  untraceable = Undescribed <$> inp
{-# OPAQUE viaImportedSyn #-}

{- | Helios F1 regression: the payload's traceability turns on
@KnownNat (BitSize Tagged)@, a type-family application. Reported as a proved
absence before the oracle could reduce families; @tupled@ must be in the
recorded paths.
-}
carriesFamily ::
  (LocalCtx dom) =>
  Signal dom (Tagged, Bool, Unsigned 8) ->
  Signal dom (Tagged, Bool, Unsigned 8)
carriesFamily inp = tupled
 where
  tupled = inp
{-# OPAQUE carriesFamily #-}

{- | The GROUND half of Helios F2. @1 <= 8@ is the same @Assert@ family as
@1 <= n@, and it reduces: "monomorphic is safe" became a usable heuristic
once the oracle normalised before giving up. Contrast 'sized', where @n@ is a
skolem and the honest answer is still undecided.
-}
grounded :: (LocalCtx dom) => Signal dom (Awkward 8) -> Signal dom (Awkward 8)
grounded inp = eight
 where
  eight = inp
{-# OPAQUE grounded #-}

{- | Helios F4: a harness boundary that carries the constraint and
deliberately has no @OPAQUE@, vouched for by the annotation. It must draw no
warning, while 'noOpaque' — the same shape, unvouched — still does. That pair
is the point: suppressing the category for the module would have hidden both.
-}
{-# ANN vouchedFlat NoCircuitScope #-}
vouchedFlat :: (HasCircuitContext) => Signal System Int -> Signal System Int
vouchedFlat inp = rooted
 where
  rooted = inp + 11

{- | A tuple PATTERN binding under a synonym context. Each binder is renamed
fresh and given a sibling @a = autoTrace "a" a'@ — an operation that is NOT
idempotent by construction (a second pass would alias the alias), so the
second pass recognises its own work instead. Both names must appear exactly
once.
-}

{- | A size-polymorphic component whose local binding the oracle cannot
decide: @Waveform (Awkward n)@ needs @1 <= n@, and only @KnownNat n@ is
given here.
-}
sized ::
  (LocalCtx dom, KnownNat n) => Signal dom (Awkward n) -> Signal dom (Awkward n)
sized inp = held
 where
  held = inp
{-# OPAQUE sized #-}

viaTuplePat :: (LocalCtx dom) => Signal dom Int -> Signal dom Int
viaTuplePat inp = a + b
 where
  (a, b) = (inp + 3, inp + 4)
{-# OPAQUE viaTuplePat #-}

{- | 'HasCircuitContext' without @OPAQUE@: still traced, but into the
CALLER's scope — @out2@ below lands in @top@, not in a scope of its own.
Diagnosed rather than fixed: a small helper flattening into its caller is
often what you want, and the plugin cannot tell which case this is.
-}
noOpaque :: (HasCircuitContext) => Signal System Int -> Signal System Int
noOpaque inp = out2
 where
  out2 = inp + 5

{- | Both constraints on one signature: probe mode wins and a probed binder
never becomes a component, so this is a component that silently is not one.
Never called — the diagnostic is the point.
-}
bothModes :: (HasCircuitContext, HasProbe) => Signal System Int -> Signal System Int
bothModes inp = out3
 where
  out3 = inp + 6
{-# OPAQUE bothModes #-}

{- | @OPAQUE@ but unsigned: the mode is read from the signature, so there is
nothing to read and this is never instrumented.
-}
unsigned = (+ 7)
{-# OPAQUE unsigned #-}

top :: (HasCircuitContext) => Signal System Int -> Signal System Int
top inp =
  viaLocalSyn inp
    + viaImportedSyn 3 inp
    + viaTuplePat inp
    + noOpaque (unsigned inp)
    + (unAwkward <$> sized (Awkward . fromIntegral <$> inp :: Signal System (Awkward 8)))
    + vouchedFlat inp
    + (unAwkward <$> grounded (Awkward . fromIntegral <$> inp))
    + (fromIntegral . thd <$> carriesFamily ((Tagged 1,True,) . fromIntegral <$> inp))
{-# OPAQUE top #-}

main :: IO ()
main = do
  (_, traces, _) <- withCircuitContext $ do
    let xs = sampleN 8 (top (fromList [1, 1 ..]))
    _ <- evaluate (deepseqX xs xs)
    pure xs
  let paths = P.map (P.map segmentName . splitOn '.') (Map.keys traces)
  putStrLn "recorded paths:"
  mapM_ (putStrLn . ("  " <>) . show) paths
  let
    failures =
      P.concat
        [ [ "constraint synonym not seen: expected " <> show p
          | p <- [["top", "viaLocalSyn", "out"], ["top", "viaImportedSyn", "out"]]
          , p `P.notElem` paths
          ]
        , [ "scope doubled (the pass is not idempotent): " <> show p
          | p <- paths
          , P.or (P.zipWith (==) p (P.drop 1 p))
          ]
        , [ "pattern binder " <> nm <> " recorded " <> show n <> " times, expected 1"
          | nm <- ["a", "b"]
          , let n = P.length [p | p <- paths, P.last p == nm]
          , n /= 1
          ]
        , [ "an OPAQUE-less binding did not flatten into its caller: "
              <> show ["top" :: String, "out2"]
          | ["top", "out2"] `P.notElem` paths
          ]
        ]
  if P.null failures
    then putStrLn "OK"
    else do
      mapM_ putStrLn failures
      exitFailure

{- | The name part of a recorded segment, dropping the @\@ordinal\@location@
disambiguation tag 'Clash.CircuitContext.Core.dumpVCDC' resolves later.
-}
segmentName :: String -> String
segmentName = P.takeWhile (/= '@')

splitOn :: Char -> String -> [String]
splitOn c s = case P.break (== c) s of
  (chunk, []) -> [chunk]
  (chunk, _ : rest) -> chunk : splitOn c rest
