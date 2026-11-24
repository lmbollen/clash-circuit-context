{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The waveform of a Hedgehog property: of the counterexample Hedgehog reports,
and, if you ask for it, of one passing case.

> prop_counter :: Property
> prop_counter = property $ do
>   xs  <- forAll genInputs
>   out <- simulateWaveform defaultOptions Counterexample "waveforms/counter" (0, length xs) (top (fromList xs))
>   out === model xs

Every case simulates without recording, so a property that passes costs
nothing extra. When it fails, Hedgehog's report ends with
@waveform: \/abs\/path\/waveforms\/counter.fst@: the waveform of the case it
reports, the smallest counterexample. The options are those of
"Clash.CircuitContext.Waveform", re-exported here.

Give each call of 'simulateWaveform' its own path, also two calls in one
property: one path is one pair of files, so of two calls with one path
(however it is spelled) one waveform is lost, and the report names the same
file for both. Nothing checks this.

== How the counterexample is recorded

A property does not know which of its cases Hedgehog will report: every case
that fails is shrunk further, and the case Hedgehog reports is known only when
shrinking ends. Each case therefore leaves a footnote that, when it is
PRINTED, simulates that case again with recording and writes the waveform.
Hedgehog prints footnotes only for the case it reports, so exactly that case
is recorded, once. The footnote of every other case is never printed, so it
never runs.

This relies on how Hedgehog evaluates reports (hedgehog 1.5: progress during
shrinking does not print footnotes, and a footnote's text is evaluated only
when it is printed), which the test suite of this package checks. A runner
that never prints the failure (one that only reads 'checkReport''s status)
gets no waveform.

The second simulation must give the same result as the first. That holds for
a design whose inputs all come from the property.
-}
module Clash.CircuitContext.Waveform.Hedgehog (
  -- * Options
  WaveformOptions (..),
  Format (..),
  Spill (..),
  defaultOptions,

  -- * A property's waveform
  Capture (..),
  simulateWaveform,
) where

import Control.Exception (SomeException, displayException, evaluate, throwIO, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)

import Hedgehog (PropertyT, Size)
import Hedgehog.Internal.Property (PropertyT (PropertyT), footnote)

import qualified Hedgehog.Gen as Gen

import Clash.Explicit.Signal (Signal, sampleN)
import Clash.Prelude (KnownDomain, NFDataX, deepseqX)

import Clash.CircuitContext.Core (HasCircuitContext)
import Clash.CircuitContext.Internal.Recorder (Recording)
import Clash.CircuitContext.Internal.Waveform

-- | Which cases of a property leave a waveform.
data Capture
  = -- | Only the counterexample Hedgehog reports.
    Counterexample
  | {- | The counterexample, and the first case whose 'Size' is at least this
    one, once per waveform path in this process (two spellings of one path,
    such as @a\/b@ and @.\/a\/b@, are one path). 'Size' is Hedgehog's measure
    of how large a case is, so a small one gives a short waveform you can read
    end to end. A property runs its cases at sizes 0, 1, 2, … in order, up to
    99, and then from 0 again. A discarded case takes a size too, so a
    property of @n@ tests and @d@ discarded cases reaches sizes below
    @min (n + d) 100@. The waveform of this case has the size in its path:
    @\<path\>_\<size\>.fst@ for @AtSize \<size\>@, so the counterexample's
    waveform does not replace it, nor it the counterexample's. Give each call
    of 'simulateWaveform' its own path: two calls with one path keep one
    case between them.
    -}
    AtSize Size
  deriving (Show, Eq)

{- | @stop@ samples of a design, for a property to check. Records nothing,
except as 'Capture' asks; see the module header. A waveform shows the cycles
@[start, stop)@ of the window, as in "Clash.CircuitContext.Waveform".

The counterexample's waveform is @\<path\>.fst@ or @.vcd@ (and its @.json@
sidecar), and the case of an 'AtSize' capture's @\<path\>_\<size\>.fst@ or
@.vcd@. Give each CALL its own path, not only each property: two calls with
one path (in one property, also spelled two ways, such as @a\/b@ and
@.\/a\/b@) write the same files, so the waveform of one of them is lost, and
the report names the one file twice. This is not checked. A relative path is
relative to the working directory, and missing directories are made, before
the case runs: a directory that cannot hold the waveform (a part of the path
is a file, or a directory is not writable) fails the property at once, with
an error that names it.
-}
simulateWaveform ::
  forall dom a.
  (KnownDomain dom, NFDataX a) =>
  WaveformOptions ->
  Capture ->
  {- | The path of the waveform, without extension: @\<path\>.fst@ or @.vcd@.
  An empty path, a directory, a path that ends in @.fst@, @.vcd@, @.json@ or
  @.spill@, and a path whose directory cannot be made or written, are
  refused, before anything runs
  -}
  FilePath ->
  -- | The cycles to show, @(start, stop)@; @stop@ is the number of samples
  (Int, Int) ->
  -- | The signal to observe
  (HasCircuitContext => Signal dom a) ->
  PropertyT IO [a]
simulateWaveform opts capture path0 window@(_, stop) sig = do
  liftIO (checkPath path0)
  path <- liftIO (resolvePath path0)
  liftIO (checkWindow path window)
  liftIO (checkDirectory path)
  keepThis <- case capture of
    Counterexample -> pure Nothing
    AtSize size -> firstAtSize path size
  out <-
    liftIO $ case keepThis of
      Just sized -> do
        (xs, rec) <- recordRun @dom opts sized window sig
        save Requested opts sized (cycleWindow @dom window) rec
          >>= either (announce sized . Left . unsavedReason) (const (pure ()))
        either throwIO pure xs
      Nothing -> simulate stop sig
  footnote (onPrint (recordCounterexample @dom opts path window sig))
  pure out

-- | Record @stop@ samples of the signal, forced.
recordRun ::
  forall dom a.
  (KnownDomain dom, NFDataX a) =>
  WaveformOptions ->
  FilePath ->
  (Int, Int) ->
  (HasCircuitContext => Signal dom a) ->
  IO (Either SomeException [a], Recording)
recordRun opts path window@(_, stop) sig =
  record opts path (fst (psWindow @dom window)) $ do
    let xs = sampleN stop sig
    evaluate (xs `deepseqX` xs)

{- | Record this case again and write its waveform. Run only when Hedgehog
prints the footnote; never throws, as it runs inside Hedgehog's report.
-}
recordCounterexample ::
  forall dom a.
  (KnownDomain dom, NFDataX a) =>
  WaveformOptions ->
  FilePath ->
  (Int, Int) ->
  (HasCircuitContext => Signal dom a) ->
  IO String
recordCounterexample opts path window sig = do
  result <- try $ do
    (_, rec) <- recordRun @dom opts path window sig
    save Failure opts path (cycleWindow @dom window) rec
  pure $ case result of
    Right (Right absolute) -> "waveform: " <> absolute
    Right (Left err) -> "no waveform for " <> path <> ": " <> unsavedReason err
    Left (err :: SomeException) -> "no waveform for " <> path <> ": " <> displayException err

{- | A string whose evaluation runs an action: the footnote text above.

NOINLINE, so that each footnote is its own thunk and runs only when it is
evaluated, at most once.
-}
onPrint :: IO String -> String
onPrint = unsafePerformIO
{-# NOINLINE onPrint #-}

{- | Is this the first case of at least this size for this path? Then the
path of its waveform: @\<path\>_\<size\>@. Asked inside a generator, because
that is where Hedgehog gives the size; it has no shrinks. The generator is
lifted into the property as 'Hedgehog.forAll' lifts one, but without the
annotation 'Hedgehog.forAll' adds: the report of a failure shows each
annotation, and one of a library whose source the report cannot read shows
as an empty line, @forAll0 =@.
-}
firstAtSize :: FilePath -> Size -> PropertyT IO (Maybe FilePath)
firstAtSize path size =
  PropertyT . lift . Gen.sized $ \current ->
    if current >= size
      then liftIO (atomicModifyIORef' fired (\names -> (Set.insert sized names, first names)))
      else pure Nothing
 where
  sized = path <> "_" <> show (fromIntegral size :: Int)
  first names = if Set.member sized names then Nothing else Just sized

{- | The waveform paths an 'AtSize' capture has used in this process. Only
paths, so it does not grow with the waveforms. It is what makes "once" hold
across cases, and across the reruns Hedgehog makes while it shrinks.
-}
fired :: IORef (Set.Set String)
fired = unsafePerformIO (newIORef Set.empty)
{-# NOINLINE fired #-}
