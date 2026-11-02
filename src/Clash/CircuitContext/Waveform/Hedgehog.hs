{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

The waveform of a Hedgehog property: of the counterexample Hedgehog reports,
and, if you ask for it, of one passing case.

> prop_counter :: Property
> prop_counter = property $ do
>   xs  <- forAll genInputs
>   out <- simulateWaveform Counterexample "counter" (length xs) (top (fromList xs))
>   out === model xs

Every case simulates without recording, so a property that passes costs
nothing extra. When it fails, Hedgehog's report ends with
@waveform: \/abs\/path\/waveforms\/counter.vcd@: the waveform of the case it
reports, the smallest counterexample.

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
  Capture (..),
  simulateWaveform,
) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Set as Set
import System.Directory (makeAbsolute)
import System.IO.Unsafe (unsafePerformIO)

import Hedgehog (PropertyT, Size)
import Hedgehog.Internal.Property (footnote, forAllWithT)

import qualified Hedgehog.Gen as Gen

import Clash.Explicit.Signal (Signal)
import Clash.Prelude (NFDataX)

import Clash.CircuitContext.Core (HasCircuitContext)
import Clash.CircuitContext.Internal.Waveform

-- | Which cases of a property leave a waveform.
data Capture
  = -- | Only the counterexample Hedgehog reports.
    Counterexample
  | {- | The counterexample, and the first case whose 'Size' is at least this
    one, once per waveform name in this process. 'Size' is Hedgehog's measure
    of how large a case is, so a small one gives a short waveform you can read
    end to end. A property runs at sizes 0, 1, 2, … in order, so a property of
    @n@ tests reaches sizes below @n@. If the property later fails, the
    counterexample replaces this waveform.
    -}
    AtSize Size
  deriving (Show, Eq)

{- | @n@ samples of a design, for a property to check. Records nothing,
except as 'Capture' asks; see the module header.

The waveform is @waveforms\/\<name\>.vcd@ (and its @.json@ sidecar); give each
property its own name.
-}
simulateWaveform ::
  (NFDataX a) =>
  Capture ->
  -- | The name of the waveform: @waveforms\/\<name\>.vcd@
  String ->
  -- | Number of samples
  Int ->
  -- | The signal to observe
  (HasCircuitContext => Signal dom a) ->
  PropertyT IO [a]
simulateWaveform capture name n sig = do
  keepThis <- case capture of
    Counterexample -> pure False
    AtSize size -> firstAtSize name size
  out <-
    liftIO $
      if keepThis
        then do
          (xs, traces, probes) <- record n sig
          _ <- save name traces probes
          pure xs
        else simulate n sig
  footnote (onPrint (recordCounterexample name n sig))
  pure out

{- | Record this case again and write its waveform. Run only when Hedgehog
prints the footnote; never throws, as it runs inside Hedgehog's report.
-}
recordCounterexample ::
  (NFDataX a) => String -> Int -> (HasCircuitContext => Signal dom a) -> IO String
recordCounterexample name n sig = do
  result <- try $ do
    (_, traces, probes) <- record n sig
    save name traces probes
  case result of
    Right True -> ("waveform: " <>) <$> makeAbsolute (waveformPath name)
    Right False -> pure ("no waveform for " <> name <> ": nothing was recorded")
    Left (err :: SomeException) ->
      pure ("no waveform for " <> name <> ": " <> displayException err)

{- | A string whose evaluation runs an action: the footnote text above.

NOINLINE, so that each footnote is its own thunk and runs only when it is
evaluated, at most once.
-}
onPrint :: IO String -> String
onPrint = unsafePerformIO
{-# NOINLINE onPrint #-}

{- | Is this the first case of at least this size for this name? Asked inside
a generator, because that is where Hedgehog gives the size; it renders as
nothing and has no shrinks.
-}
firstAtSize :: String -> Size -> PropertyT IO Bool
firstAtSize name size =
  forAllWithT (const "") . Gen.sized $ \current ->
    if current >= size
      then liftIO (atomicModifyIORef' fired (\names -> (Set.insert name names, Set.notMember name names)))
      else pure False

{- | The waveform names an 'AtSize' capture has used in this process. Only
names, so it does not grow with the waveforms. It is what makes "once" hold
across cases, and across the reruns Hedgehog makes while it shrinks.
-}
fired :: IORef (Set.Set String)
fired = unsafePerformIO (newIORef Set.empty)
{-# NOINLINE fired #-}
