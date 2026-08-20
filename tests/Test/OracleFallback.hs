{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

{- | Phase 2: exercises 'autoTrace' MANUALLY (no plugin rewrite involved)
against the real runtime, validating the oracle's decisions:

* concrete traceable signal → traced;
* concrete signal with a non-'BitPack' payload → identity, and compiles;
* polymorphic signal with the full evidence in givens → traced;
* polymorphic signal WITHOUT 'BitPack' evidence → identity, and compiles
  (the no-backtracking property ordinary instances can't provide);
* 'Vec' of signals → traced element-wise with @_i@ names;
* record of signals with a generically derived 'Traceable' instance →
  traced field-wise (the oracle recurses on the instance's written
  context); the same record shape WITHOUT an instance → identity, and
  compiles;
* circuit-notation port shapes: @Tagged p (Signal …)@ → traced (newtype
  delegation); @Tagged p ((), Signal …)@ → the signal component traced as
  @nm_1@, the unit recording nothing; @Tagged p (Signal … NoPack)@ →
  identity; a tuple with one untraceable component → identity for the
  WHOLE binder (all-or-nothing per binder), and compiles.
-}
module Main where

import qualified Prelude as P

import Control.Exception (evaluate)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Tagged (Tagged (..), unTagged)
import System.Exit (exitFailure)

import Clash.Explicit.Prelude

import Clash.CircuitContext.Auto (Traceable, autoTrace)
import Clash.CircuitContext.Core (HasCircuitContext, withCircuitContext)

-- | Payload without BitPack: @CanTrace (Signal System NoPack)@ is 'False.
data NoPack = MkNoPack
  deriving (Generic, NFDataX, Show)

{- | A record of signals with a generically derived 'Traceable' instance:
@CanTrace (RecOut System)@ is 'True (oracle recurses on the written
instance context).
-}
data RecOut dom = RecOut
  { roA :: Signal dom (Unsigned 8)
  , roB :: Signal dom Bool
  }
  deriving (Generic)

instance (KnownDomain dom) => Traceable (RecOut dom)

-- | Same shape, no instance: @CanTrace (RecNo System)@ is 'False.
data RecNo dom = RecNo
  { rnA :: Signal dom (Unsigned 8)
  }
  deriving (Generic)

{- | Phantom protocol tags, standing in for what circuit-notation puts in
@Tagged@'s first argument (a @Protocol@ instance type).
-}
data PortA

data PortB

{- | Note: no 'Typeable' — @(KnownDomain, BitPack, NFDataX)@ is the full
tracing requirement.
-}
polyTraced ::
  (HasCircuitContext, KnownDomain dom, BitPack a, NFDataX a) =>
  Signal dom a ->
  Signal dom a
polyTraced s = autoTrace "polyyes" s

polyNoEvidence ::
  (HasCircuitContext, KnownDomain dom, NFDataX a) =>
  Signal dom a ->
  Signal dom a
polyNoEvidence s = autoTrace "polyno" s

check :: String -> Bool -> IO ()
check what ok
  | ok = putStrLn ("ok: " <> what)
  | otherwise = putStrLn ("FAIL: " <> what) >> exitFailure

main :: IO ()
main = do
  (_, traces, _probes) <- withCircuitContext $ do
    let
      mono =
        autoTrace "mono" (fromList [1, 2, 3, 4] :: Signal System (Unsigned 8))
      blocked =
        autoTrace
          "blocked"
          (fromList [MkNoPack, MkNoPack, MkNoPack] :: Signal System NoPack)
      polyY = polyTraced (fromList [7, 7, 7] :: Signal System (Unsigned 4))
      polyN = polyNoEvidence (fromList [9, 9, 9] :: Signal System (Unsigned 8))
      vec =
        autoTrace
          "vec"
          ( (fromList [True, True, True] :: Signal System Bool)
              :> (fromList [False, False, False] :: Signal System Bool)
              :> Nil
          )
      recY =
        autoTrace
          "recy"
          ( RecOut
              (fromList [1, 2, 3] :: Signal System (Unsigned 8))
              (fromList [True, False, True])
          )
      recN =
        autoTrace
          "recn"
          (RecNo (fromList [4, 5, 6] :: Signal System (Unsigned 8)))
      -- Circuit-notation port shapes (see module header).
      tagSig =
        autoTrace
          "tagsig"
          (Tagged (fromList [1, 2, 3]) :: Tagged PortA (Signal System (Unsigned 8)))
      tagPair =
        autoTrace
          "tagpair"
          ( Tagged ((), fromList [8, 8, 8]) ::
              Tagged PortB ((), Signal System (Unsigned 8))
          )
      tagBlocked =
        autoTrace
          "tagblocked"
          ( Tagged (fromList [MkNoPack, MkNoPack]) ::
              Tagged PortA (Signal System NoPack)
          )
      tagMixed =
        autoTrace
          "tagmixed"
          ( Tagged (fromList [MkNoPack, MkNoPack], fromList [1, 2]) ::
              Tagged PortB (Signal System NoPack, Signal System (Unsigned 8))
          )
    -- Force one sample of each so lazy trace registration fires.
    _ <- evaluate (P.head (sampleN 2 mono))
    _ <- evaluate (P.head (sampleN 2 blocked))
    _ <- evaluate (P.head (sampleN 2 polyY))
    _ <- evaluate (P.head (sampleN 2 polyN))
    _ <- P.traverse (evaluate . P.head . sampleN 2) (toList vec)
    _ <- evaluate (P.head (sampleN 2 (roA recY)))
    _ <- evaluate (P.head (sampleN 2 (roB recY)))
    _ <- evaluate (P.head (sampleN 2 (rnA recN)))
    _ <- evaluate (P.head (sampleN 2 (unTagged tagSig)))
    _ <- evaluate (P.head (sampleN 2 (snd (unTagged tagPair))))
    _ <- evaluate (P.head (sampleN 2 (unTagged tagBlocked)))
    _ <- evaluate (P.head (sampleN 2 (snd (unTagged tagMixed))))
    pure ()
  let
    keys = Map.keys traces
    has nm = P.any (nm `isInfixOf`) keys
  putStrLn ("trace keys: " <> show keys)
  check "mono traced" (has "mono@")
  check "poly-with-evidence traced (given lookup)" (has "polyyes@")
  check "vec traced element-wise" (has "vec_0@" P.&& has "vec_1@")
  check
    "record with derived instance traced field-wise"
    (has "recy_roA@" P.&& has "recy_roB@")
  check "non-BitPack payload fell back to identity" (P.not (has "blocked"))
  check "poly-without-evidence fell back to identity" (P.not (has "polyno"))
  check "record without instance fell back to identity" (P.not (has "recn"))
  check "Tagged signal traced (newtype delegation)" (has "tagsig@")
  check
    "Tagged ((), signal) traced component-wise; unit records nothing"
    (has "tagpair_1@" P.&& P.not (has "tagpair_0"))
  check
    "Tagged non-BitPack payload fell back to identity"
    (P.not (has "tagblocked"))
  -- Per-leaf tolerance. Composites carry their fields' requirements as
  -- @AutoTrace (CanTrace c) c@, so an untraceable component degrades to
  -- identity ON ITS OWN and its traceable siblings still record. Before, the
  -- whole tuple fell back; the real cost was a clash-protocols-memmap device
  -- port whose Bwd is @(MemoryMap, Signal dom WishboneS2M)@ silently losing the
  -- bus response along with the untraceable memory map.
  check
    "tuple with one untraceable component traces the traceable one"
    (has "tagmixed_1@")
  check
    "...and the untraceable component alone falls back"
    (P.not (has "tagmixed_0"))
  putStrLn "fallback passed"
