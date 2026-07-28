-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
--
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

{- | Internal module used to test whether memory mapped circuits generate
valid HDL.

This contains mostly non-sense code to test various features.
-}

module Protocols.MemoryMap.Test.Interconnect where

import Clash.Prelude

import GHC.Stack (HasCallStack, callStack, getCallStack)
import Protocols (Circuit (..))
import Protocols.Experimental.Wishbone (
  Wishbone,
  WishboneM2S (..),
  WishboneMode (Standard),
  WishboneS2M (..),
  emptyWishboneM2S,
  emptyWishboneS2M,
 )
import Protocols.MemoryMap (
  Address,
  MemoryMap (..),
  MemoryMapTree (Interconnect),
  Mm,
  ToConstBwd,
  mergeDeviceDefs,
 )

interconnect ::
  forall dom addrWidth nBytes n.
  ( HasCallStack
  , KnownNat n
  , KnownNat addrWidth
  , KnownNat nBytes
  , (CLog 2 n <= addrWidth)
  , 1 <= n
  , KnownNat (addrWidth - CLog 2 n)
  ) =>
  Circuit
    ( ToConstBwd Mm
    , Wishbone dom 'Standard addrWidth nBytes
    )
    ( Vec
        n
        ( ToConstBwd Mm
        , Wishbone dom 'Standard (addrWidth - CLog 2 n) nBytes
        )
    )
interconnect = Circuit go
 where
  (_, loc) : _ = getCallStack callStack

  go ::
    ( ((), Signal dom (WishboneM2S addrWidth nBytes))
    , Vec n (SimOnly MemoryMap, Signal dom (WishboneS2M nBytes))
    ) ->
    ( (SimOnly MemoryMap, Signal dom (WishboneS2M nBytes))
    , Vec
        n
        ((), Signal dom (WishboneM2S (addrWidth - CLog 2 n) nBytes))
    )
  go (((), m2s), unzip -> (mms, s2ms)) = ((SimOnly memoryMap, s2m), m2ss)
   where
    memoryMap =
      MemoryMap
        { deviceDefs = mergeDeviceDefs ((.deviceDefs) . unSim <$> toList mms)
        , tree = Interconnect loc (toList descs)
        }
    descs = zip relAddrs ((.tree) . unSim <$> mms)
    relAddrs = prefixToAddr <$> prefs
    unSim (SimOnly x) = x
    prefs = iterateI (+1) 0

    (unbundle -> (s2m, m2ss0)) = inner prefs <$> m2s <*> bundle s2ms
    m2ss = ((),) <$> unbundle m2ss0

    prefixToAddr :: BitVector (CLog 2 n) -> Address
    prefixToAddr prefix = natToNum @nBytes * (toInteger prefix `shiftL` fromInteger shift')
     where
      shift' = snatToInteger $ SNat @(addrWidth - CLog 2 n)


  inner ::
    Vec n (BitVector (CLog 2 n)) ->
    WishboneM2S addrWidth nBytes ->
    Vec n (WishboneS2M nBytes) ->
    (WishboneS2M nBytes, Vec n (WishboneM2S (addrWidth - CLog 2 n) nBytes))
  inner prefixes m2s@WishboneM2S{..} s2ms
    | not (busCycle && strobe) = (emptyWishboneS2M, m2ss)
    -- no valid index selected
    | Nothing <- selected = (emptyWishboneS2M{err = True}, m2ss)
    | Just idx <- selected = (s2ms !! idx, replace idx m2sStripped m2ss)
   where
    m2ss = repeat emptyWishboneM2S
    m2sStripped = m2s{addr = internalAddr}
    (compIdx, internalAddr) = split @_ @(CLog 2 n) @(addrWidth - CLog 2 n) addr
    selected = elemIndex compIdx prefixes
