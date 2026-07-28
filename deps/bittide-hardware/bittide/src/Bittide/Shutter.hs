-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Shutter where

import Clash.CircuitContext (HasCircuitContext)
import Clash.Explicit.Prelude
import Protocols

{- | Only lets through the signal @CSignal dom a@ when the the input signal is
-- 'True'. Otherwise, it returns 'Nothing'.
-}
{-# OPAQUE shutter #-}
shutter ::
  forall dom a.
  (HasCircuitContext, KnownDomain dom) =>
  {- | Shutter control. If 'True', the signal is let through. If 'False', the
  signal is blocked.
  -}
  Signal dom Bool ->
  Circuit
    (CSignal dom a)
    (CSignal dom (Maybe a))
shutter shut = Circuit go
 where
  go (a, _) = (units, mux shut (Just <$> a) (pure Nothing))
