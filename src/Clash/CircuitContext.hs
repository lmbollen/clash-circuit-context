-- | Umbrella module: scoped simulation tracing for Clash designs.
-- See "Clash.CircuitContext.Core" for the runtime API. The GHC plugin lives in
-- "Clash.CircuitContext.Plugin"; its ABI in "Clash.CircuitContext.Auto".
module Clash.CircuitContext (
  module Clash.CircuitContext.Core,
) where

import Clash.CircuitContext.Core
