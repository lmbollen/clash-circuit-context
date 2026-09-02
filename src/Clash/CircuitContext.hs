{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Umbrella module: scoped simulation tracing for Clash designs.
See "Clash.CircuitContext.Core" for the runtime API. The GHC plugin lives in
"Clash.CircuitContext.Plugin"; its ABI in "Clash.CircuitContext.Auto".
'Traceable' — the extension point for tracing user types, including its
generic derivation for records of signals — is re-exported from there.

What the plugin does NOT instrument, it reports: see
"Clash.CircuitContext.Plugin.Diagnostics" for the warning categories and the
flags that silence or promote them.
-}
module Clash.CircuitContext (
  module Clash.CircuitContext.Core,
  Traceable (..),
) where

import Clash.CircuitContext.Auto (Traceable (..))
import Clash.CircuitContext.Core
