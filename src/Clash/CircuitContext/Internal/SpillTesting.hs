{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Not part of the API: what this package's tests need to reach a path of the
recorder that otherwise only the file system chooses.
-}
module Clash.CircuitContext.Internal.SpillTesting (withoutMappings) where

import Control.Exception (bracket_)
import Data.IORef (writeIORef)

import Clash.CircuitContext.Internal.SpillFile (mapping)

{- | Run an action whose recorders write their spill files without mapping
them, as where mapping fails: every region a buffer. For one thread at a
time: it sets what every recorder made meanwhile reads.
-}
withoutMappings :: IO a -> IO a
withoutMappings = bracket_ (writeIORef mapping False) (writeIORef mapping True)
