{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Write the FST waveform of clash-circuit-context spill files; see "SpillMain".
-}
module Main (main) where

import Clash.CircuitContext.Recording (Format (FST))
import SpillMain (spillMain)

main :: IO ()
main = spillMain FST
