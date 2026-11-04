{- |
Copyright  :  (C) 2026, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Lucas Bollen <lucas@qbaylogic.com>

Write the VCD waveform of clash-circuit-context spill files; see "SpillMain".
-}
module Main (main) where

import Clash.CircuitContext.Recording (Format (VCD))
import SpillMain (spillMain)

main :: IO ()
main = spillMain VCD
