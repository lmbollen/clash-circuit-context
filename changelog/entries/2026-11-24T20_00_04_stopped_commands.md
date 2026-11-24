---
no-links: true
---

# CHANGED
The message of a stopped run gives the commands that write its waveform, with
the path filled in: `recoverWaveform defaultOptions "<path without .spill>"`
in Haskell, and `cabal run clash-circuit-context:spill2fst -- <path>.spill`
from a shell in a cabal project that has the package. Before, it named
`spill2fst` and `recoverWaveform` only, and printed the `.spill` file, which
`recoverWaveform` does not take.

# FIXED
`spill2fst` and `spill2vcd` name each file by its absolute path in their
messages, and say that a spill file that is missing does not exist, once.
Before, they printed paths as given, and a missing file's path twice, with
the raw error of `openBinaryFile`.
