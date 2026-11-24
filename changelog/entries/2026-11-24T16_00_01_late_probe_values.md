---
no-links: true
---

# FIXED
A probed value that the simulation evaluates after a later cycle's value (for
example a value kept in a mealy machine's state and put out a few cycles
later) is recorded at its own cycle. Before, its cycle showed `z`, which means
"not evaluated", although it was evaluated. Such late values are kept as
compact records, in the spill file when there is one.
