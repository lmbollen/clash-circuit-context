---
no-links: true
---

# FIXED
Waveform files (`.vcd` and `.json`) get the default permissions of the
process (its umask). Before, they were readable only by their owner.
