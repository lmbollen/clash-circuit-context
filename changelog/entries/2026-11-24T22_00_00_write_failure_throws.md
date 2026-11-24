---
no-links: true
---

# CHANGED
`writeWaveform` throws when its waveform cannot be written (a full disk, a
directory where the file should go), with an error that names the waveform
and says why; with `spill = OnDisk`, the spill file of the run is kept, and
the error names it. Before, a message on stderr said so, and `writeWaveform`
returned its samples: a test passed without its waveform. A run that recorded
nothing in the window still gives a message, not an error.
