---
no-links: true
---

# ADDED
The executables `spill2fst` and `spill2vcd` write the waveform of spill
files, also of runs that were stopped:

```sh
spill2fst waveforms/counter.spill
```

`Clash.CircuitContext.Recording.writeSpill` does the same from Haskell, and
`recordingStart` gives the start of a recording's window.
