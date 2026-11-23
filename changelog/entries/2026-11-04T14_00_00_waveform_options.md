---
no-links: true
---

# CHANGED
`writeWaveform`, `withWaveform` and `simulateWaveform` take `WaveformOptions`
as their first argument. `defaultOptions` writes an FST, recorded in memory.
To keep what these functions did before (a VCD, recorded through a spill
file):

```hs
writeWaveform defaultOptions{format = VCD, spill = OnDisk} "waveforms/counter" (0, 100) dut
```
