---
no-links: true
---

# FIXED
`withWaveform` fails a test with the check's own exception also when the run
that records the failure cannot record (for example because another
recording uses its spill file). Before, the recording's error took the place
of the check's failure. A message on stderr now says why there is no
waveform.
