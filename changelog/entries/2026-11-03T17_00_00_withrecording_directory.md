---
no-links: true
---

# FIXED
`withRecording` makes the directory of its spill file. Before, a first run in
a fresh checkout failed with `openBinaryFile: does not exist`. When it cannot
(a part of the path is a file, or a directory is not writable), it throws,
before the action runs, an `IOError` that names the spill file and the part
of the path that is in the way.
