# How to keep the waveform of a stopped run

A run can stop before it writes its waveform: a test runner cancels a test
that takes too long, you press Ctrl-C, or the system kills the process. This
guide shows how to keep what such a run recorded, and write its waveform
after it stopped.

## Record into a spill file

Give the waveform function the option `spill = OnDisk`:

```haskell
opts :: WaveformOptions
opts = defaultOptions{spill = OnDisk}

case_firmware :: Assertion
case_firmware = do
  bytes <- writeWaveform opts "firmware" (0, 1_000_000) (uartBytes design)
  takeWhile (/= '\n') (decode bytes) @?= "RESULT: OK"
```

The recorded values then go to `waveforms/<name>.spill` while the
simulation runs. When the waveform is written, the spill file is deleted.
When the run stops first, the spill file stays.

Only a run that records can leave a spill file. `writeWaveform` records its
run. `withWaveform` and `simulateWaveform` first run without recording, and
record only when they run a failed case again. So a test that hangs and is
cancelled in its first run leaves nothing. To keep the waveform of a test
that can hang, use `writeWaveform`, as above.

With the default, `spill = InMemory`, the recorded values are in memory, and
a run that stops leaves nothing.

## When a run stops

A run that is cancelled writes a message on stderr with the absolute path of
the spill file:

```text
clash-circuit-context: the run was stopped; what it recorded is in
/home/you/project/waveforms/firmware.spill. Write its waveform with spill2fst
or spill2vcd (executables of clash-circuit-context), or with recoverWaveform
or writeSpill
```

`withRecording` with a spill file writes the same message.

A process that is killed writes no message. Its spill file is in the same
place. A second Ctrl-C kills the process: the first one cancels the run, and
the second one stops the process before the run can write its message. Some
tools send the signal twice, for example `timeout -s INT`, which sends it to
the process and to its process group.

## Write the waveform

Do one of these:

- From the command line, use an executable of the package:

  ```bash
  spill2fst waveforms/firmware.spill
  ```

  This writes `waveforms/firmware.fst` and `waveforms/firmware.json`, and
  keeps the spill file. Give `--delete` to delete it. For all options, see
  [Executables](../reference/executables.md).

  The executable prints the window that it wrote, for example
  `spill2fst: wrote waveforms/firmware.fst, window 0,128717`: the run
  recorded 128717 cycles.

- From Haskell, use `recoverWaveform` with the options of the waveform. It
  writes `waveforms/<name>.fst` or `.vcd`, deletes the spill file, and gives
  the absolute path of the waveform:

  ```haskell
  recoverWaveform defaultOptions "firmware" >>= print
  ```

  For the spill file of `withRecording`, which can have any path, use
  `writeSpill` (see [Write the files
  yourself](../reference/waveform-files.md#write-the-files-yourself)).

What the waveform has:

- A run that was cancelled: all the values that it recorded before it
  stopped.
- A process that was killed: the values up to about one second before it was
  killed. A recording writes the new values of all signals to the spill file
  once each second.

In both cases the last cycles of the signals can end at different times,
over a few cycles: a signal that the simulation did not evaluate in a cycle
is `z` in that cycle.

The sidecar of the waveform is complete. A spill file has the descriptions
of its signals, so the executables do not need the design or its types.

For the content of a spill file, see [Waveform
files](../reference/waveform-files.md#the-spill-file).
