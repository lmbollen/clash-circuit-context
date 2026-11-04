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
clash-circuit-context: firmware: the run was stopped; what it recorded is in
/home/you/project/waveforms/firmware.spill. Write its waveform with
recoverWaveform, or with spill2fst or spill2vcd (executables of
clash-circuit-context)
```

A process that is killed writes no message. Its spill file is in the same
place.

## Write the waveform

Do one of these:

- From the command line, use an executable of the package:

  ```bash
  spill2fst waveforms/firmware.spill
  ```

  This writes `waveforms/firmware.fst` and `waveforms/firmware.json`, and
  keeps the spill file. Give `--delete` to delete it. For all options, see
  [Executables](../reference/executables.md).

- From Haskell, use `recoverWaveform` with the options of the waveform. It
  writes `waveforms/<name>.fst` or `.vcd`, and deletes the spill file:

  ```haskell
  recoverWaveform defaultOptions "firmware" >>= print
  ```

The waveform has the cycles that the run recorded, up to the last block of
values that it wrote. The values of the last cycles before a process was
killed can be missing: each signal writes its values in blocks of 16 KiB.
A run that was cancelled writes all the values that it recorded before it
stops. The last cycle of each signal can be missing: a trace records a cycle
when the simulation evaluates the next one.

The sidecar of the waveform is complete. A spill file has the descriptions
of its signals, so the executables do not need the design or its types.

For the content of a spill file, see [Waveform
files](../reference/waveform-files.md#the-spill-file).
