# Executables

The package has two executables. Each writes the waveform of spill files:
`spill2fst` as FST, `spill2vcd` as VCD. Both write the sidecar beside the
waveform. A spill file of a run that was stopped holds what the run recorded
up to there, and the executables write that.

The executables need nothing of the design: a spill file has the names of
its signals, their widths, and their descriptions for the sidecar.

In a checkout of the package, run one with `cabal run`, or install both on
your `PATH`:

```bash
cabal run spill2fst -- waveforms/counter.spill
cabal install exe:spill2fst exe:spill2vcd
```

## Usage

```text
spill2fst [OPTION]... SPILL...
spill2vcd [OPTION]... SPILL...
```

| Option | Effect |
|---|---|
| `-o FILE`, `--output=FILE` | the path of the waveform. Only with one spill file. By default, the path of the spill file with the extension of the waveform: `waveforms/counter.spill` gives `waveforms/counter.fst` |
| `-w START,STOP`, `--window=START,STOP` | the cycles to show, `[START, STOP)`, in units of the timescale (for a design in one domain: cycles). By default, all cycles that the run recorded |
| `--delete` | delete each spill file after its waveform is written |
| `-h`, `--help` | show the help |

The executables keep the spill file, unless you give `--delete`.

## Output and exit status

For each waveform that it writes, an executable prints
`<program>: wrote <path>`. For each spill file that it cannot write, it
prints the reason on stderr, and continues with the next file. The exit
status is 0 if each file was written, and 1 if not.

The same conversion is the function `writeSpill` of
`Clash.CircuitContext.Recording`: use it to convert spill files from your
own program.

```haskell
writeSpill :: Format -> Maybe (Int, Int) -> FilePath -> FilePath -> IO (Either String ())
```
