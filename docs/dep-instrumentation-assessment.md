# Assessment: instrumenting bittide-hardware's Clash dependencies with ccc

**Date:** 2026-07-28
**Question:** *Instrument all Clash-related dependencies of bittide-hardware with
the `Clash.CircuitContext.Plugin`, adding `HasCircuitContext` to key combinators
(`registerWb`/`deviceWb`/…) — and assess the consequence.*

## Setup

Local editable checkouts of the dependency repos were cloned at the exact SHAs
bittide pins, under `deps/`:

| package(s) | repo | SHA |
|---|---|---|
| `clash-protocols`, `clash-protocols-base` | clash-lang/clash-protocols | `e03dbd89` |
| `clash-protocols-memmap`, `clash-bitpackc` | QBayLogic/clash-protocols-memmap | `20bacc70` |
| `clash-cores` | clash-lang/clash-cores | `5d084eff` |

bittide's `cabal.project` `source-repository-package` stanzas for these were
commented out (kept for provenance) and replaced with local `packages:` entries;
`cabal.project.local` was pointed at `../../clash-circuit-context.cabal` and set
`tests: False` for the instrumented checkouts (we only link their libraries).
`clash-vexriscv` was deliberately excluded: it is a foreign (C++/Verilator/sbt)
blackbox with no `-<` circuit code to trace.

The "pattern" replicated onto each dependency is bittide's own:
`-fplugin=Clash.CircuitContext.Plugin` in the shared `ghc-options`, `ImplicitParams`
in the default extensions, and a `clash-circuit-context` build-dep. This is sound
precisely because **ccc does not depend on clash-protocols** — so a dependency can
depend on ccc without a cycle.

## What was instrumented (additive, opt-in per function)

**Cycle 1 — `clash-protocols-memmap` (the named target):**
- `HasCircuitContext` added to the shared `RegisterWbConstraints` synonym — one
  edit that covers the entire `register*` family (`registerWb`, `registerWb_`,
  `registerWbVec*`, `registerWithOffset*`, and all `*I` variants).
- Explicit `HasCircuitContext` on the workers that don't use that synonym:
  `deviceWb`, `deviceWbI`, `deviceWithOffsetsWb`, `addressableBytesWb`.

**Cycle 2 — `clash-protocols` + `clash-cores`:**
- `clash-protocols`: `Protocols.Df.{fifo,registerFwd,registerBwd}` (the stateful
  Df stream combinators) and their re-export wrappers in `Protocols.DfConv`
  (`fifo`/`registerFwd`/`registerBwd`) and `Protocols.PacketStream.Base`
  (`registerFwd`/`registerBwd`/`registerBoth`).
- `clash-cores`: `Clash.Cores.Etherbone.etherboneC`.

## Finding 1 — the mechanism works unchanged at the dependency level

`Protocols.Plugin` (circuit-notation) desugars every `x <- comp -< a` into
`Data.Tagged.Tagged`-wrapped port binders, which ccc's renamer already wraps and
which the existing `instance Traceable (Tagged p t)` (+ unit/tuple instances) in
`Auto.hs` already cover. So **no ccc plugin changes were needed** — only
`HasCircuitContext` on the target signatures + enabling the plugin on the package.
Every instrumented library compiled cleanly with the plugin engaged.

## Finding 2 — virality is small, and its size tracks combinator *level*

`HasCircuitContext` is `?circuitContext :: CircuitContext`, an implicit param, so
it is viral: every transitive caller must provide it (or cap it with
`withoutCircuitContext`). The decisive mitigation already present in bittide:
`RegisterWbConstraints` **already carried `?byteOrder :: ByteOrder`**, so
`?circuitContext` rides the exact rails the codebase already threads, and
synthesis/sampling tops already cap context with `withoutCircuitContext`.

Measured call-site breakage (via a `-fdefer-type-errors` census — a complete,
one-build enumeration of every unsatisfied site across
protocols+memmap+cores+bittide+instances):

| cycle | instrumented | call sites needing a fix |
|---|---|---|
| 1 (memmap, high-level `registerWb`/`deviceWb`) | ~20 combinators (1 synonym + 4 workers) | **7** — 3 in `bittide` lib, 3+1 in `bittide-instances` |
| 2 (protocols/cores, low-level `Df.fifo`/`registerBwd`) | 7 combinators | **7** — all combinator-wrapper functions inside `clash-protocols` (DfConv ×3, PacketStream ×3) + 1 in `bittide` (`axiStreamPacketFifo`) |

**Key insight for future work:** virality scales with how low-level and
widely-reused a combinator is. High-level combinators (`registerWb`) barely
propagate — they sit near the leaves of the design. Low-level primitives
(`Df.fifo`) cascade through every stream-combinator wrapper that re-exports them
(`Df` → `DfConv` → `PacketStream` → `axiStreamPacketFifo`). Prefer instrumenting
high-level combinators; reach for low-level ones only when their internals are
specifically wanted.

Each fix was one line: add `HasCircuitContext` next to the existing constraints
(propagate), except at genuine synthesis boundaries where the idiom is to cap
(e.g. `vexRiscGmiiC`'s `wbToAxi4StreamTx'` got `withoutCircuitContext`, matching
its sibling sub-circuits).

## Finding 3 — the new waveform depth (watchdog testbench)

Regenerating the VexRiscv watchdog self-test waveform (`case_time_rust_self_test`):

| | scopes | wires |
|---|---|---|
| baseline (pre-instrumentation) | 25 | 114 |
| after cycle 1 (memmap) | 25 | **168** (+54) |

The +54 wires are the previously-invisible internals of the CPU's memory-mapped
Wishbone devices — `wbStorage_0/1`, `timeWb`, `uartInterfaceWb` now expose
`wb0_Bwd`, `wbs_Fwd_N`, `wbS2M`, `activeSubordinate`, … . This closes the audit's
"coverage gap 1" (Circuit/`-<` components registering no leaves) *natively*, at
the dependency layer, with no per-edge `traceEdgeC` stopgap. No new scopes because
the memmap combinators were left non-`OPAQUE` (wires flatten into the enclosing
component scope); adding `OPAQUE` would nest them under `registerWb_N`/`deviceWb_N`.

The watchdog path does **not** gain protocols-level wires — its interconnect uses
only `idC` (trivial).

Cycle-2 (Axi self-test, which exercises `axiStreamPacketFifo` → `DfConv.fifo` →
`Df.fifo`): compiles cleanly, assertion passes byte-exact, and the memmap (cycle-1)
wires appear (189 wires incl. `wbStorage_*`, `uartInterfaceWb`, …) — but **zero new
wires from the instrumented Df combinators**, even though `wbAxisRxBufferCircuit#`
and `axiRxHandler` (which contain the fifo) are traced under a live context. See
Finding 5.

Watchdog remains **168 wires** after cycle 2 (no regression).

## Finding 4 — value fidelity preserved

The watchdog UART self-test assertion still passes byte-exact
(`"Timeout took 50 microseconds"`), so instrumentation does not perturb values.
Spot-checking a new wire (`timeWb.wbS2M`, a `WishboneS2M`) shows genuine bus
values with the defined/undefined(`x`)/not-evaluated(`z`) trichotomy intact — the
ack bits render defined while the data field renders `x` only when truly undriven;
never a false zero.

## Finding 5 — `HasCircuitContext` is necessary but not sufficient for wires

The most important cycle-2 result. Adding `HasCircuitContext` makes a combinator
*compile under instrumentation* and *threads the context* — but new wires only
appear where ccc has something to wrap:

- **circuit-notation combinators trace well.** memmap's `deviceWb`/`registerWb`
  are `circuit $ \(mm, wb) -> do … <- … -< …`; the plugin's renamer wraps every
  `Data.Tagged.Tagged` port binder → +54 wires (cycle 1).
- **low-level `Circuit`-constructor combinators trace nothing from the constraint
  alone.** `Df.fifo` = `Circuit $ hideReset circuitFunction`, `registerBwd` =
  `forceResetSanity |> Circuit go`. There is no `-<` binder to wrap, and the
  internal signals live in a non-`OPAQUE`, inlined local `where` that is not a
  recording-scope root — so instrumenting them yields **0 wires** on the Axi path
  despite context flowing correctly (cycle 2).

Consequence: to actually surface a low-level combinator's internals you must
either rewrite it in circuit-notation, or make it `OPAQUE` and expose its state as
named signal binds — neither of which is an "additive `HasCircuitContext`" change.
The high-value, low-effort targets are therefore **combinators already written in
circuit-notation** (the whole memmap register/device layer, most of clash-protocols'
higher-level `circuit`-based combinators).

### Register *contents* — the third option: explicit `traceSignalC`

The register *state* (`registerWbDf`'s `packedOut`) never appears from the `-<`
tracing above — `registerWbDf` is a low-level `Circuit go`, and its `where`-binds
aren't wrapped (Finding 5). The fix (implemented in the memmap checkout) taps
`packedOut` with `traceSignalC` in `go`. Crucially, the tap must sit on an
**always-sampled** path: `registerTrace` records in lockstep with the tapped
signal being forced, and output-discarding wrappers like `registerWb_`
(`_ignored <- registerWbDf …`) never force the value output. So the tap is threaded
through the bus response `wbS2M2` (consumed every cycle by the interconnect for
every register), gated on `clashSimulation`:

```haskell
wbS2M2Traced
  | clashSimulation =
      (\c s -> c `seq` s)
        <$> traceSignalC [I.i|#{deviceName}_#{registerName}_content|] packedOut
        <*> wbS2M2
  | otherwise = wbS2M2
```

Tapping `aOut` instead would still record `registerWb_` registers — but only via
`dumpVCDC` draining the packed tail at dump time, i.e. the O(cycles) space leak the
lockstep tap exists to avoid. `registerWbDf` is the shared worker behind the whole
`register*` family, so one edit covers `registerWb`/`registerWb_`/`registerWbVec*`/
`registerWithOffset*`/`*I`. Identity outside `clashSimulation`, so **zero synthesis
impact**. Result:

- RegisterWb test: **45** `<device>_<register>_content` wires (`ManyTypes_x2_content`,
  `ManyTypes_sum0_content`, …), widths matching the register types, real values.
- Watchdog: 168 → **175 wires** — the CPU's live register contents by name
  (`Timer_scratchpad_content`, `Timer_frequency_content`, `Uart_data_content`, …).
  Assertion still byte-exact.

This mirrors the module's pre-existing `traceRegSignal` (clash-shockwaves) hook,
which already traced the same `packedOut` to a different sink behind `config.trace`.

**On `OPAQUE`:** making `deviceWb`/`registerWbDf` `OPAQUE` would auto-expose their
`where`-binds as a scope (the automatic alternative), but `OPAQUE` changes a
combinator's **synthesis netlist boundary** — that is the *component author's*
choice, not something to bake into a shared library combinator. An author who wants
a device scope writes their own `{-# OPAQUE myDevice #-}` wrapper around `deviceWb`.
So `deviceWb` is left inlinable and only the (synthesis-neutral) `traceSignalC` is
added.

## Cycle-2 Df instrumentation was reverted (and why)

Adding `HasCircuitContext` to the low-level `Df.fifo`/`registerFwd`/`registerBwd`
(and the `DfConv`/`PacketStream` wrappers + bittide's `axiStreamPacketFifo`) had
two problems: (1) per Finding 5 it produced **no wires** (they aren't
circuit-notation), and (2) the viral constraint cascaded into **every** Df/stream
user, breaking `cabal build all` — clash-protocols' own tests, `bittide-extra`,
`bittide-experiments`, exes, doctests all failed to compile. It was pure cost.

**Resolution (the operating rule going forward):** only the plugin's *auto-traced*
constructs justify the viral `HasCircuitContext` — circuit-notation `-<` combinators
(memmap) and `OPAQUE` scope roots. For anything the plugin cannot name (low-level
`Circuit go` internals like a Df fifo's state), do **not** blanket-instrument the
combinator; instead call the explicit user API **`traceSignalC`** at the specific
signal you want, exactly as the register-content trace does. That keeps the viral
constraint opt-in and localized to where a trace is actually added, instead of
riding through every transitive user.

So the Df/protocols `HasCircuitContext` edits were reverted. What remains:
memmap (circuit-notation `-<` bus wires + explicit `traceSignalC` register
contents), the 7 memmap-driven `HasCircuitContext`/`withoutCircuitContext` call-site
fixes in bittide/bittide-instances, and `clash-cores`'s circuit-notation
`etherboneC`. With those, **`cabal build all` is green** (dep test-suites stay
`tests: False`, matching upstream where they are non-local source-repo-packages).

## Cost

Every edit to a low package (`clash-protocols`, `clash-protocols-base`) forces a
full downstream recompile (protocols → memmap → cores → bittide → instances),
~20–40 min each. This dominates the effort; the source edits themselves are
one-liners. `-fdefer-type-errors` is the efficient way to census the whole
virality surface in a single build instead of iterating module-by-module.

## Recommendation

- **Instrument circuit-notation combinators; skip low-level `Circuit`-constructor
  ones.** The memmap register/device layer is a clear win (+54 real wires, tiny
  virality, values intact) and could be upstreamed behind an opt-in flag. Adding
  `HasCircuitContext` to `Df.fifo` et al. costs virality + a full rebuild and
  returns no wires (Finding 5) — not worth it unless those combinators are first
  rewritten in circuit-notation or made `OPAQUE` scope roots.
- **Virality scales with combinator level** (Finding 2): prefer high-level
  combinators near the design leaves; low-level primitives cascade through every
  wrapper that re-exports them.
- Consider whether ccc should offer a **non-viral default** (e.g. the plugin
  supplying `noCircuitContext` where the constraint is otherwise unsatisfied) to
  remove the propagation burden entirely — the single biggest ergonomic cost, even
  though it turned out small here thanks to the pre-existing `?byteOrder` threading.
- The instrumented `deps/` checkouts + repointed `cabal.project` are left in place
  as a ready harness for further dogfooding.
