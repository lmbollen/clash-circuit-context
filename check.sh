#!/usr/bin/env bash
# clash-circuit-context acceptance: build + run all suites + golden VCD diffs.
# NOTE: needs GHC >= 9.8 on this branch (clash-shockwaves uses
# TypeAbstractions): run in deps/bittide-hardware's devshell, or pass -w ghc-9.10.3.
# Usage: ./check.sh [extra cabal args, e.g. -w ghc-9.10.3]
set -uo pipefail
cd "$(dirname "$0")"
fail=0
log=$(mktemp)

run() { echo "== $* =="; "$@" || fail=1; }
# Same, but keeping a copy of the output for the greps below. NOT `run … | tee`:
# a pipeline runs its left side in a SUBSHELL, so `fail=1` would be set in a
# child and lost — which silently reported ALL CHECKS PASSED while auto-instrumentation
# was failing. PIPESTATUS[0] is the command's own status, not tee's.
runlog() { echo "== $* =="; "$@" 2>&1 | tee -a "$log"; [ "${PIPESTATUS[0]}" -eq 0 ] || fail=1; }

runlog cabal build all -j1 "$@"
run cabal test manual-instrumentation -j1 "$@"
run cabal test oracle-fallback -j1 "$@"
runlog cabal test auto-instrumentation -j1 "$@"
# timeout: a strict port trace would deadlock on circuit-notation's lazy let
# knot (silent hang under the threaded RTS) — fail loudly instead.
run timeout 600 cabal test notation -j1 "$@"
run cabal test adt-sidecar -j1 "$@"
# The Example level (usage, kept honest by running) and the Test level
# (recorder properties, the capture contract, and the tasty-sequenced tests
# that decode the waveforms the Example level wrote).
run cabal test waveform-tests -j1 "$@"

for g in manual-instrumentation auto-instrumentation notation; do
  if diff <(grep -v '^\$date' "$g.vcd") "goldens/$g.vcd" >/dev/null; then
    echo "ok: $g.vcd matches golden"
  else
    echo "FAIL: $g.vcd differs from golden"; fail=1
  fi
done

# Value-fidelity invariants (see Core.hs packMaskValue/expandRunsX/renderVC):
#  - a never-sampled cycle renders 'z' (NOT evaluated), never a false '0';
#  - an evaluated-but-undefined value renders 'x', with any DEFINED bits kept
#    (a partial value like 'b0x…' proves we don't zero-out undefined bits).
if grep -qE '^bz+ ' auto-instrumentation.vcd; then
  echo "ok: not-evaluated cycles render 'z' (distinct from undefined 'x')"
else
  echo "FAIL: expected a 'z' (not-evaluated) value in auto-instrumentation.vcd"; fail=1
fi
if grep -qE '^b[01]+x' auto-instrumentation.vcd; then
  echo "ok: partial undefined keeps defined bits (no false zero)"
else
  echo "FAIL: expected a partial 'b…x' value (defined+undefined) in auto-instrumentation.vcd"; fail=1
fi

# The guarded-body warning must have fired during auto-instrumentation compilation
# (only checkable on a fresh build; skip silently when cached).
if grep -q "Compiling Main.*AutoInstrumentation" "$log" 2>/dev/null; then
  if grep -q "skipping component wrap for an equation of 'fallthrough'" "$log"; then
    echo "ok: guarded-body warning fired"
  else
    echo "FAIL: expected plugin warning missing"; fail=1
  fi
fi

rm -f "$log"
[ "$fail" -eq 0 ] && echo "ALL CHECKS PASSED" || echo "CHECKS FAILED"
exit "$fail"
