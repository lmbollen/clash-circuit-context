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

run cabal build all -j1 "$@" 2>&1 | tee "$log"
run cabal test manual-smoke -j1 "$@"
run cabal test fallback -j1 "$@"
run cabal test auto-smoke -j1 "$@" 2>&1 | tee -a "$log"
# timeout: a strict port trace would deadlock on circuit-notation's lazy let
# knot (silent hang under the threaded RTS) — fail loudly instead.
run timeout 600 cabal test notation-smoke -j1 "$@"
run cabal test shockwaves-smoke -j1 "$@"

for g in manual-smoke auto-smoke notation-smoke; do
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
if grep -qE '^bz+ ' auto-smoke.vcd; then
  echo "ok: not-evaluated cycles render 'z' (distinct from undefined 'x')"
else
  echo "FAIL: expected a 'z' (not-evaluated) value in auto-smoke.vcd"; fail=1
fi
if grep -qE '^b[01]+x' auto-smoke.vcd; then
  echo "ok: partial undefined keeps defined bits (no false zero)"
else
  echo "FAIL: expected a partial 'b…x' value (defined+undefined) in auto-smoke.vcd"; fail=1
fi

# The guarded-body warning must have fired during auto-smoke compilation
# (only checkable on a fresh build; skip silently when cached).
if grep -q "Compiling Main.*AutoSmoke" "$log" 2>/dev/null; then
  if grep -q "skipping component wrap for an equation of 'fallthrough'" "$log"; then
    echo "ok: guarded-body warning fired"
  else
    echo "FAIL: expected plugin warning missing"; fail=1
  fi
fi

rm -f "$log"
[ "$fail" -eq 0 ] && echo "ALL CHECKS PASSED" || echo "CHECKS FAILED"
exit "$fail"
