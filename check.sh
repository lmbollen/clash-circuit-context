#!/usr/bin/env bash
# clash-circuit-context acceptance: build + run all suites + golden VCD diffs.
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

for g in manual-smoke auto-smoke; do
  if diff <(grep -v '^\$date' "$g.vcd") "goldens/$g.vcd" >/dev/null; then
    echo "ok: $g.vcd matches golden"
  else
    echo "FAIL: $g.vcd differs from golden"; fail=1
  fi
done

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
