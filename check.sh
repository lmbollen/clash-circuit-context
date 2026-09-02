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
# Constraint synonyms, idempotence under a double plugin enable, and the
# compile-time diagnostics (grepped from the build log below).
runlog cabal test plugin-diagnostics -j1 "$@"
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

# Every warning category must have fired while compiling
# Test/PluginDiagnostics.hs, tagged with the flag that controls it. A
# diagnostic that stops firing is exactly as silent as the bug it reports, so
# it is checked like any other output. Same fresh-build caveat as above.
if grep -q "Compiling Main.*PluginDiagnostics" "$log" 2>/dev/null; then
  for want in \
    "[-Wx-circuit-context-uninstrumented]" \
    "[-Wx-circuit-context-undecided]" \
    "[-Wx-circuit-context-untraced]" \
    "'noOpaque' has HasCircuitContext but no OPAQUE" \
    "'unsigned' is OPAQUE but has no type signature" \
    "'bothModes' has both HasProbe and HasCircuitContext" \
    "not traced: Signal dom Undescribed" \
    "no instance for: BitPack Undescribed" \
    "the oracle could not decide: 1 <= n"
  do
    if grep -qF "$want" "$log"; then
      echo "ok: diagnostic fired: $want"
    else
      echo "FAIL: expected diagnostic missing: $want"; fail=1
    fi
  done
fi

# The categories are real GHC warnings, so "strict mode" is -Werror= and
# "quiet" is -Wno-. That is a claim about the compiler's behaviour, not ours,
# which is exactly why it is checked rather than described. Custom warning
# categories are GHC >= 9.8; below that the plugin still warns, just not per
# category, so there is nothing to check.
ghcver=$(cabal exec "$@" -- ghc --numeric-version 2>/dev/null | tail -1)
case "$ghcver" in
  9.6.*)
    echo "skip: per-category warning control needs GHC >= 9.8 (have $ghcver)" ;;
  *)
    scratch=$(mktemp -d)
    cat > "$scratch/StrictProbe.hs" <<'HS'
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}

-- | One binding the oracle declines, so exactly one plugin warning fires.
module StrictProbe where

import qualified Prelude as P

import Clash.Explicit.Prelude

import Clash.CircuitContext

notTraced :: (HasCircuitContext) => Int -> Signal System Int -> Signal System Int
notTraced k inp = out
 where
  out = inp + pure (P.length msg)
  msg = P.replicate k 'x'
{-# OPAQUE notTraced #-}
HS
    # The script's own arguments are cabal's (-w <ghc>); probe's are GHC's.
    cabalargs=("$@")
    probe() {
      cabal exec ${cabalargs[@]+"${cabalargs[@]}"} -- ghc -v0 -fno-code \
        -fforce-recomp -package clash-circuit-context -package clash-prelude \
        "$@" "$scratch/StrictProbe.hs" 2>&1
    }
    # Captured, not piped: under `set -o pipefail` a pipeline inherits the
    # FAILING compile's status, so `probe -Werror=… | grep error` reports
    # not-fatal exactly when the flag worked. Same trap as the runlog note.
    default_out=$(probe)
    quiet_out=$(probe -Wno-x-circuit-context-untraced)
    probe -Werror=x-circuit-context-untraced >/dev/null && strict_rc=0 || strict_rc=1

    case "$default_out" in
      *-Wx-circuit-context-untraced*)
        echo "ok: a declined binding warns by default" ;;
      *)
        echo "FAIL: no default warning for a declined binding"; fail=1 ;;
    esac
    case "$quiet_out" in
      *-Wx-circuit-context-untraced*)
        echo "FAIL: -Wno-x-circuit-context-untraced did not silence it"; fail=1 ;;
      *)
        echo "ok: -Wno- silences a category" ;;
    esac
    if [ "$strict_rc" -ne 0 ]; then
      echo "ok: -Werror= makes a declined binding fatal (strict mode)"
    else
      echo "FAIL: -Werror=x-circuit-context-untraced was not fatal"; fail=1
    fi
    rm -rf "$scratch"
    ;;
esac

rm -f "$log"
[ "$fail" -eq 0 ] && echo "ALL CHECKS PASSED" || echo "CHECKS FAILED"
exit "$fail"
