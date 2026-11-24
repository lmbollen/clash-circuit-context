#!/usr/bin/env bash
# clash-circuit-context acceptance: build + run all suites + golden VCD diffs.
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
# The clash-shockwaves half: the ADT sidecar written beside the VCD.
run cabal test adt-sidecar -j1 "$@"
# Constraint synonyms, idempotence under a double plugin enable, and the
# compile-time diagnostics (grepped from the build log below).
runlog cabal test plugin-diagnostics -j1 "$@"
# The plugin after the typelits solver plugins: compiling this suite is the
# test that the oracle leaves GHC's rounds of plugin progress to them. Each
# module is held to the rounds it takes without the plugin (see the suite's
# ghc-options), so a change that adds one does not compile.
run cabal test typelits-plugins -j1 "$@"
# The Example level (usage, kept honest by running) and the Test level
# (recorder properties, the capture contract, and the tasty-sequenced tests
# that decode the waveforms the Example level wrote).
run cabal test waveform-tests -j1 "$@"
# The same suite as it runs where a spill file is not mapped (every platform
# but Linux: it is written through a Handle, a region at a time), with
# linux_HOST_OS undefined in every module (scripts/not-linux.h).
run cabal test waveform-tests -j1 --builddir=dist-not-linux \
  --ghc-options="-optP-include -optP$PWD/scripts/not-linux.h" "$@"

for g in manual-instrumentation auto-instrumentation; do
  if diff <(grep -v '^\$date' "$g.vcd") "goldens/$g.vcd" >/dev/null; then
    echo "ok: $g.vcd matches golden"
  else
    echo "FAIL: $g.vcd differs from golden"; fail=1
  fi
done

# The FST of the same recordings must show what the VCD shows, value for
# value. Read with GTKWave's fst2vcd, so checked where it is installed.
if command -v fst2vcd >/dev/null && command -v python3 >/dev/null; then
  for g in manual-instrumentation auto-instrumentation; do
    if fst2vcd "$g.fst" 2>/dev/null >"$g.fst.vcd" \
      && python3 scripts/same-waveform.py "$g.vcd" "$g.fst.vcd"; then
      echo "ok: $g.fst shows what $g.vcd shows"
    else
      echo "FAIL: $g.fst differs from $g.vcd"; fail=1
    fi
  done
else
  echo "skip: fst2vcd (GTKWave) or python3 is missing, so the FSTs are not compared with the VCDs"
fi

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
  # A last equation whose guards can all fail still warns, about the
  # function rather than about the plugin's encoding of its guards (#13).
  if grep -qE "In an equation for .partialGuard." "$log" \
    && ! grep -qE "Patterns of type .\(\). not matched" "$log"; then
    echo "ok: incomplete-guards warning names the function"
  else
    echo "FAIL: incomplete-guards warning missing or about the encoding"; fail=1
  fi
fi

# Every warning category must have fired while compiling
# Test/PluginDiagnostics.hs, tagged with the flag that controls it. A
# diagnostic that stops firing is exactly as silent as the bug it reports, so
# it is checked like any other output. Same fresh-build caveat as above.
# Custom warning categories are GHC >= 9.8; below that the same messages
# arrive as plain warnings, untagged, so only the messages are checked there.
ghcver=$(cabal exec "$@" -- ghc --numeric-version 2>/dev/null | tail -1)
case "$ghcver" in
  9.6.*) tags=() ;;
  *) tags=(
       "[-Wx-circuit-context-uninstrumented]"
       "[-Wx-circuit-context-undecided]"
       "[-Wx-circuit-context-untraced]"
       "[-Wx-circuit-context-undescribed]"
     ) ;;
esac
# A width that GHC cannot know (Test/SideConditions.hs, unknownOperand, in the
# typelits-plugins suite): asserted when that module was compiled into this log.
if grep -q "Compiling.*SideConditions" "$log" 2>/dev/null; then
  if grep -qF "no instance for: KnownNat m" "$log"; then
    echo "ok: diagnostic fired: no instance for: KnownNat m"
  else
    echo "FAIL: expected diagnostic missing: no instance for: KnownNat m"; fail=1
  fi
fi
if grep -q "Compiling Main.*PluginDiagnostics" "$log" 2>/dev/null; then
  for want in \
    ${tags[@]+"${tags[@]}"} \
    "'noOpaque' has HasCircuitContext but no OPAQUE" \
    "'unsigned' is OPAQUE but has no type signature" \
    "'bothModes' has both HasProbe and HasCircuitContext" \
    "not traced: Signal dom Undescribed" \
    "no instance for: BitPack Undescribed" \
    "not traced: Pair dom" \
    "Give it a Traceable instance" \
    "not probed: Undescribed" \
    "not traced: Signal dom Tags" \
    "type instance NonTranslatable Tags = 'True" \
    "not traced: Both" \
    "not traced: Signal dom (Maybe Integer)" \
    "use Signed 64 in its place" \
    "recorded as raw bits, not described: Bare" \
    "no instance for: Waveform Bare" \
    "the oracle could not decide: 1 <= n" \
    "the oracle could not decide: KnownNat (n + 1)" \
    "the oracle could not decide: BitPack (Overlapped tag)" \
    "Name the binding with traceNamed, which takes Traceable" \
    "not traced: pulse :: Signal System Bool" \
    "not traced: konst :: Signal System Bool" \
    "not traced: hiddenPulse :: Signal dom Bool" \
    "bind them there with an explicit forall" \
    "Give it its type signature to record it: pulse :: Signal System Bool"
  do
    if grep -qF "$want" "$log"; then
      echo "ok: diagnostic fired: $want"
    else
      echo "FAIL: expected diagnostic missing: $want"; fail=1
    fi
  done
  # Higher-rank bindings are never reported. A local is a function, so it is
  # no trace target and is left alone (Test/Downstream.hs rankN, probedRankN).
  # A component is eta-expanded, or under ImpredicativeTypes wrapped as
  # written, so it keeps its scope; plugin-diagnostics asserts the scopes. No
  # diagnostic may name any of them. Absence is the assertion, so it is only
  # made where it means something: when Test/Downstream.hs, which holds most
  # of these binders, was recompiled into this log. The others are in
  # Test/DeclGroups.hs, Test/Impredicative.hs and (GHC >= 9.10)
  # Test/TypeAbstractions.hs, which are recompiled with it on a fresh build.
  if ! grep -q "Compiling Test.Downstream" "$log"; then
    echo "skip: Test/Downstream.hs was not recompiled, so silence about higher-rank bindings proves nothing"
  else
    quiet=ok
    for binder in "'withComponentRst''" "'belowSyn'" "'belowImportedSyn'" \
                  "'belowOpSyn'" "'withRstProbe'" "'withRst'" \
                  "'topRankN'" "'topRankNLocalSyn'" "'topRankNImportedSyn'" \
                  "'topRankNOpSyn'" "'withSyncReset'" "'withPickedReset'" \
                  "'splitRst'" \
                  "'topRankNInvis'" "'impredicative'" "'impredGuarded'" \
                  "'impredTwoEqs'" "'impredIf'" "'impredList'" \
                  "'impredLambda'" "'impredPointFree'"; do
      if grep -qF "$binder" "$log"; then
        echo "FAIL: a diagnostic names the higher-rank binder $binder"; fail=1
        quiet=
      fi
    done
    [ -n "$quiet" ] && echo "ok: higher-rank bindings are instrumented or left alone, without a word"
  fi
  # A closed binding GHC generalised has no single waveform, and is not
  # reported (Test/Downstream.hs unsignedClosed.nothings).
  if grep -qF "nothings ::" "$log"; then
    echo "FAIL: reported a closed binding that GHC generalised"; fail=1
  else
    echo "ok: a generalised closed binding is not reported"
  fi
  # The signature a closed-binding warning names needs no forall hint when
  # the enclosing signature already binds its type variables with an
  # explicit forall under ScopedTypeVariables (Test/Downstream.hs
  # unsignedClosedScoped.scopedPulse). The warning ends at GHC's excerpt.
  if grep -q "Compiling Test.Downstream" "$log"; then
    scoped=$(awk '/not traced: scopedPulse ::/ {p = 1} p && /^ *[0-9]* *\|/ {exit} p' "$log")
    if [ -z "$scoped" ]; then
      echo "FAIL: no warning for the closed binding scopedPulse"; fail=1
    elif grep -q "explicit forall" <<<"$scoped"; then
      echo "FAIL: the warning for scopedPulse says to bind a type variable already in scope"; fail=1
    else
      echo "ok: a closed-binding warning gives no forall hint for a scoped type variable"
    fi
  fi
  # Integer inside a composite: no BitPack, and the fix is Signed 64, never a
  # NonTranslatable declaration (that would silence every later real fix).
  if grep -qF "NonTranslatable Integer" "$log"; then
    echo "FAIL: offered to declare Integer NonTranslatable"; fail=1
  fi
  # Bindings no recorder could ever take are not reported: plain values in a
  # traced component, types declared NonTranslatable (lists, functions, a
  # String, a user's Names), a signal or a clock inside a probed step
  # (Test/PluginDiagnostics.hs neverWires and probedStep). A warning about
  # them has no fix. Integer and Natural are NOT among them: Clash translates
  # them, so the recorder records them, and must not report them either.
  never=$(grep -E "not (traced|probed): (\[Char\]|String|Integer|Natural|Bool|Vec 4 Bool|Int -> Int|Signal dom \[Int\]|Signal dom Integer|Signal dom Natural|Signal dom Names|Signal System Int|Clock System|SNat [0-9]+|Ptr Int|FunPtr \(Int -> Int\)|TypeRep|Dynamic|SomeException|Signal dom \(Ptr Int\))$" "$log")
  if [ -n "$never" ]; then
    echo "FAIL: reported a binding no recorder could take:"; echo "$never"; fail=1
  else
    echo "ok: bindings no recorder could take are not reported"
  fi
  # ...and the marker must silence exactly the binder it vouches for, while
  # the same shape without it still reports (Helios F4). Absence is the
  # assertion, so it is checked against the same fresh log -- fresh for
  # Test/Downstream.hs, where both harnesses live. Main recompiling does not
  # mean it did: a change to another module of the suite recompiles Main
  # alone, and a cached Downstream says nothing either way.
  if ! grep -q "Compiling Test.Downstream" "$log"; then
    echo "skip: Test/Downstream.hs was not recompiled, so its warnings are not in the log"
  elif grep -qF "'runHarness'" "$log"; then
    echo "FAIL: {-# ANN … NoCircuitScope #-} did not silence its binder"; fail=1
  elif grep -qF "'runHarnessUnvouched'" "$log"; then
    echo "ok: NoCircuitScope silences only the binder it vouches for"
  else
    # Both quiet means the marker silenced the category, or the module never
    # built -- either way the pair proves nothing.
    echo "FAIL: the unvouched harness did not warn, so the marker is untested"
    fail=1
  fi
fi

# The rewriter decides no flag that a given holds: GHC solves that wanted
# from the given. So the plugin warns about Test/RewriterShapes.hs on each
# line marked "-- warns", as many times as it says ("-- warns 2"; once if
# no number), and nowhere else. Same fresh-build caveat.
if grep -q "Compiling Test.RewriterShapes" "$log" 2>/dev/null; then
  shapes=tests/Test/RewriterShapes.hs
  got=$(awk -v f="$shapes" '
    index($0, f ":") == 1 && / warning/ { split($0, p, ":"); at = p[2] ":" p[3]; next_is = 1; next }
    next_is { if ($0 ~ /^ *(not (traced|probed)|recorded as raw bits)/) print at; next_is = 0 }
  ' "$log" | sort -u)
  gotLines=$(printf '%s\n' "$got" | cut -d: -f1 | grep . | sort -n | tr '\n' ' ')
  wantLines=$(awk 'match($0, /-- warns( [0-9]+)?$/) {
      n = substr($0, RSTART + 9); n = (n == "") ? 1 : n + 0
      for (i = 0; i < n; i++) print NR
    }' "$shapes" | sort -n | tr '\n' ' ')
  if [ "$gotLines" = "$wantLines" ]; then
    echo "ok: no flag that a given holds is decided (warnings at lines $gotLines)"
  else
    echo "FAIL: warnings of $shapes at lines [$gotLines], want [$wantLines]"; fail=1
  fi
fi

# A payload without Waveform is still traced and probed, as raw bits. That
# is reported -- it asks for the instance -- but as undescribed, never as
# untraced/undecided: those mean a LOST signal, and
# -Werror=x-circuit-context-untraced must not fail a build whose signals are
# all there. Test/AdtSidecar.hs traces and probes such a payload (Ticks) on
# purpose. Same fresh-build caveat.
if grep -q "Compiling Main.*AdtSidecar" "$log" 2>/dev/null; then
  if grep -qE "not (traced|probed): .*Ticks" "$log"; then
    echo "FAIL: a recorded (only undescribed) payload was reported as lost"; fail=1
  elif grep -qF "recorded as raw bits, not described: Ticks" "$log"; then
    echo "ok: an undescribed payload is reported as undescribed, not lost"
  else
    echo "FAIL: an undescribed payload drew no undescribed warning"; fail=1
  fi
fi

# The categories are real GHC warnings, so "strict mode" is -Werror= and
# "quiet" is -Wno-. That is a claim about the compiler's behaviour, not ours,
# which is exactly why it is checked rather than described. Custom warning
# categories are GHC >= 9.8; below that the plugin still warns, just not per
# category, so there is nothing to check.
case "$ghcver" in
  9.6.*)
    echo "skip: per-category warning control needs GHC >= 9.8 (have $ghcver)" ;;
  *)
    scratch=$(mktemp -d)
    cat > "$scratch/StrictProbe.hs" <<'HS'
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}

-- | A binding the oracle declines and could have recorded (a polymorphic
-- value: it might hold a signal), so a plugin warning fires and nothing else
-- does. Deliberately free of clash-prelude imports: this is compiled
-- through `cabal exec ghc`, whose ambient package environment is not ours to
-- control, and a module that names only this package cannot trip over a
-- duplicate registration of someone else's.
module StrictProbe where

import Clash.CircuitContext (HasCircuitContext)

notTraced :: (HasCircuitContext) => a -> a
notTraced k = out
 where
  out = k
HS
    # The script's own arguments are cabal's (-w <ghc>); probe's are GHC's.
    cabalargs=("$@")
    probe() {
      cabal exec ${cabalargs[@]+"${cabalargs[@]}"} -- ghc -v0 -fno-code \
        -fforce-recomp -package clash-circuit-context \
        "$@" "$scratch/StrictProbe.hs" 2>&1
    }
    # Captured, not piped: under `set -o pipefail` a pipeline inherits the
    # FAILING compile's status, so `probe -Werror=… | grep error` reports
    # not-fatal exactly when the flag worked. Same trap as the runlog note.
    default_out=$(probe)
    quiet_out=$(probe -Wno-x-circuit-context-untraced); quiet_rc=$?
    strict_out=$(probe -Werror=x-circuit-context-untraced); strict_rc=$?

    # A module that fails to type-check emits no x-circuit-context warning
    # EITHER, so the -Wno- case below -- whose assertion is an ABSENCE -- would
    # pass on a broken module. Check that it built before believing it went
    # quiet. (The lesson is the downstream audit's; it cost them two retracted
    # conclusions while writing their reproducers.)
    if [ "$quiet_rc" -ne 0 ]; then
      echo "FAIL: the -Wno- probe did not compile, so its silence proves nothing"
      fail=1
    fi

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
    # Nonzero alone is not enough: ANY compile error is nonzero. The failure
    # has to be OUR category being promoted, so the output must say so.
    case "$strict_rc:$strict_out" in
      0:*)
        echo "FAIL: -Werror=x-circuit-context-untraced was not fatal"; fail=1 ;;
      *-Werror=x-circuit-context-untraced*)
        echo "ok: -Werror= makes a declined binding fatal (strict mode)" ;;
      *)
        echo "FAIL: the -Werror probe failed for some other reason"; fail=1 ;;
    esac
    rm -rf "$scratch"
    ;;
esac

# Without the plugin, an empty Traceable instance does not compile. The
# error must say how to fix that (turn the plugin on), not only name the
# internals of the generic path (AutoTrace, CanTrace, $dmtraceNamed). With
# the plugin, the same module must compile: the plugin solves the marker.
# Free of clash-prelude imports, for the reason given at StrictProbe above.
scratch=$(mktemp -d)
cat > "$scratch/NoPlugin.hs" <<'HS'
{-# LANGUAGE DeriveGeneric #-}

module NoPlugin where

import GHC.Generics (Generic)

import Clash.CircuitContext (Traceable)

data Pair a = Pair a a
  deriving (Generic)

instance Traceable (Pair a)
HS
cabalargs=("$@")
noplugin() {
  cabal exec ${cabalargs[@]+"${cabalargs[@]}"} -- ghc -v0 -fno-code \
    -fforce-recomp -package clash-circuit-context \
    "$@" "$scratch/NoPlugin.hs" 2>&1
}
off_out=$(noplugin); off_rc=$?
on_out=$(noplugin -fplugin=Clash.CircuitContext.Plugin); on_rc=$?
case "$off_rc:$off_out" in
  0:*)
    echo "FAIL: an empty Traceable instance compiled without the plugin"; fail=1 ;;
  *TurnOnThePlugin*"add -fplugin=Clash.CircuitContext.Plugin"*)
    echo "ok: without the plugin, the error says to turn it on" ;;
  *)
    echo "FAIL: without the plugin, the error does not say to turn it on:"
    echo "$off_out" | head -5; fail=1 ;;
esac
if [ "$on_rc" -eq 0 ]; then
  echo "ok: with the plugin, the same instance compiles"
else
  echo "FAIL: with the plugin, the empty instance did not compile:"
  echo "$on_out" | head -5; fail=1
fi

# A design compiled without the plugin does not compile: a call of a
# function that asks for HasCircuitContext needs TurnOnTheCircuitContextPlugin,
# which only the plugin solves. Without it the call sites would reach the
# components Clash makes, one per call (clash ignores an OPTIONS_GHC plugin).
cat > "$scratch/Design.hs" <<'HS'
{-# LANGUAGE ImplicitParams #-}

module Design where

import Clash.CircuitContext

double :: (HasCircuitContext) => Int -> Int
double x = y
 where
  y = x + x
{-# OPAQUE double #-}

top :: Int -> Int
top x = withoutCircuitContext (double x)
HS
design() {
  cabal exec ${cabalargs[@]+"${cabalargs[@]}"} -- ghc -v0 -fno-code \
    -fforce-recomp -package clash-circuit-context \
    "$@" "$scratch/Design.hs" 2>&1
}
off_out=$(design); off_rc=$?
on_out=$(design -fplugin=Clash.CircuitContext.Plugin); on_rc=$?
case "$off_rc:$off_out" in
  0:*)
    echo "FAIL: an instrumented design compiled without the plugin"; fail=1 ;;
  *TurnOnTheCircuitContextPlugin*)
    echo "ok: without the plugin, an instrumented design says to turn it on" ;;
  *)
    echo "FAIL: without the plugin, the error does not say to turn it on:"
    echo "$off_out" | head -5; fail=1 ;;
esac
if [ "$on_rc" -eq 0 ]; then
  echo "ok: with the plugin, the same design compiles"
else
  echo "FAIL: with the plugin, the design did not compile:"
  echo "$on_out" | head -5; fail=1
fi

# A top entity that asks for HasCircuitContext is an error: its ports would
# be named after the call sites and the context. Top.hs has an annotation;
# TopByName.hs has none, but Clash takes a top-level binding named topEntity
# as a top entity by its name.
cat > "$scratch/Top.hs" <<'HS'
{-# LANGUAGE ImplicitParams #-}

module Top where

import Clash.Annotations.TopEntity

import Clash.CircuitContext

sbc :: (HasCircuitContext) => Int -> Int
sbc x = x + 1
{-# OPAQUE sbc #-}
{-# ANN sbc (Synthesize {t_name = "sbc", t_inputs = [PortName "x"], t_output = PortName "y"}) #-}
HS
cat > "$scratch/TopByName.hs" <<'HS'
{-# LANGUAGE ImplicitParams #-}

module TopByName where

import Clash.CircuitContext

topEntity :: (HasCircuitContext) => Int -> Int
topEntity x = x + 1
{-# OPAQUE topEntity #-}
HS
for top in Top TopByName; do
  top_out=$(cabal exec ${cabalargs[@]+"${cabalargs[@]}"} -- ghc -v0 -fno-code \
    -fforce-recomp -package clash-circuit-context \
    -fplugin=Clash.CircuitContext.Plugin "$scratch/$top.hs" 2>&1); top_rc=$?
  case "$top_rc:$top_out" in
    0:*)
      echo "FAIL: a top entity that asks for HasCircuitContext compiled ($top.hs)"; fail=1 ;;
    *"is a top entity, and its signature asks for HasCircuitContext"*)
      echo "ok: a top entity that asks for HasCircuitContext is an error ($top.hs)" ;;
    *)
      echo "FAIL: a top entity that asks for HasCircuitContext gave another error ($top.hs):"
      echo "$top_out" | head -5; fail=1 ;;
  esac
done
rm -rf "$scratch"

# An incremental build after an edit that leaves an interface as it was. A
# (with the plugin) is split into the wrapper f and the worker $wf; B, not
# recompiled, keeps an unfolding that calls A.$wf; C, recompiled, reads it.
# The worker's name was not in GHC's name cache, so reading B's unfolding
# after A was recompiled in the same build failed with "Can't find
# interface-file declaration for variable $wf". Free of clash-prelude
# imports, for the reason given at StrictProbe above.
scratch=$(mktemp -d)
cat > "$scratch/A.hs" <<'HS'
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fplugin=Clash.CircuitContext.Plugin #-}

module A (f) where

import Clash.CircuitContext (HasCircuitContext)

f :: (HasCircuitContext) => Int -> Int
f x = x + 1
{-# OPAQUE f #-}
HS
cat > "$scratch/B.hs" <<'HS'
{-# LANGUAGE ImplicitParams #-}

module B (g) where

import Clash.CircuitContext (HasCircuitContext)

import A (f)

g :: (HasCircuitContext) => Int
g = f 1 + f 2
HS
cat > "$scratch/C.hs" <<'HS'
{-# LANGUAGE ImplicitParams #-}

module C (h) where

import Clash.CircuitContext (HasCircuitContext)

import B (g)

h :: (HasCircuitContext) => Int
h = g + 1
HS
incremental() {
  cabal exec ${cabalargs[@]+"${cabalargs[@]}"} -- ghc -v0 -O -no-link \
    -package clash-circuit-context "-i$scratch" -outputdir "$scratch/out" \
    --make "$scratch/C.hs" 2>&1
}
first_out=$(incremental); first_rc=$?
echo "-- edited" >>"$scratch/A.hs"
echo "-- edited" >>"$scratch/C.hs"
again_out=$(incremental); again_rc=$?
if [ "$first_rc" -ne 0 ]; then
  echo "FAIL: the incremental-build modules did not compile:"
  echo "$first_out" | head -5; fail=1
elif [ "$again_rc" -eq 0 ]; then
  echo "ok: an incremental build reads the worker of a recompiled module"
else
  echo "FAIL: an incremental build after an edit of a plugin module failed:"
  echo "$again_out" | head -5; fail=1
fi
rm -rf "$scratch"

# HDL: each design in tests/hdl compiles to Verilog without optimisation and
# with it, and Calls makes one module of its component. Needs a `clash` that
# sees this package: set CCC_CLASH to one, e.g. the book's scripts/clash.
if [ -n "${CCC_CLASH:-}" ]; then
  hdl=$(mktemp -d)
  for design in tests/hdl/*.hs; do
    name=$(basename "$design" .hs)
    for level in -O0 -O; do
      out=$("$CCC_CLASH" -fplugin=Clash.CircuitContext.Plugin "$level" \
        "-i$PWD/tests/hdl" "$PWD/$design" --verilog -outputdir "$hdl/build-$name$level" \
        -fclash-hdldir "$hdl/out-$name$level" 2>&1)
      if [ $? -eq 0 ] && ! grep -q "error" <<<"$out"; then
        echo "ok: $name compiles to Verilog at $level"
      else
        echo "FAIL: $name does not compile to Verilog at $level:"
        echo "$out" | grep -A3 error | head -8; fail=1
      fi
    done
  done
  for level in -O0 -O; do
    accs=$(find "$hdl/out-Calls$level" -name 'acc*.v' | wc -l)
    if [ "$accs" -eq 1 ]; then
      echo "ok: one acc module at $level"
    else
      echo "FAIL: $accs acc modules at $level, expected one"; fail=1
    fi
  done
  rm -rf "$hdl"
else
  echo "skip: HDL checks (set CCC_CLASH to a clash that sees this package)"
fi

rm -f "$log"
[ "$fail" -eq 0 ] && echo "ALL CHECKS PASSED" || echo "CHECKS FAILED"
exit "$fail"
