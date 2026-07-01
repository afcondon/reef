#!/usr/bin/env bash
# Cross-runtime conformance for the Reef Odonus engine.
#
# Proves Reef.Odonus.stepEmit produces identical fired events under the JS
# backend (node) and purerl (Erlang) — the guarantee that makes wiring
# odonus_voice.erl onto reef_odonus@ps:stepEmit safe. Diffs both against the
# committed golden.
#
# Needs a purerl workspace that depends on reef for the Erlang side; we use
# purerl-tidal (which already does). Run from the reef package root.
set -euo pipefail

REEF="$(cd "$(dirname "$0")/.." && pwd)"
PURERL="$REEF/../live-coding/purerl-tidal"
GOLDEN="$REEF/conformance/odonus-golden.txt"
DATA='^ *[0-9]+ \|'

echo "== node (JS backend) =="
( cd "$REEF" && spago build >/dev/null 2>&1 \
  && node --input-type=module \
       -e 'import { run } from "./output/Reef.Conformance/index.js"; process.stdout.write(run);' \
  ) | grep -E "$DATA" > /tmp/reef-node-out.txt
echo "== erl (purerl, via purerl-tidal) =="
( cd "$PURERL" && spago build >/dev/null 2>&1 && make erl-quick >/dev/null 2>&1 \
  && erl -pa ebin -noshell -eval 'io:format("~s~n", ['"'"'reef_conformance@ps'"'"':run()]), halt().' 2>/dev/null ) \
  | grep -E "$DATA" > /tmp/reef-erl-out.txt

echo "== diff node vs erl =="
diff /tmp/reef-node-out.txt /tmp/reef-erl-out.txt && echo "  ✅ node == erl"
echo "== diff vs committed golden =="
diff "$GOLDEN" /tmp/reef-node-out.txt && echo "  ✅ matches golden"

# --- PitchSet quantisation table (Reef.PitchSetGolden.tableRender) -----------
# The agreed quantisation examples — literal offsets, flat-equal mapping, finite
# vs periodic, octave vs period-19. Pure, so it must render identically on both
# backends. (The pinned golden itself lives in test/Test/Main.purs.)
echo "== PitchSet table: node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { tableRender } from "./output/Reef.PitchSetGolden/index.js"; process.stdout.write(tableRender);' \
  ) > /tmp/reef-pitchset-node.txt
( cd "$PURERL" && erl -pa ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_pitchSetGolden@ps'"'"':tableRender()]), halt().' 2>/dev/null \
  ) > /tmp/reef-pitchset-erl.txt
diff /tmp/reef-pitchset-node.txt /tmp/reef-pitchset-erl.txt && echo "  ✅ PitchSet table node == erl"

# --- generative determinism net (Reef.Conformance.genRun) --------------------
# THE LOCKSTEP FOUNDATION (P1). 2000 steps with the 10 non-transcendental gen
# sources active, threading runGen then stepEmit, the full evolving state sampled
# every 25 steps. Must be byte-identical on both runtimes — that is what makes a
# frontend co-simulation provably track the rig (reef/docs/PLAN-lockstep-cosimulation.md).
GEN_GOLDEN="$REEF/conformance/genrun-golden.txt"
echo "== genRun (2000-step generative): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { genRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(genRun);' \
  ) > /tmp/reef-genrun-node.txt
( cd "$PURERL" && erl -pa ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':genRun()]), halt().' 2>/dev/null \
  ) > /tmp/reef-genrun-erl.txt
diff /tmp/reef-genrun-node.txt /tmp/reef-genrun-erl.txt && echo "  ✅ genRun node == erl (generative engine identical over 2000 steps)"
diff "$GEN_GOLDEN" /tmp/reef-genrun-node.txt && echo "  ✅ genRun matches frozen golden"

# --- Beta / pow transcendental diagnostic (Reef.Conformance.betaProbe) -------
# The one transcendental in the engine: `pow`, inside the Marbles Beta weights
# (GNotes). IEEE 754 does not mandate a correctly-rounded pow, so this is the one
# place V8 Math.pow and BEAM math:pow could diverge. Empirically they agree here;
# this guards that assumption.
BETA_GOLDEN="$REEF/conformance/beta-golden.txt"
echo "== betaProbe (pow): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { betaProbe } from "./output/Reef.Conformance/index.js"; process.stdout.write(betaProbe);' \
  ) > /tmp/reef-beta-node.txt
( cd "$PURERL" && erl -pa ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':betaProbe()]), halt().' 2>/dev/null \
  ) > /tmp/reef-beta-erl.txt
diff /tmp/reef-beta-node.txt /tmp/reef-beta-erl.txt && echo "  ✅ betaProbe node == erl (V8 Math.pow == BEAM math:pow on these inputs)"
diff "$BETA_GOLDEN" /tmp/reef-beta-node.txt && echo "  ✅ betaProbe matches frozen golden"

# --- input-protocol determinism net (Reef.Conformance.inputRun) --------------
# THE P3 PROOF (reef/docs/PLAN-lockstep-cosimulation.md). A scripted tick-tagged
# stream of user actions (every Input family, incl. the seed-threading rolls)
# replayed in lockstep — each input encoded to JSON and decoded back THROUGH the
# Reef.Protocol codec on the critical path, interleaved with the autonomous gen
# sources and stepEmit, one seed threading both. The full SimState (Odonus + gen
# config + pad + seed) is digested. Byte-identical here = the input protocol AND
# its codec behave identically on both runtimes, which is what lets a frontend
# broadcast tick-tagged inputs and have the rig apply them to the same state.
INPUT_GOLDEN="$REEF/conformance/inputrun-golden.txt"
echo "== inputRun (400-step lockstep input replay): node vs erl =="
# UNLIKE the engine goldens above, inputRun exercises simple-json's wire codec
# (encodeInput/decodeInput) — which on the BEAM defers to `jsx`. So the Erlang
# side needs jsx's rebar dep ebin on its code path, not just `-pa ebin`. This is
# the same dependency reef_voice will need to decode tick-tagged inputs live.
# Add ONLY jsx's ebin: the broader `_build/.../*/ebin` glob would also pull in
# purerl_tidal's own ebin, whose stale reef_conformance@ps.beam (rebar copies
# reef in; `make erl-quick` only refreshes the root ebin/) shadows the fresh one.
( cd "$REEF" && node --input-type=module \
  -e 'import { inputRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(inputRun);' \
  ) > /tmp/reef-inputrun-node.txt
( cd "$PURERL" && erl -pa ebin _build/default/lib/jsx/ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':inputRun()]), halt().' 2>/dev/null \
  ) > /tmp/reef-inputrun-erl.txt
diff /tmp/reef-inputrun-node.txt /tmp/reef-inputrun-erl.txt && echo "  ✅ inputRun node == erl (input protocol + codec identical over 400 steps)"
diff "$INPUT_GOLDEN" /tmp/reef-inputrun-node.txt && echo "  ✅ inputRun matches frozen golden"

# --- SimState handoff net (Reef.Conformance.simRun) ---------------------------
# THE P4d PROOF. Both runtimes DECODE the same real handoff JSON (a SimState the
# JS frontend encoded, gen on + a seed) via Reef.Protocol.decodeSim, then step
# stepTick from it. Byte-identical here = the BEAM's decodeSim rebuilds exactly
# the state the browser encoded and evolves it identically — i.e. the lockstep
# handoff lands the rig on the frontend's state. Also uses simple-json → jsx.
SIM_GOLDEN="$REEF/conformance/simrun-golden.txt"
echo "== simRun (SimState handoff decode + 400-step step): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { simRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(simRun);' \
  ) > /tmp/reef-simrun-node.txt
( cd "$PURERL" && erl -pa ebin _build/default/lib/jsx/ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':simRun()]), halt().' 2>/dev/null \
  ) > /tmp/reef-simrun-erl.txt
diff /tmp/reef-simrun-node.txt /tmp/reef-simrun-erl.txt && echo "  ✅ simRun node == erl (handoff decode + step identical over 400 steps)"
diff "$SIM_GOLDEN" /tmp/reef-simrun-node.txt && echo "  ✅ simRun matches frozen golden"

echo "ALL GREEN — Odonus engine + PitchSet table + generative net + pow probe + input protocol + SimState handoff identical on JS + Erlang, match goldens."
