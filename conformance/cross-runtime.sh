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

# --- Balistes engine determinism net (Reef.Conformance.balistesRun) ----------
# PHASE 0 of the Balistes lockstep. The shared `Reef.Balistes.Engine` (Grids core)
# over 256 steps: X/Y swept across the whole drum-map grid (bilinear interp +
# tables + u8Mix), perturbations resampled every 32 steps threading one RNG seed
# through `Reef.Bits.xorshift32` (the FFI 32-bit primitive), and the trigger/accent
# rule. Byte-identical here = `balistes_voice` can run onto reef_balistes_engine@ps
# and co-simulate the frontend, retiring the two hand-ports (Triggerfish.Balistes.
# Engine + balistes_engine.erl) that could silently disagree on the RNG. Pure
# engine — no simple-json/jsx, so plain `-pa ebin`.
BAL_GOLDEN="$REEF/conformance/balistes-golden.txt"
echo "== balistesRun (256-step Grids engine): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { balistesRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(balistesRun);' \
  ) > /tmp/reef-balistes-node.txt
( cd "$PURERL" && erl -pa ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':balistesRun()]), halt().' 2>/dev/null ) \
  > /tmp/reef-balistes-erl.txt
diff /tmp/reef-balistes-node.txt /tmp/reef-balistes-erl.txt && echo "  ✅ balistesRun node == erl (Grids engine + xorshift32 RNG identical over 256 steps)"
diff "$BAL_GOLDEN" /tmp/reef-balistes-node.txt && echo "  ✅ balistesRun matches frozen golden"

# --- Balistes handoff + shared-render net (Reef.Conformance.balistesSimRun) ---
# P1 of the Balistes lockstep. A full BalSim (engine state + render overlay)
# round-tripped through Reef.Balistes.Protocol (encodeBalSim/decodeBalSim), then
# 128 steps of the shared stepBal digested by what renderStep (open-hat / note /
# Dilla-push / ratchet / accent) emits. Byte-identical here = the WHOLE shared
# Balistes path (codec + engine + render) is identical on both runtimes, so
# reef_balistes_voice co-simulates the frontend from a pushed handoff. Uses the
# codec, so — like inputRun/simRun — the BEAM needs jsx's ebin on its code path.
BAL_SIM_GOLDEN="$REEF/conformance/balistes-sim-golden.txt"
echo "== balistesSimRun (handoff codec + 128-step render): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { balistesSimRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(balistesSimRun);' \
  ) > /tmp/reef-balsim-node.txt
( cd "$PURERL" && erl -pa ebin _build/default/lib/jsx/ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':balistesSimRun()]), halt().' 2>/dev/null ) \
  > /tmp/reef-balsim-erl.txt
diff /tmp/reef-balsim-node.txt /tmp/reef-balsim-erl.txt && echo "  ✅ balistesSimRun node == erl (codec + shared engine + render identical over 128 steps)"
diff "$BAL_SIM_GOLDEN" /tmp/reef-balsim-node.txt && echo "  ✅ balistesSimRun matches frozen golden"
