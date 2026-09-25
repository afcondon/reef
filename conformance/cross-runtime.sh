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

# --- chord-quantised render net (Reef.Conformance.chordRun) -------------------
# The chord overlay path renders pitches but shipped an octave bug because NO golden
# exercised it (run is chord-off; inputRun digests cell indices). chordRun feeds a
# C-major triad via mkFollowChord and renders 32 steps of SOUNDING pitch. Pure
# (no codec), so plain -pa ebin. Byte-identical here = the chord-quantise fix
# (realize the index to a melodic pitch, then snap to the nearest chord tone) is
# identical on both runtimes.
CHORD_GOLDEN="$REEF/conformance/chord-golden.txt"
echo "== chordRun (32-step chord-quantised render): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { chordRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(chordRun);' \
  ) > /tmp/reef-chord-node.txt
( cd "$PURERL" && erl -pa ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':chordRun()]), halt().' 2>/dev/null ) \
  > /tmp/reef-chord-erl.txt
diff /tmp/reef-chord-node.txt /tmp/reef-chord-erl.txt && echo "  \u2705 chordRun node == erl (chord-quantise render identical over 32 steps)"
diff "$CHORD_GOLDEN" /tmp/reef-chord-node.txt && echo "  \u2705 chordRun matches frozen golden"

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

# --- Balistes input-protocol net (Reef.Conformance.balistesInputRun) ----------
# LIVE KNOB/GESTURE LOCKSTEP. A scripted tick-tagged Balistes session (every BInput
# family incl. BReseed/BReset) round-tripped through Reef.Balistes.Protocol
# (encodeBTagged/decodeBTagged), interleaved with stepBal + renderStep over 200
# steps. Byte-identical here = the input protocol + its codec behave identically on
# both runtimes, so the frontend can broadcast tick-tagged knob edits and the rig
# applies them to the same model step. Uses the codec → BEAM needs jsx's ebin.
BAL_INPUT_GOLDEN="$REEF/conformance/balistes-input-golden.txt"
echo "== balistesInputRun (200-step tick-tagged input replay): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { balistesInputRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(balistesInputRun);' \
  ) > /tmp/reef-balinput-node.txt
( cd "$PURERL" && erl -pa ebin _build/default/lib/jsx/ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':balistesInputRun()]), halt().' 2>/dev/null ) \
  > /tmp/reef-balinput-erl.txt
diff /tmp/reef-balinput-node.txt /tmp/reef-balinput-erl.txt && echo "  ✅ balistesInputRun node == erl (input protocol + codec identical over 200 steps)"
diff "$BAL_INPUT_GOLDEN" /tmp/reef-balinput-node.txt && echo "  ✅ balistesInputRun matches frozen golden"

# --- Balistes fixed-rhythm net (Reef.Conformance.fixedRun) --------------------
# The AFixed lockstep. A hand-written fixed rhythm (conditions, probabilities,
# ratchets) round-tripped through Reef.Balistes.Protocol (encodeFixed/decodeFixed)
# and rendered by the shared renderFixed over 8 loops. Byte-identical here = the
# fixed-rhythm eval (incl. the cellHash multiplications, which rely on NO 32-bit
# wrap — both runtimes give full precision < 2^53) + its codec behave identically,
# so reef_balistes_voice plays a pushed fixed rhythm in lockstep. Codec → jsx ebin.
FIXED_GOLDEN="$REEF/conformance/fixed-golden.txt"
echo "== fixedRun (128-step fixed rhythm): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { fixedRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(fixedRun);' \
  ) > /tmp/reef-fixed-node.txt
( cd "$PURERL" && erl -pa ebin _build/default/lib/jsx/ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':fixedRun()]), halt().' 2>/dev/null ) \
  > /tmp/reef-fixed-erl.txt
diff /tmp/reef-fixed-node.txt /tmp/reef-fixed-erl.txt && echo "  ✅ fixedRun node == erl (fixed-rhythm eval + codec identical over 128 steps)"
diff "$FIXED_GOLDEN" /tmp/reef-fixed-node.txt && echo "  ✅ fixedRun matches frozen golden"

# --- Balistes POLYTRIG net (Reef.Conformance.trigRun) -------------------------
# The TIDAL-tab / ASelene lockstep. A resolved rack (named jacks + an off-grid jack)
# round-tripped through Reef.Balistes.Protocol (encodeTrigKit/decodeTrigKit) and
# sliced by the shared renderTrigStep over two 16-step cycles. The off-grid onsets
# land mid-step, so `frac = onset * cycleSteps - step` is a non-trivial float both
# runtimes must compute identically. Byte-identical here = reef_balistes_voice plays
# a pushed POLYTRIG rack in lockstep with the frontend's ASelene branch. Codec → jsx.
TRIG_GOLDEN="$REEF/conformance/trig-golden.txt"
echo "== trigRun (32-step POLYTRIG rack slice): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { trigRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(trigRun);' \
  ) > /tmp/reef-trig-node.txt
( cd "$PURERL" && erl -pa ebin _build/default/lib/jsx/ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':trigRun()]), halt().' 2>/dev/null ) \
  > /tmp/reef-trig-erl.txt
diff /tmp/reef-trig-node.txt /tmp/reef-trig-erl.txt && echo "  ✅ trigRun node == erl (rack slicing + fractional-onset multiply + codec identical over 32 steps)"
diff "$TRIG_GOLDEN" /tmp/reef-trig-node.txt && echo "  ✅ trigRun matches frozen golden"

# --- Vetula performance-scheduler net (Reef.Conformance.vetulaRun) ------------
# VETULA LOCKSTEP V1. The shared Reef.Vetula.Perf scheduler — a saved chord
# progression fanned to voices with different per-chord dwell schedules (bars per
# chord, 0 = skip) + phase offsets — round-tripped through Reef.Vetula.Protocol
# (encodePerf/decodePerf) then evaluated at each absolute pulse over 128 pulses:
# every voice's read-head plus the → odo conductor's held cursor + the pitch-class
# set it feeds Odonus. Pure integer arithmetic (no seed, no floats, no bits), so it
# is deterministic by construction; byte-identical here = the whole shared scheduler
# + its codec agree, which is what lets reef_vetula_voice conduct the rig's Odonus
# in lockstep with the browser. Codec → the BEAM needs jsx's ebin.
VETULA_GOLDEN="$REEF/conformance/vetula-golden.txt"
echo "== vetulaRun (128-pulse performance scheduler): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { vetulaRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(vetulaRun);' \
  ) > /tmp/reef-vetula-node.txt
( cd "$PURERL" && erl -pa ebin _build/default/lib/jsx/ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':vetulaRun()]), halt().' 2>/dev/null ) \
  > /tmp/reef-vetula-erl.txt
diff /tmp/reef-vetula-node.txt /tmp/reef-vetula-erl.txt && echo "  ✅ vetulaRun node == erl (performance scheduler + codec identical over 128 pulses)"
diff "$VETULA_GOLDEN" /tmp/reef-vetula-node.txt && echo "  ✅ vetulaRun matches frozen golden"

# --- Vetula MIDI-render net (Reef.Conformance.vetulaMidiRun) ------------------
# VETULA LOCKSTEP V2a. The shared renderVoiceMidiAt (block + arp -> gated notes)
# over 128 pulses for the same performance. Uses decodePerf (codec -> jsx), so the
# BEAM needs jsx's ebin. Byte-identical here = reef_vetula_voice emits the same
# MIDI the browser's stepVoice does -- the self-contained sync leg (no Odonus).
VMIDI_GOLDEN="$REEF/conformance/vetula-midi-golden.txt"
echo "== vetulaMidiRun (128-pulse block+arp MIDI render): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { vetulaMidiRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(vetulaMidiRun);' \
  ) > /tmp/reef-vmidi-node.txt
( cd "$PURERL" && erl -pa ebin _build/default/lib/jsx/ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':vetulaMidiRun()]), halt().' 2>/dev/null ) \
  > /tmp/reef-vmidi-erl.txt
diff /tmp/reef-vmidi-node.txt /tmp/reef-vmidi-erl.txt && echo "  OK vetulaMidiRun node == erl (block+arp MIDI render identical over 128 pulses)"
diff "$VMIDI_GOLDEN" /tmp/reef-vmidi-node.txt && echo "  OK vetulaMidiRun matches frozen golden"

# --- Stellatus ring re-sequencer (Reef.Conformance.stellatusRun) --------------
# STELLATUS BEAM WIRING A. The shared Reef.Stellatus.Engine walk (grid-locked arc
# walk + weighted jumps) + events (per-step /dirt/play bag, glitch-folded speed)
# over the fixed loop, 96 absolute steps. Uses decodeScene (codec -> jsx), so the
# BEAM needs jsx's ebin. Byte-identical here = reef_stellatus_voice emits exactly
# what the browser visualizer walks (the whole cross-runtime wire is one push).
STELLATUS_GOLDEN="$REEF/conformance/stellatus-golden.txt"
echo "== stellatusRun (96-step ring walk + glitch + jumps): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { stellatusRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(stellatusRun);' \
  ) > /tmp/reef-stellatus-node.txt
( cd "$PURERL" && erl -pa ebin _build/default/lib/jsx/ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':stellatusRun()]), halt().' 2>/dev/null ) \
  > /tmp/reef-stellatus-erl.txt
diff /tmp/reef-stellatus-node.txt /tmp/reef-stellatus-erl.txt && echo "  OK stellatusRun node == erl (walk + glitch + jumps identical over 96 steps)"
diff "$STELLATUS_GOLDEN" /tmp/reef-stellatus-node.txt && echo "  OK stellatusRun matches frozen golden"

# --- Conspicillum grain selector (Reef.Conformance.conspicillumRun) -----------
# CONSPICILLUM C2. The shared Reef.Conspicillum.Corpus selector: filter, weight,
# seeded draw and grain-window placement, 64 draws from a fixed seed over a
# synthetic Quadrat set. Uses decodeScene (codec -> jsx), so the BEAM needs
# jsx's ebin.
#
# Byte-identical here is what lets the browser DRAW the cloud the rig SOUNDS.
# Conspicillum's browser side is a pure visualizer — it recomputes the cloud
# rather than being told what played — so any divergence is a picture that
# lies, with nothing anywhere reporting it.
#
# Three things a divergence would move, all of them silently: which samples
# survive the filter (including the rule that a sample which cannot answer an
# axis is EXCLUDED, not defaulted — sample 7 carries no `harm`), how the draw
# leans toward an axis, and each grain's begin/end window, which must be
# `sustain / secs` for the sample it landed in or the cloud transposes itself
# sample by sample. Rendered as scaled integers: `show` on a Number is a
# formatting decision the two runtimes do not owe each other.
CONSPICILLUM_GOLDEN="$REEF/conformance/conspicillum-golden.txt"
echo "== conspicillumRun (64 weighted draws + grain windows): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { conspicillumRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(conspicillumRun);' \
  ) > /tmp/reef-conspicillum-node.txt
( cd "$PURERL" && erl -pa ebin _build/default/lib/jsx/ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':conspicillumRun()]), halt().' 2>/dev/null ) \
  > /tmp/reef-conspicillum-erl.txt
diff /tmp/reef-conspicillum-node.txt /tmp/reef-conspicillum-erl.txt && echo "  OK conspicillumRun node == erl (filter, weighting and windows identical over 64 draws)"
diff "$CONSPICILLUM_GOLDEN" /tmp/reef-conspicillum-node.txt && echo "  OK conspicillumRun matches frozen golden"

# --- Conspicillum cloud over a cycle (Reef.Conformance.conspicillumCloudRun) --
# CONSPICILLUM C3. Onsets -> placed, transformed grains: `Every n k` counted
# across cycles, a seeded `Chance` rule beside it, and the per-cycle seed
# derived (not threaded) from the base seed and the cycle number.
#
# Cycles 0,1,2 then 7 OUT OF ORDER. That last one is the point. Conspicillum is
# cycle-ADDRESSED where Stellatus loops, because its browser side is a pure
# visualizer that recomputes rather than being told — so it must be able to ask
# for cycle 7 without having simulated the six before it. A threaded seed would
# make cycle 7 alone differ from cycle 7 reached by playing, and the picture
# would be quietly wrong exactly when the player dropped into a running set.
#
# The `Every 3 0` rule with 8 onsets a cycle is what pins the cross-cycle
# counting: reversed grains land on ordinals 0,3,6 then 9,12,15 (indices 1,4,7)
# then 18,21 — the figure WALKS. A per-cycle reset would restart it at index 0
# every bar, which is both wrong and entirely silent.
CONSPICILLUM_CLOUD_GOLDEN="$REEF/conformance/conspicillum-cloud-golden.txt"
echo "== conspicillumCloudRun (4 cycles, cross-cycle Every + seeded Chance): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { conspicillumCloudRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(conspicillumCloudRun);' \
  ) > /tmp/reef-conspicillum-cloud-node.txt
( cd "$PURERL" && erl -pa ebin _build/default/lib/jsx/ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':conspicillumCloudRun()]), halt().' 2>/dev/null ) \
  > /tmp/reef-conspicillum-cloud-erl.txt
diff /tmp/reef-conspicillum-cloud-node.txt /tmp/reef-conspicillum-cloud-erl.txt && echo "  OK conspicillumCloudRun node == erl (placement, rules and per-cycle seeds identical)"
diff "$CONSPICILLUM_CLOUD_GOLDEN" /tmp/reef-conspicillum-cloud-node.txt && echo "  OK conspicillumCloudRun matches frozen golden"

# --- Conspicillum as Sector (Reef.Conformance.conspicillumSectorRun) ----------
# A one-bar tape as sixteen grains that FOLLOW it, with the walk on (jump, hold
# and home on an 8-step grid, reach 3) and OpShift/OpRatchet in the rules. Pins
# the walk's own seed stream, its reset on every cycle (cycle 7 out of order
# again), and ratchet's expansion — the one place a cycle emits more grains
# than it has onsets.
CONSPICILLUM_SECTOR_GOLDEN="$REEF/conformance/conspicillum-sector-golden.txt"
echo "== conspicillumSectorRun (tape walk + shift + ratchet): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { conspicillumSectorRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(conspicillumSectorRun);' \
  ) > /tmp/reef-conspicillum-sector-node.txt
( cd "$PURERL" && erl -pa ebin _build/default/lib/jsx/ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':conspicillumSectorRun()]), halt().' 2>/dev/null ) \
  > /tmp/reef-conspicillum-sector-erl.txt
diff /tmp/reef-conspicillum-sector-node.txt /tmp/reef-conspicillum-sector-erl.txt && echo "  OK conspicillumSectorRun node == erl (walk, shift and ratchet identical)"
diff "$CONSPICILLUM_SECTOR_GOLDEN" /tmp/reef-conspicillum-sector-node.txt && echo "  OK conspicillumSectorRun matches frozen golden"

# --- Conspicillum harmonic fit (Reef.Conformance.conspicillumHarmonicRun) -----
# CONSPICILLUM C5. A ii-V-i in D minor scored against a corpus of REAL Quadrat
# chord hits — the actual MIDI notes struck, pulled from the chord-hits-* sets.
#
# The instrument's headline claim, made checkable: the same corpus ranks
# differently under each chord, so a progression is realised onto recorded
# voicings rather than transposed onto one sample.
#
# Watch the last two columns. 24 of the 136 chord hits carry no notes and 7 are
# eleven-pitch-class smears, and a smear covers EVERY chord perfectly on
# coverage alone — so if the smear column ever climbs above the real chords,
# the instrument has learned to prefer its broken material. That is what the
# squared foreign-note term in Reef.Conspicillum.Harmonic is defending.
CONSPICILLUM_HARMONIC_GOLDEN="$REEF/conformance/conspicillum-harmonic-golden.txt"
echo "== conspicillumHarmonicRun (ii-V-i over real chord hits): node vs erl =="
( cd "$REEF" && node --input-type=module \
  -e 'import { conspicillumHarmonicRun } from "./output/Reef.Conformance/index.js"; process.stdout.write(conspicillumHarmonicRun);' \
  ) > /tmp/reef-conspicillum-harmonic-node.txt
( cd "$PURERL" && erl -pa ebin _build/default/lib/jsx/ebin -noshell \
  -eval 'io:format("~s", ['"'"'reef_conformance@ps'"'"':conspicillumHarmonicRun()]), halt().' 2>/dev/null ) \
  > /tmp/reef-conspicillum-harmonic-erl.txt
diff /tmp/reef-conspicillum-harmonic-node.txt /tmp/reef-conspicillum-harmonic-erl.txt && echo "  OK conspicillumHarmonicRun node == erl"
diff "$CONSPICILLUM_HARMONIC_GOLDEN" /tmp/reef-conspicillum-harmonic-node.txt && echo "  OK conspicillumHarmonicRun matches frozen golden"
