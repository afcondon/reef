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

echo "ALL GREEN — Odonus engine + PitchSet table identical on JS + Erlang, match goldens."
