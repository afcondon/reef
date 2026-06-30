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
( cd "$REEF" && spago test 2>/dev/null ) | grep -E "$DATA" > /tmp/reef-node-out.txt
echo "== erl (purerl, via purerl-tidal) =="
( cd "$PURERL" && spago build >/dev/null 2>&1 && make erl-quick >/dev/null 2>&1 \
  && erl -pa ebin -noshell -eval 'io:format("~s~n", ['"'"'reef_conformance@ps'"'"':run()]), halt().' 2>/dev/null ) \
  | grep -E "$DATA" > /tmp/reef-erl-out.txt

echo "== diff node vs erl =="
diff /tmp/reef-node-out.txt /tmp/reef-erl-out.txt && echo "  ✅ node == erl"
echo "== diff vs committed golden =="
diff "$GOLDEN" /tmp/reef-node-out.txt && echo "  ✅ matches golden"
echo "ALL GREEN — Odonus engine identical on JS + Erlang, matches golden."
