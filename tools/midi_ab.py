#!/usr/bin/env python3
"""
midi_ab.py — a lockstep A/B oracle for the reef co-simulation.

Passively taps the IAC MIDI bus (exactly as Ableton would) and hears BOTH the
frontend (Triggerfish, heads on ch 1-4) and the backend (reef_voice, heads on
ch 12-15). It reconstructs each source's per-head note stream, ALIGNS the two by
pitch sequence — so a phase offset or a single dropped note doesn't throw the
whole comparison off — and reports precisely where they diverge:

  * pitch mismatch      -> the generative MODEL diverged (the serious one)
  * missing note        -> one source dropped a note (e.g. a crash, or a skip
                           applied on only one side)
  * timing skew / drift -> scheduling / clock (a CONSTANT skew is just a phase
                           offset from the handoff; a GROWING one is real drift)
  * duration / count    -> the render layer (gate length, ratchet retriggers)

Velocity is reported for information only — humanise is deliberately frontend-
local, so velocities are expected to differ and never count as a divergence.

Usage:
  midi_ab.py                       # capture until Ctrl-C, then report
  midi_ab.py --seconds 20          # capture a fixed window, then report
  midi_ab.py --port "IAC Driver Tidal"
  midi_ab.py --live                # also print divergences as they stream

Reproducible run: click "⚑ Pin seed" in the GENERATE pane, Push to Rig, start
this, let it play a bar or two, Ctrl-C. Same seed + same edits => same verdict.
"""

import argparse
import signal
import statistics
import sys
import time

import mido

DEFAULT_PORT = "IAC Driver Tidal"
# A source is identified by the MIDI-channel band its heads land on. The head
# index is the channel's offset from the lowest channel seen in that band, so the
# 0-/1-indexed-channel ambiguity between runtimes doesn't matter.
FE_BAND = range(0, 4)     # frontend heads I-IV  -> MIDI ch 1-4  (0-indexed 0-3)
BE_BAND = range(11, 15)   # backend  heads I-IV  -> MIDI ch 12-15 (0-indexed 11-14)

MATCH_SHIFT = 8           # try aligning the two streams within ±this many notes
TIMING_JITTER_MS = 15.0   # per-note deviation from the median offset above which
                          # we call it drift rather than a constant phase offset


def band_of(channel):
    if channel in FE_BAND:
        return "FE"
    if channel in BE_BAND:
        return "BE"
    return None


def reconstruct(events, end_t):
    """Pair note-ons with note-offs into notes, keyed by (band, head)."""
    open_notes = {}   # (channel, note) -> [ (t_on, vel), ... ] FIFO (for ratchets)
    streams = {}      # (band, head)    -> [ note dicts ]
    for t, msg in events:
        if msg.type not in ("note_on", "note_off"):
            continue
        band = band_of(msg.channel)
        if band is None:
            continue
        key = (msg.channel, msg.note)
        is_on = msg.type == "note_on" and msg.velocity > 0
        if is_on:
            open_notes.setdefault(key, []).append((t, msg.velocity))
        else:
            q = open_notes.get(key)
            if q:
                t_on, vel = q.pop(0)
                base = min(FE_BAND) if band == "FE" else min(BE_BAND)
                head = msg.channel - base
                streams.setdefault((band, head), []).append(
                    {"t": t_on, "note": msg.note, "vel": vel, "dur": t - t_on})
    # Any still-held notes at capture end: close them at end_t so they count.
    for (channel, note), q in open_notes.items():
        band = band_of(channel)
        if band is None:
            continue
        base = min(FE_BAND) if band == "FE" else min(BE_BAND)
        head = channel - base
        for t_on, vel in q:
            streams.setdefault((band, head), []).append(
                {"t": t_on, "note": note, "vel": vel, "dur": end_t - t_on})
    for s in streams.values():
        s.sort(key=lambda n: n["t"])
    return streams


def best_shift(fe, be):
    """Index shift d (fe[i] ~ be[i+d]) maximising pitch matches; ties -> |d| min."""
    best_d, best_score = 0, -1
    for d in range(-MATCH_SHIFT, MATCH_SHIFT + 1):
        score = overlap = 0
        for i in range(len(fe)):
            j = i + d
            if 0 <= j < len(be):
                overlap += 1
                if fe[i]["note"] == be[j]["note"]:
                    score += 1
        # Prefer more matches; on a tie, prefer the smaller shift (and any overlap).
        if overlap and (score > best_score or (score == best_score and abs(d) < abs(best_d))):
            best_d, best_score = d, score
    return best_d


def compare_head(fe, be):
    """Align one head's two streams and summarise the differences."""
    d = best_shift(fe, be)
    pairs = [(fe[i], be[i + d]) for i in range(len(fe)) if 0 <= i + d < len(be)]
    pitch_mismatch = [(f, b) for f, b in pairs if f["note"] != b["note"]]
    deltas = [(b["t"] - f["t"]) * 1000.0 for f, b in pairs]          # ms, be - fe
    dur_ratio = [b["dur"] / f["dur"] for f, b in pairs if f["dur"] > 1e-6]
    offset = statistics.median(deltas) if deltas else 0.0
    jitter = max((abs(x - offset) for x in deltas), default=0.0)
    unpaired_fe = len(fe) - len(pairs)
    unpaired_be = len(be) - len(pairs)
    return {
        "shift": d, "pairs": len(pairs),
        "pitch_ok": len(pairs) - len(pitch_mismatch), "pitch_bad": pitch_mismatch,
        "offset_ms": offset, "jitter_ms": jitter,
        "dur_ratio": statistics.median(dur_ratio) if dur_ratio else float("nan"),
        "unpaired_fe": unpaired_fe, "unpaired_be": unpaired_be,
        "fe": len(fe), "be": len(be),
    }


def report(streams):
    heads = sorted({h for (_band, h) in streams})
    print("\n" + "=" * 72)
    print("  reef lockstep A/B report")
    print("=" * 72)
    if not heads:
        print("  No notes captured on the FE (ch 1-4) or BE (ch 12-15) bands.")
        return
    verdict_bad = []
    for h in heads:
        fe = streams.get(("FE", h), [])
        be = streams.get(("BE", h), [])
        print(f"\n  head {h}   FE notes={len(fe):<4} BE notes={len(be):<4}")
        if not fe or not be:
            only = "FE (ch 1-4)" if fe else "BE (ch 12-15)"
            print(f"    ONLY {only} present — the other source is silent for this head.")
            verdict_bad.append(f"head {h}: one source silent")
            continue
        c = compare_head(fe, be)
        print(f"    aligned pairs={c['pairs']}  shift={c['shift']:+d} notes"
              f"   pitch {c['pitch_ok']}/{c['pairs']} match")
        print(f"    timing: offset={c['offset_ms']:+.1f}ms  jitter=±{c['jitter_ms']:.1f}ms"
              f"   dur(be/fe)×{c['dur_ratio']:.2f}")
        if c["unpaired_fe"] or c["unpaired_be"]:
            print(f"    UNPAIRED: {c['unpaired_fe']} FE-only, {c['unpaired_be']} BE-only notes")
        if c["pitch_bad"]:
            verdict_bad.append(f"head {h}: {len(c['pitch_bad'])} pitch mismatch")
            for f, b in c["pitch_bad"][:6]:
                print(f"      PITCH  FE {f['note']:>3} vs BE {b['note']:>3}  @ t={f['t']:.2f}")
        if c["jitter_ms"] > TIMING_JITTER_MS:
            verdict_bad.append(f"head {h}: timing jitter ±{c['jitter_ms']:.0f}ms (drift?)")
        if c["unpaired_fe"] or c["unpaired_be"]:
            verdict_bad.append(f"head {h}: {c['unpaired_fe']+c['unpaired_be']} unpaired (dropped notes)")
        if c["dur_ratio"] == c["dur_ratio"] and not (0.85 <= c["dur_ratio"] <= 1.18):
            verdict_bad.append(f"head {h}: note lengths differ ×{c['dur_ratio']:.2f} (render)")

    print("\n" + "-" * 72)
    if verdict_bad:
        print("  VERDICT: DIVERGENCE")
        for v in verdict_bad:
            print(f"    - {v}")
    else:
        print("  VERDICT: IN SYNC  (pitches match, timing within jitter, no dropped notes)")
        print("  (velocity differences are expected — humanise is frontend-local.)")
    print("-" * 72)


RIG_WS = "ws://127.0.0.1:3012/ws"
STEP_BEATS = 0.25


def fetch_anchor_rows():
    """Pull the beam's anchor receipt log (NowUs, Beat, Tempo) — the mapping
    reef_voice itself uses to place notes in wall-clock time. NowUs is unix micros
    (erlang:system_time(microsecond)), the same clock as Python time.time()."""
    import websocket
    ws = websocket.create_connection(RIG_WS, timeout=5)
    ws.send("dump-anchor-log")
    chunks = []
    try:
        while True:
            chunks.append(str(ws.recv()))
            if len(chunks) > 4:
                break
    except Exception:
        pass
    ws.close()
    rows = []
    for line in "\n".join(chunks).splitlines():
        if "anchor_rx" not in line:
            continue
        try:
            _seq, nowus, ev = line.split(",", 2)
            inside = ev[ev.index("{") + 1:].strip("{} ").split(",")
            rows.append((int(nowus), float(inside[1]), float(inside[2])))  # nowus, beat, tempo
        except Exception:
            continue
    rows.sort()
    return rows


def _nearest(rows, t_unix):
    return min(rows, key=lambda r: abs(r[0] - t_unix))


def beat_at(rows, t_unix):
    nowus, beat, tempo = _nearest(rows, t_unix)
    return beat + (t_unix - nowus) * tempo / 60_000_000.0


def intended_unix(rows, near_unix, step):
    """The unix time at which Link beat == step*STEP_BEATS — i.e. reef_voice's WallUs."""
    nowus, beat, tempo = _nearest(rows, near_unix)
    return nowus + (step * STEP_BEATS - beat) * 60_000_000.0 / tempo


def clock_probe(seconds):
    """Decompose the FE/BE offset: measure each note's arrival vs the Link-anchor-
    intended onset, separately for the backend (its own clock -> MIDI-path accuracy)
    and the frontend (vs the same shared clock -> its total phase error)."""
    import statistics
    t_perf0, t_unix0 = time.perf_counter(), time.time()
    events = []
    port = mido.open_input(DEFAULT_PORT, callback=lambda m: events.append((time.perf_counter(), m)))
    print(f"Clock probe: capturing {seconds}s on '{DEFAULT_PORT}' + reading the beam anchor log. Play now...")
    time.sleep(seconds)
    port.close()
    rows = fetch_anchor_rows()
    if not rows:
        print("  Couldn't read the beam anchor log (is a Link anchor flowing?)."); return
    # Convert tap (monotonic) timestamps to unix MICROSECONDS (the beam's NowUs
    # units), then reconstruct notes so all times share one clock + scale.
    to_us = lambda p: (t_unix0 + (p - t_perf0)) * 1_000_000.0
    uevents = [(to_us(p), m) for (p, m) in events]
    streams = reconstruct(uevents, to_us(time.perf_counter()))
    be_resid, fe_resid = [], []
    for h in sorted({hd for (_b, hd) in streams}):
        fe, be = streams.get(("FE", h), []), streams.get(("BE", h), [])
        if not fe or not be:
            continue
        d = best_shift(fe, be)
        for i in range(len(fe)):
            j = i + d
            if not (0 <= j < len(be)) or fe[i]["note"] != be[j]["note"]:
                continue
            step = round(beat_at(rows, be[j]["t"]) / STEP_BEATS)   # step from the on-time backend note
            intended = intended_unix(rows, be[j]["t"], step)       # unix micros
            be_resid.append((be[j]["t"] - intended) / 1000.0)      # micros -> ms
            fe_resid.append((fe[i]["t"] - intended) / 1000.0)

    def stat(xs):
        return (statistics.median(xs), statistics.pstdev(xs)) if xs else (float("nan"), float("nan"))

    bm, bs = stat(be_resid)
    fm, fs = stat(fe_resid)
    beat_ms = 60000.0 / rows[-1][2] if rows else 500.0
    # A whole-beat part of the frontend offset is a clock RE-LOCK phase error (#53);
    # the sub-beat remainder is the MIDI-scheduling latency (the now()+delay bug).
    fe_beats = round(fm / beat_ms)
    fe_latency = fm - fe_beats * beat_ms
    print("\n" + "=" * 72)
    print("  clock-phase decomposition   (arrival − Link-anchor-intended onset)")
    print("=" * 72)
    print(f"  paired notes: {len(be_resid)}   (1 beat = {beat_ms:.0f} ms)")
    print(f"  BACKEND (ch12-15): {bm:+7.1f} ms  (±{bs:.1f})   <- reef_voice vs its own anchor clock")
    print(f"  FRONTEND (ch1-4):  {fm:+7.1f} ms  (±{fs:.1f})   <- Triggerfish vs the same shared clock")
    print("-" * 72)
    print(f"  FE−BE gap: {fm - bm:+.1f} ms  =  clock re-lock {fe_beats:+d} beat "
          f"({fe_beats * beat_ms:+.0f} ms, #53)  +  MIDI latency {fe_latency:+.1f} ms (#56)")
    if abs(bm) < 12 and bs < 12:
        print("  -> backend rides its clock tightly; the offset is essentially ALL frontend-side")
        print("     (frontend clock phase and/or Web-MIDI latency).")
    elif bs >= 12:
        print(f"  -> backend jitter ±{bs:.0f} ms — MIDI path not honouring absolute WallUs (poll leakage?).")
    if abs(bm) >= 12:
        print(f"  -> backend also sits {bm:+.0f} ms off its own intended time (path latency).")
    print("=" * 72)


def discover(seconds):
    """Tap EVERY input port and report which port+channel carries note traffic —
    use this to find where the frontend and backend MIDI actually land."""
    names = mido.get_input_names()
    seen = {}  # (port, channel) -> [count, first_note]
    ports = []

    def make_cb(name):
        def cb(msg):
            if msg.type == "note_on" and msg.velocity > 0:
                k = (name, msg.channel)
                if k not in seen:
                    seen[k] = [0, msg.note]
                    print(f"  + {name:<24} ch{msg.channel+1:<2} (first note {msg.note})")
                seen[k][0] += 1
        return cb

    for name in names:
        try:
            ports.append(mido.open_input(name, callback=make_cb(name)))
        except Exception as e:
            print(f"  (couldn't open {name}: {e})")
    print(f"Discovery: listening on {len(ports)} input port(s) for {seconds}s. Play now...\n")
    time.sleep(seconds)
    for p in ports:
        p.close()
    print("\n  --- port/channel activity ---")
    if not seen:
        print("  NOTHING received on any input port. Is anything actually playing?")
    for (name, ch), (count, first) in sorted(seen.items()):
        print(f"    {name:<24} ch{ch+1:<3} notes={count}")


def main():
    ap = argparse.ArgumentParser(description="reef lockstep A/B MIDI oracle")
    ap.add_argument("--port", default=DEFAULT_PORT, help="MIDI input port to tap")
    ap.add_argument("--seconds", type=float, default=None,
                    help="capture this many seconds then report (default: until Ctrl-C)")
    ap.add_argument("--live", action="store_true",
                    help="also print pitch mismatches / drops as they stream")
    ap.add_argument("--discover", action="store_true",
                    help="tap ALL input ports to find where FE/BE MIDI lands")
    ap.add_argument("--clock-probe", action="store_true",
                    help="decompose the FE/BE offset vs the beam's Link anchor clock")
    args = ap.parse_args()

    if args.discover:
        discover(args.seconds or 20.0)
        return
    if args.clock_probe:
        clock_probe(args.seconds or 20.0)
        return

    names = mido.get_input_names()
    if args.port not in names:
        print(f"MIDI input '{args.port}' not found. Available:\n  " + "\n  ".join(names))
        sys.exit(1)

    events = []
    live_open = {}   # (ch,note)->pitch, for --live pairing across bands per head

    def on_msg(msg):
        t = time.perf_counter()
        events.append((t, msg))
        if args.live and msg.type == "note_on" and msg.velocity > 0:
            band = band_of(msg.channel)
            if band:
                base = min(FE_BAND) if band == "FE" else min(BE_BAND)
                print(f"  {band} head{msg.channel-base}  note {msg.note:>3} vel {msg.velocity:>3}")

    port = mido.open_input(args.port, callback=on_msg)
    print(f"Tapping '{args.port}'  (FE=ch1-4, BE=ch12-15).  "
          + (f"Capturing {args.seconds}s..." if args.seconds else "Ctrl-C to stop & report."))

    stop = {"now": False}
    signal.signal(signal.SIGINT, lambda *_: stop.__setitem__("now", True))
    t0 = time.perf_counter()
    try:
        while not stop["now"]:
            if args.seconds and time.perf_counter() - t0 >= args.seconds:
                break
            time.sleep(0.05)
    finally:
        port.close()
    report(reconstruct(events, time.perf_counter()))


if __name__ == "__main__":
    main()
