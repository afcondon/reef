# Plan — Lockstep Co-Simulation (identical UI, standalone or on-rig)

## The Product requirement (AC, PM-hat, 2026-06-30)

- **Standalone** (the webapp demo): the frontend does *everything* — animates the
  playheads, runs all controls and generation, and emits the notes (Web-MIDI).
- **On Atlantis (the rig)**: the UI **looks and behaves identically**, but the
  generation and the MIDI output all happen on the **backend** (the BEAM).
- The live note display on the LHS (added as decoration / for audience
  projection) must stay **alive in both modes** — it turns out to be great for
  playing, not just watching.

## The chosen approach: deterministic **lockstep** (sync inputs, not state)

Two canonical patterns exist for "the same thing running in two places":

1. **Shared state** — one authority computes, the other renders a streamed copy.
   The UI becomes a *remote echo*: liveliness hostage to stream bandwidth/latency,
   local snappiness lost unless you re-add prediction anyway.
2. **Shared input (lockstep)** — both sides run the *same deterministic
   simulation* and exchange only the **inputs** (sparse user actions), never the
   state. This is how networked RTS games sync thousands of "random" units.

**Randomisation is NOT a blocker for lockstep — it is the textbook case for it.**
Marbles is a deterministic seeded PRNG; `reroll(seed, params)` is pure. Share the
**seed, not the results**: both sides, from the same seed, applying the same ops
in the same order at the same ticks, draw the same "random" numbers and stay
bit-identical.

### Why this is uniquely available to us

Lockstep's hard precondition — *the exact same deterministic simulation on both
sides* — is already met:

- **One engine, two runtimes.** `reef`'s `stepEmit` compiled to JS *and* Erlang,
  proven **byte-identical** node ↔ BEAM (the conformance goldens). That
  byte-identicality *is* the determinism lockstep needs.
- **A shared clock already exists.** Binnacle subscribes to the Link anchor, so
  frontend and rig already agree on "tick N".
- **A wire already exists.** The WS + `Reef.Protocol`; inputs just need a
  tick-tag ("apply at tick N+k") so both sides apply each input on the same tick.

So the UI is identical on-rig **because in both modes it is running the same
simulation** — not echoing a remote one. The rig holds authority over **MIDI out
and "truth"**; the frontend is a perfectly-synced co-simulation that, on-rig,
simply doesn't emit notes.

## Model

| | Standalone | Rig-attached |
|---|---|---|
| Who simulates | frontend | **both** (co-sim) |
| MIDI authority | frontend (Web-MIDI) | **rig** (link-spike → IAC) |
| Frontend role | everything | scope + controller (sims locally, mutes MIDI) |
| Sync | n/a | shared seed + tick-tagged inputs (+ periodic resync) |

- Both runtimes run the **whole module** (engine + chord clock + Marbles gen)
  from a **shared seed**.
- User actions become **tick-tagged inputs** (`apply at currentTick + k`, a small
  lookahead buffer so both sides receive them before that tick) and are broadcast.
  Both sides apply at exactly that tick → identical evolution.
- The rig emits MIDI. The frontend renders its own identical sim (no/ muted MIDI
  when rig-attached).
- **Handoff** (standalone → rig): the seed + current state travel once, so the rig
  picks up exactly where the frontend was.

## The hard requirement: provable determinism

- Integer ops are already byte-identical across runtimes (goldens prove it).
- **Risk: floats.** Odonus's `Head.accumulator :: Number` drives phase; JS and
  Erlang float arithmetic *can* drift differently over long runs.
- **Mitigations (do both):**
  1. **Make the phase state integer/rational** (accumulator as a fraction, not a
     float) → provably deterministic, no drift ever.
  2. **Belt-and-braces: periodic authoritative resync.** The rig snapshots full
     state every few seconds; the frontend reconciles. (Client-prediction /
     server-reconciliation — standard netcode.)

## Work breakdown

1. **Move generation into reef.** `Gen` + `Marbles` (both pure PS, no FFI) move
   from `Triggerfish/Odonus/` into reef — the last un-shared piece of the module.
   Now the *whole* module (engine + gen) runs on both runtimes. Triggerfish imports
   them from reef.
2. **Make the engine provably deterministic over long runs.** Replace the float
   accumulator with integer/rational phase; audit for any other nondeterminism
   (iteration order, float compares). Extend the cross-runtime conformance golden
   from 32 steps to **thousands**, *with generation active*, and prove byte-identical
   node ↔ BEAM. This golden is the lockstep safety net.
3. **Input/delta protocol.** `Reef.Protocol` gains an `Input` ADT (the setters +
   gen events: `SetNote i v`, `SetRoot pc`, `SetPitchSet ps`, `ArmGen src`, …)
   carrying a **tick-tag**; codec compiled to both runtimes.
4. **Tick alignment.** Frontend derives the rig's tick number from the Binnacle
   Link anchor; inputs scheduled at `currentTick + buffer`.
5. **`reef_voice` as co-sim authority.** Holds the canonical sim; applies
   tick-tagged inputs at their tick; emits MIDI; periodically pushes a resync
   snapshot up.
6. **Triggerfish lockstep client.** Runs the same sim locally; applies the same
   inputs at the same ticks; renders the LHS scope; mutes MIDI when rig-attached;
   accepts resync snapshots.
7. **Mode switch + handoff.** Standalone ↔ rig-attached; seed/state transfer on
   handoff; frontend mutes Web-MIDI when the rig has authority.

## Phasing (each phase testable on its own)

- **P1 — Generation into reef + long-run determinism golden.** Move Gen+Marbles;
  prove the full module (with gen) byte-identical cross-runtime over thousands of
  steps. *This is the foundation; everything else rides on it.*
- **P2 — Deterministic phase.** Integer/rational accumulator; long-run identical
  including phasing.
- **P3 — Input protocol + tick alignment.** `Input` ADT with tick-tags; frontend
  reads tick from the Link anchor.
- **P4 — Co-sim live.** `reef_voice` applies tick-tagged inputs + emits MIDI;
  Triggerfish runs the lockstep client. Verify: a knob turn lands identically on
  both at the same tick (recorded MIDI == frontend's visualised note).
- **P5 — Resync.** Periodic authoritative snapshot; frontend reconciles (defeats
  any residual drift).
- **P6 — Mode switch + handoff.** Standalone ↔ rig-attached; seed transfer; MIDI
  authority handover.

## Open questions / risks

- **Float determinism** is the central risk; P2 (integer phase) + P5 (resync)
  together should make it a non-issue. Decide how hard to lean on each.
- **Input latency buffer (k)** — musical feel vs safety margin. Bigger k = safer
  sync but more lag before an edit takes effect. Tune against the lookahead window.
- **Disconnect behaviour** — both sides keep simulating independently; on
  reconnect, the rig's resync snapshot reconverges the frontend. (Music keeps
  playing on the rig regardless — the autonomy AC wants.)
- **The Vetula fork** (deferred): is harmonic generation another autonomous module
  on the BEAM (cross-voice coordination returns) or a frontend conductor pushing
  `SetPitchSet` inputs down? Lockstep makes *either* workable — a Vetula co-sim is
  just more shared deterministic module; a conductor is just more inputs.

## Why this is the right shape

It gives the PM dream directly: **byte-identical UI behaviour standalone and
on-rig, because it is the same code running in both places** — with the rig
authoritative for output. The LHS note display stays live in both modes for free,
because it *is* the co-simulation. And it is only possible because of what we
already built: harmonia → reef → one engine, two runtimes, proven identical.
