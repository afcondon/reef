# Plan — Vetula as Palette, Tidal/Odonus as Brush

## Framing (AC, PM-hat, 2026-07-03)

Vetula is an **artist's palette** — a board for mixing colours. Raw-Tidal and
Odonus are the **brush**. The palette produces the *harmonic material* (voiced
pitch-sets over time); the brush decides *when and how* those pitches are struck.

This resolves two problems that turned out to share one root:

1. **Workflow is clunky** — no persistence (everything rebuilt from scratch each
   session), can't re-voice a running progression, too many stages from lab to
   playing (no "play this now").
2. **Vetula contains its own sequencer** (`Reef.Vetula.Perf`) — works very well
   but is conceptually a mistake: every primitive in it already exists one layer
   down in Tidal, better.

Both are downstream of the same conflation: Vetula is doing **two jobs** and only
one is Vetula's.

- **Harmony** — the progression, the voicings, the voice-leading
  (`enumerateVoicings` / `voiceLead` in harmonia). *The irreplaceable part.*
- **Sequencing** — bars-per-chord dwell, skips, block/arp/strum, phase, per-voice
  rhythm. *This is the embedded sequencer, and it's Tidal's job.*

The fix: **Vetula stops being an instrument-with-a-sequencer and becomes a
harmonic pattern-source.** Sequencing goes to the brush; voice-leading stays in
the palette. Then persistence, re-voicing, "play now", and the Odonus-quantise
path all fall out for free, because they're already solved for Tidal cells.

## Guiding lights (AC)

1. Make **auditioning/sampling** the vast harmonic space tractable — *mostly done*.
2. Make **creation** of chord progressions easy — *done*.
3. Make **re-voicing** of chosen chords easy — *done in some cases, not others*.
4. Allow **music-theory-informed processing** of these sequences — *partial (see
   the strum reclassification below)*.
5. Allow the **full power of Tidal's pattern language** to work on these sequences
   of pitch-sets — *the new frontier*.

The two "not done" lights (#3 re-voicing, #5 Tidal power) are exactly what this
move enables.

## Settled technical decisions

- **Browser = the palette.** Harmonic authoring + *chord/legato preview* via local
  Web MIDI, for the ear while composing. It is a **compositional audition**, not
  performance parity — so it does **not** need to be lockstep-accurate, and can be
  a simple browser-only renderer.
- **Backend = the brush.** The real Tidal engine (purerl-tidal, on the BEAM)
  applies rhythm/pattern and emits the performance MIDI.
- **Keep Tidal out of the browser** — even though Strudel proves it's possible.
  Sync would be a constant fight against GC. Instead, **animate the frontend from
  backend events** (as Odonus already does); any jitter/latency there is
  imperceptible, as we've seen.
- **Live-playback animation in the browser is trivial at most** — step-through
  highlight driven by backend events. We are not re-animating the sounding music
  in the front end. (Stepping through an arp ≈ Odonus in terms of visual sync, so
  even that is a solved shape if wanted.)

### What this retires, what survives

The lockstep work made `Reef.Vetula.Perf` a **shared performance renderer** (block/
arp/strum running byte-identical on both runtimes so browser == rig). That is now
**retired as a performance path**: the rig plays Tidal, the browser animates from
events.

- **Survives:** the WS/event plumbing, the `→ RIG` push path, the *insight*
  (push-once, pure-function-of-pulse), the `set_perf` live-swap.
- **No longer needed:** block/arp/strum duplicated on the BEAM as a performance
  renderer.
- **Relocates:** the strum/legato logic (see below) — the *code was right, the
  taxonomy was wrong*.

This is a net simplification: one shared thing to reason about becomes zero (the
rig owns performance; the browser owns audition).

## The interface: `Pattern PitchSet`, with two readings

The palette/brush wire is a **time-varying pitch-set** — a sequence of voiced
pitch-sets with dwell/skip baked into its time structure. A voice-led progression
has **two readings**, and Vetula (being a voice-leading tool) owns both:

- **Vertical / chordal** — `Pattern PitchSet`, a sequence of sonorities. The brush
  samples / arps / euclids this.
- **Horizontal / contrapuntal** — N tied voice-lines: each voice run-length-encoded
  across the segments it survives. This is the **legato** reading (see strum below).

One interface, two brushes, identical contract:

- **Tidal brush** samples notes from the currently-active pitch-set and patterns
  them (`arp`, `struct`, `euclid`, `off`, `every`, …).
- **Odonus brush** uses the active pitch-set as its `realize :: Index -> Pitch`
  source — already the plan (Vetula = harmonic authority, honour-the-source). The
  old bespoke `FollowChord` conductor was just a hand-rolled "Odonus samples the
  palette"; it disappears.

## The taxonomy that unblocked this — roll vs arp vs strum

These sit on **different axes**. Conflating them was the confusion.

| operation | axis | what it does | home | shape |
|---|---|---|---|---|
| **roll** | vertical, intra-chord | spread *one* chord's notes' onsets over a small window, sustains ring on | **brush** (Tidal) | local, stateless pattern transform |
| **arp** | vertical, intra-chord | spread *one* chord's notes across the event, gated per-note | **brush** (Tidal) | local, stateless pattern transform |
| **strum** *(AC's sense)* | **horizontal, inter-chord** | fold over the *sequence*; per boundary **re-attack / release / hold** | **palette** (Vetula) | fold → tied `Pattern Note` |

### roll and arp — the brush verbs

Both take one sonority and spread its notes in time. Stateless, local, operate on a
single event. Tidal has `arp` (rich, patternable) and `rolled`/`rolledBy` (a
hardcoded ascending corner). **roll is a genuinely useful brush verb and we should
make it available** — direction/order, spread window, sustain policy — ideally as a
cleaner combinator than upstream's `rolled`. (A possible upstream contribution to
Alex's Tidal later: `arp` and `rolled` as presets of one `spread` combinator over
(window, sustain, order). Park as a proven-shaped side-quest; not on the spine.)

### strum — actually the audible form of voice-leading

AC's "strum" is **not** an intra-chord operation. It is a **stateful fold over the
sequence of chords**: at each boundary, *terminate any playing notes not in the new
set, start any notes not in the old set, and hold the common tones.* A common tone
across chords is **one sustained note, not two attacks**.

That is **voice-leading made audible** — the whole reason harmonia exists,
expressed as sound: smooth-motion voicings only *sound* smooth if common tones ring
through and only the moving voices re-strike. It is the **horizontal / contrapuntal
reading** of the progression: run-length-encode each voice, suppress re-attacks on
held pitches, release on departure.

**Why it belongs in the palette, not Tidal:**

1. It *is* voice-leading — the palette's core competency.
2. It's a **fold over the whole progression** (unbounded run-length tie across
   segments). The palette owns the whole progression; the brush wants to stay a
   *local, stateless* transform.
3. **Tidal's model resists it.** Patterns are locally queried — `query (arc t0 t1)`
   is meant to be computable from that arc. Deciding "attack or continuation, and
   how long does it ring?" needs unbounded neighbour-walking. You *can* force it,
   but it's not a natural combinator — it's a **compile step over the whole
   progression that emits already-tied `Pattern Note`.** So it doesn't want to be a
   Tidal verb; it wants to be how the palette emits its horizontal reading.

The old `Perf` strum branch already did exactly this (`entering = notes not in
prevNotes`; forward-sustain walk). Keep the logic; **relocate its conceptual home**
to the palette's legato emission.

**Naming.** Now that it's cut away from the roll, "strum" is arguably the wrong word
(strum *is* a roll). What AC means is closer to **legato / tie / hold-through** —
theory term *common-tone tying*, verb-gesture "hold through the changes." Rename so
the word "strum" is free to mean the roll if wanted.

### Palette articulation modes

The palette emits (at least) two modes, both palette output; the brush consumes
either:

- **detached / re-strike** → `Pattern PitchSet` (every segment re-attacks) — feeds
  the brush to sample.
- **legato / voice-continuous** (AC's strum) → N tied voice-lines — the horizontal
  reading.

**Legato preview is the most diagnostic audition we have** — it is how you actually
*hear whether the voice-leading is good*. AC's "instant preview of chords, maybe
strums" instinct was pointing exactly here.

## Graphical affordances for Tidal primitives

The way to give the palette #5 (full Tidal power) **without** breaking the
compositional-not-theory-exposing ethos: **Tidal's power through gesture, not
syntax.** The GUI builds a **typed transform stack** — a closed ADT of the Tidal
ops Vetula chooses to expose graphically — and that stack **compiles to Tidal code**
sent to the rig. The browser never interprets it (it only previews chords/legato);
the ADT *is* the interchange. The pattern is a **hidden compiled export**, not an
editing surface.

Starter affordance set (Hainbach × Rams; dogfoods hylograph-halogen-ui):

- **euclid / struct** — a step-grid or (k, n, rotation) dial. Balistes territory;
  the widget vocabulary already exists.
- **arp** — direction selector, or *draw the order on the chord disc*.
- **roll** — a single spread knob (+ direction, + sustain policy).
- **off** — echo offset + transpose.
- **every / whenmod** — an "every N do ⟨transform⟩" builder that nests the stack.
- **dwell / skip** — already visual today.

The stack is **append-only** (new affordances = new constructors) — matches the
recompile-test rule (no user recompile to extend) and the pappardelle append-only
ethos.

## Re-voicing (#3) closes for free

Make re-voicing a **first-class palette gesture on any chord in the sequence**
(running or not): select a chord → re-run `enumerateVoicings` → pick a new voicing.
Because the output is *material pushed once*, re-voicing a **running** progression =
re-emit + hot-swap, which the `set_perf` live-swap already supports and purerl-tidal
already does per-cell. The partial→full gap in #3 closes as a consequence of the
pattern-source model.

## Persistence (workflow #1) closes for free

A saved progression becomes **text** (the emitted pattern + voicing params + the
transform stack), and it is the *same artifact the rig plays*. Can live in
`sketches/`. "One push is the whole wire" becomes "the file *is* the wire." Saving,
loading, naming, sharing — all trivial once the output is a pattern.

## First slice (smallest thing that proves the shape)

Prove the **palette → brush contract end-to-end** before building any affordances:

1. Define the `Pattern PitchSet` interface (time-varying voiced pitch-set, with
   dwell/skip).
2. Get the **Tidal brush** playing a **hand-written** pattern that samples a pushed
   Vetula progression on the rig — no affordances yet, pattern typed by hand.
3. Confirm the same progression drives the **Odonus brush** via `realize` (the
   FollowChord special-case gone).

That validates the contract. Affordances (the transform stack) are the layer on
top, once the contract holds. The **legato/strum** relocation and the **roll** brush
verb are parallel, self-contained pieces.

## Open questions to settle while building

- **Sustain policy vocabulary** for the roll/arp brush: a small closed ADT —
  `slice | holdToEnd | fixed Dur | untilNextOnset`. That ADT is the real design
  decision.
- **Naming** of strum → legato / tie / hold-through.
- **The arp/strum(legato) seam in preview:** keep a light articulation sense in the
  palette so preview *sounds* right for the ear, while the brush owns the real
  rhythmic-time version for performance. Not in conflict; decide per-case.
- **Does the browser preview need any rhythm at all**, or only chord + legato? (AC's
  current read: chord + maybe arp/strum; live-playback rhythm is a backend concern.)
