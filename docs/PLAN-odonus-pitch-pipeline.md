# Odonus pitch pipeline — the port spec (decision A, locked 2026-07-08)

Status: **spec** (task #129). Supersedes the Stage-A "cells index a PitchSet"
model landed in reef `4fbf8e1` / triggerfish `ea77214`. The primitives it stands
on (`quantiseEqual`, `quantiseNearest` over a `PitchSet`) are already published
and law-tested in **harmonia** (`8b4adef`); this spec composes them.

## Why the change

Stage A made **the cell a discrete index into the voice's PitchSet**, and let
Vetula *supply that PitchSet* (`setPitchSet`). That conflated two things AC wants
kept apart:

- the **melodic shape** — the 16 cells, "what note-ish thing each step wants",
  authored by the player and stable across chord changes;
- the **harmonic constraint** — the chord currently sounding, which should
  *colour* the melody at the very end, never *replace* it.

When Vetula fed the index source, a chord change rewrote the melody (and shrank
the knob range to the chord's cardinality → the knob-value-over-360° glitch). The
correction (AC, 2026-07-08): **cells always index the scale; Vetula constrains the
output.** The melody drives every playhead all the time; the chord only snaps the
result at the end.

## The pipeline (one voice, one cell)

```
knob 0–255            raw internal value, ALWAYS shown on the knob face
  │
  │  q1 = quantiseEqual(scale, span, 255)      position → a SCALE TONE   [decision A]
  ▼
home                  a real scale pitch at real register (the stable melodic shape)
  │
  │  label = quantiseNearest(activeSet, home)  the knob's LIVE LABEL (offset-free)
  │
  │  + per-voice offset   (±12, CHROMATIC)     fired into the quantiser
  ▼
  │  q2 = quantiseNearest(activeSet)           activeSet = Vetula chord if firing, ELSE scale
  ▼
snapped
  │  + global octave shift   (±12·k, all voices together)
  ▼
MIDI
```

`activeSet` is the one switch: **a firing Vetula chord, else the scale itself.**
In scale-only mode `activeSet == scale`, so q2 is the identity on `home` (a scale
tone is already a member — the *member-is-fixed-point* law), and the label equals
the home note. Vetula owns the key only while a chord fires; otherwise the scale's
own root is the key.

## Proposed API (pure, harmonia-backed)

```purescript
type VoiceContext =
  { scale     :: PitchSet   -- the key / stable melodic home (period 12)
  , activeSet :: PitchSet   -- Vetula chord if firing, else == scale
  , span      :: Int        -- how many periods the knob sweeps (register range)
  , knobMax   :: Int        -- raw knob ceiling (255)
  , globalOct :: Int        -- ±12·k semitones, applied to every voice
  }

type VoiceControl = { knob :: Int, offset :: Int }   -- offset ∈ [-12, 12]

home      :: VoiceContext -> Int -> Int              -- q1
home ctx knob = quantiseEqual ctx.scale ctx.span ctx.knobMax knob

voiceLabel :: VoiceContext -> Int -> Int             -- what the knob face shows
voiceLabel ctx knob = quantiseNearest ctx.activeSet (home ctx knob)

voiceNote  :: VoiceContext -> VoiceControl -> Int    -- the sounding MIDI
voiceNote ctx { knob, offset } =
  quantiseNearest ctx.activeSet (home ctx knob + offset) + ctx.globalOct
```

Four lines of composition. Everything below the knob is a `PitchSet` operation
that harmonia already proves.

## Register follows the melody — for free

The open question the earlier model couldn't answer ("which octave does the
constrained output land in?") is now settled by a law, not a special case.
`home` is a real pitch at real register (equal-mapping spans `span` periods).
`quantiseNearest` obeys **locality** — `2·|q n − n| ≤ period` — so the snapped
note is always within half an octave of `home + offset`. The chord recolours the
melody *in place*; it never yanks it to Vetula's own register. That is exactly
"register should follow the melody, not the voicing."

Pure vs extended `activeSet` (axis 1) is still available: a pure chord (period 12)
offers every chord tone in the melody's octave; an `extendedChord` offers
octave-dependent colour tones (a 9th only where it really sits). The pipeline
doesn't change — only the set handed in as `activeSet`.

## Axis 3 (voices sound the chord) — also for free

A ±12 chromatic offset, once snapped to `activeSet`, lands on a *neighbouring*
chord tone: `home = C`, `offset = +3` → `D#` → nearest Fmaj7 tone `E`. So a spread
of per-voice offsets covers the chord's tones with no special-casing — only the
exact ±12 endpoints re-land on the same pitch-class an octave away. AC's
suspicion that the "make the voices sound the chord" algorithm is "full of ad-hoc
rules" is answered: it falls out of nearest-snap over spread offsets.

## Dropped, deliberately

- **The internal McMullen chord quantiser.** `activeSet ∈ {scale, vetulaChord}`
  only; a one-chord Vetula progression covers the old case.
- **Scalar / diatonic transpose (`degShift`).** Only the global octave shift
  survives. Key transposition lives in Vetula.
- **Index-space per-voice `transp`.** Replaced by the ±12 chromatic `offset`
  fired into q2.
- **Cells-as-indices / `setPitchSet` feed.** Cells hold a raw knob value; the
  scale is the index source, always.

## Which harmonia laws underwrite each stage

| stage | rests on | law |
|---|---|---|
| `home` always a scale tone | `quantiseEqual` | *equal: always lands on a set member* |
| `home` monotone in knob | `quantiseEqual` | *equal: monotone (nondecreasing) in the input* |
| label == home in simple mode | `quantiseNearest` | *nearest: member is a fixed point* |
| `voiceNote` always a chord tone (+8ve) | `quantiseNearest` | *nearest: membership* |
| register follows the melody | `quantiseNearest` | *nearest: locality `2·|Δ|≤period`* |
| octave-equivariant colour | `quantiseNearest` | *nearest: `q(n+p)=q(n)+p`* |

The Odonus voice is correct *because* the primitives are. Its own tests assert the
composition (simple-mode identity, membership of the sounding note, register
locality, axis-3 spread) and reduce to the seven laws already green in
`Test.QuantiseSpec`.

## Implementation plan

1. **This session** — the pure pipeline + its tests, against the harmonia laws,
   with **no app / bundle / browser**. (Home TBD with AC: a new
   `Harmonia`-family module vs. `Reef.Odonus`; see the open question below.)
2. **Phase 2 (#132, with AC)** — reef's `renderCell` adopts the pipeline: cell
   becomes a raw knob, the chord-off path becomes `home`, the chord-on overlay
   becomes q2 over `activeSet`. This moves the Odonus conformance golden (index
   realize → equal-home-then-nearest); regenerate it and re-prove node↔BEAM. The
   wire codec at the `Reef.Protocol` boundary is the risky, conformance-guarded
   part — hence with-AC.
3. **Triggerfish UI** — the NOTE knob shows the raw 0–255 value; the label shows
   `voiceLabel` live (re-colouring as Vetula moves); the per-voice ±12 offset knob
   replaces the index-space transp; `degShift` / scale-authoring-as-index UI
   retire.

## Still open

- **`span` / `knobMax` defaults** — how wide a register one knob sweeps (Stage A
  used `span = 3`). Pick, then freeze into the demo tables.
- **Pure vs extended `activeSet`** — whether the live Vetula follow hands a pure
  (period 12) or extended chord. Default pure; extended is a per-progression
  choice once Vetula can emit it.
