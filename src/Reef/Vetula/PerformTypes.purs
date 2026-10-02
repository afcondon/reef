-- | `Reef.Vetula.PerformTypes` — the pure value types of the Perform surface, shared
-- | by the live component (`Vetula.App`) and the document serialiser
-- | (`Reef.Vetula.Lepidoptera`). Extracted from `App` so the serialiser can depend on
-- | these without importing the 6k-line Halogen module — and so `App` can import
-- | the serialiser (for a save button) without a cycle.
-- |
-- | Everything here is pure (Prelude + `Data.Array.reverse` only): the layer/fx
-- | algebra, the terminal sink, and the canonical ASCII vocabulary the document
-- | round-trip prints and parses (NOT the glyph labels, which are lossy display).
-- |
-- | Time verbs are `Slow`/`Fast` (Tidal's own names), NOT a signed `rate`: real
-- | Tidal `rate` means sample-playback speed, a name collision we refuse. `slow N`
-- | is the most-wanted verb — harmonic progressions usually want to unfold over
-- | many bars (conditioning Odonus, ambient arpeggios), and `slow 64` composes
-- | cleanly on the per-cycle scheduler (see `scheduleBox`). No cap on N.
-- |
-- | Moved from Triggerfish (`Vetula.Perform.Types`) on 2026-10-02 so the rig can read Vetula's
-- | cards (docs/kb/plans/gpl-boundary-review.md, step 3); Triggerfish re-exports it.
module Reef.Vetula.PerformTypes where

import Prelude

import Data.Array (reverse)
import Data.Maybe (Maybe(..))
import Reef.PatternArg (PatternArg)
import Reef.Clips (MidiClip)

-- | A function-stack LAYER on a Perform box — a uniform `Pattern (Array Int) ->
-- | Pattern (Array Int)` endomorphism (see `applyFx`), so any layer drags anywhere
-- | in the stack or between boxes. Two families under one type: pitch-shapers that
-- | `map` over each chord (Transpose · Octave) and Tidal combinators polymorphic in
-- | the value (Slow/Fast = `slow`/`fast`). Voice/Select (Harmonia) + more land next;
-- | arp/strum are the terminal REALISATION, not layers (they explode chord→time).
data PerfFx
  = Transpose PatternArg  -- ± semitones (`transpose 7`, `transpose "0 7 <5 3>"`)
  | Octave PatternArg     -- ± octaves (`oct -1`, `oct "<0 1>"`)
  | Slow Int         -- stretch the pattern over N cycles (`slow N`); N ≥ 1
  | Fast Int         -- compress the pattern into 1/N of a cycle (`fast N`); N ≥ 1
  | Voice PatternArg -- re-voice each chord; arg interprets to a VoiceShape (`voice open`,
                     -- `voice "<open drop2>"`)
  | Select PerfSel   -- thin each chord to some of its voices (Harmonia takeVoicing)
  | Arpg ArpDir Int  -- explode chord→time: `rate` steps per bar, walked in a
                     -- direction (up/down/updown), cycling the chord's notes. The
                     -- friendly form — a direction toggle and a rate nudge.
  | ArpP String      -- explode chord→time from an explicit INDEX FIGURE: a
                     -- mini-notation of positions into the chord (0 = lowest),
                     -- octave-wrapping past the top. The power form — rests,
                     -- subdivision, alternation, euclid all live here (`arp "0 1 2"`,
                     -- `arp "0 ~ <2 3>"`). Parsed + interpreted at apply-time.
  | Strum Int        -- explode chord→time as a fast onset stagger (ms per note)

derive instance eqPerfFx :: Eq PerfFx

-- | Arpeggiation order of a chord's notes (low→high, high→low, or bounce).
data ArpDir = ArpUp | ArpDown | ArpUpDown

derive instance eqArpDir :: Eq ArpDir

arpDirGlyph :: ArpDir -> String
arpDirGlyph = case _ of
  ArpUp -> "↑"
  ArpDown -> "↓"
  ArpUpDown -> "↕"

cycleArpDir :: ArpDir -> ArpDir
cycleArpDir = case _ of
  ArpUp -> ArpDown
  ArpDown -> ArpUpDown
  ArpUpDown -> ArpUp

-- | A chord's notes in an arp direction's order.
arpOrder :: ArpDir -> Array Int -> Array Int
arpOrder = case _ of
  ArpUp -> identity
  ArpDown -> reverse
  ArpUpDown -> \ns -> ns <> reverse ns

-- | Chord re-voicings — Harmonia `Voicing -> Voicing` strategies.
data VoiceShape = Open | Rootless | Drop2 | Drop24 | Quartal | Cluster

derive instance eqVoiceShape :: Eq VoiceShape

-- | Voice selection — keep the low or high N voices of each chord (Harmonia
-- | `Selector`). `Low "1"` = a bass line; `High "1"` = a melody line. The count is a
-- | `PatternArg` so it can vary (`top "1 2"`, `bottom "<1 2>"`).
data PerfSel = Low PatternArg | High PatternArg

derive instance eqPerfSel :: Eq PerfSel

-- | The verb ARGUMENT type (`PatternArg`), its tokenizer, printer and per-cycle
-- | sampler now live in `Reef.PatternArg` — the shared verb-arg vocabulary both
-- | the Vetula card (micro) and the `Reef.Macro` lane (macro) parse. This module
-- | re-uses the type in the `PerfFx`/`PerfSel` constructors above; callers that want
-- | `PatternArg`/`printArg`/`glyphArg`/`argSrc` import them from `Reef.PatternArg`.

-- | The "WHEN" clause on a layer — the flat form of Tidal's conditional combinators
-- | (`every` / `sometimesBy` / `within`). Rather than a layer that WRAPS a sub-stack,
-- | each layer carries a condition for when it applies, implemented with Tidal's own
-- | cycle-aware combinators. Prototype: `Always` or `Every n` (via `every`).
-- | The gate is an EXPLICIT, named clause (never an implicit peel — §4.5): absent by
-- | default, and when present always written out (`every 4`, `prob 0.3`, `afterbar 16`).
-- | A small ADT for now — each a pure `cycle → Bool` predicate resolved in `applyLayer`;
-- | the open predicate-registry / world-context dream (fullMoon) is the next refinement.
data When
  = Always            -- every cycle (no gate)
  | Every Int         -- each N-th cycle (Tidal's `every`: cycle mod N == 0)
  | Prob Number       -- a P-fraction of cycles, deterministic per-cycle hash (0..1)
  | AfterBar Int      -- only from bar N onward — a build-up / entrance gate
  | Whenmod Int Int   -- Tidal's `whenmod n r`: apply when (cycle mod n) >= r. The
                      -- general cycle gate — subsumes `every` and its complement
                      -- (`whenmod 8 1` = every cycle EXCEPT every 8th).

derive instance eqWhen :: Eq When

whenLabel :: When -> String
whenLabel = case _ of
  Always -> "∀"
  Every n -> "e" <> show n
  Prob p -> "p" <> show p
  AfterBar n -> "▸" <> show n
  Whenmod n r -> "m" <> show n <> "≥" <> show r

cycleWhen :: When -> When
cycleWhen = case _ of
  Always -> Every 2
  Every 2 -> Every 3
  Every 3 -> Every 4
  Every 4 -> Every 8
  Every _ -> Prob 0.5
  Prob _ -> AfterBar 8
  AfterBar _ -> Whenmod 8 1
  Whenmod _ _ -> Always

-- | How a captured multi-channel phrase maps to output when it's a box's source
-- | (recording axis #27, docs/DESIGN-midi-clip-library.md). `Flatten` collapses all
-- | sounding source heads onto the box's single channel and runs the full transform
-- | stack — "as a Vetula voice". `Original` keeps each note on its source channel and
-- | plays faithfully — "as a MIDI clip" (no chord stack; #27b-2). No channel re-map.
data ChannelMode = Flatten | Original

derive instance eqChannelMode :: Eq ChannelMode

channelModeLabel :: ChannelMode -> String
channelModeLabel = case _ of
  Flatten -> "flatten"
  Original -> "orig ch"

-- | A captured phrase attached to a Perform box as its source (in place of a chord
-- | progression). Holds a self-contained COPY of the library clip (small data;
-- | survives the clip being renamed/deleted), the source heads currently muted from
-- | the recording (a live performance control; [] = all sounding), and the channel
-- | mode. Phrase boxes route → midi/rig only (→ odo is meaningless for a frozen
-- | gesture), and are skipped by the eDSL text round-trip (a recording is data, not
-- | structure) — they persist via the record/scene form.
type PhraseAttach =
  { clip :: MidiClip
  , mutedHeads :: Array Int
  , channelMode :: ChannelMode
  }

-- | A stack entry: a function `fx` plus the `when` clause gating it per cycle.
type Layer = { fx :: PerfFx, when :: When }

-- | A fresh layer from the palette applies every cycle until you dial its clause.
mkLayer :: PerfFx -> Layer
mkLayer fx = { fx, when: Always }

-- | What an HTML5 drag is carrying: a fresh layer FROM the palette, or an existing
-- | layer being moved FROM a box's stack (box index, layer index). Dropping onto a
-- | box appends; onto a layer chip inserts before it (reorder / precise placement).
data PerfDragSrc = FromPalette PerfFx | FromBox Int Int

-- | The box's terminal SINK — the fold's cap, one per box, swapped not stacked
-- | (docs/DESIGN-vetula-chyron-redesign §Perform, Model A "forked tail"). The seam
-- | where Solo and Atlantis diverge: `TMidi`/`TOdo` are Solo-capable (browser
-- | WebMIDI / feed Odonus locally); `TRig` is a rig-only destination (CV/OSC/ES-9/
-- | FH-2) the browser can't sound — so in Solo it GHOSTS (silent, greyed). Only
-- | the terminal forks; the whole layer body above it is shared across runtimes.
data PerfTerm = TMidi | TOdo | TRig

derive instance eqPerfTerm :: Eq PerfTerm

termLabel :: PerfTerm -> String
termLabel = case _ of
  TMidi -> "→ midi"
  TOdo -> "→ odo"
  TRig -> "→ rig"

-- | Short pill label for the terminal selector.
termShort :: PerfTerm -> String
termShort = case _ of
  TMidi -> "midi"
  TOdo -> "odo"
  TRig -> "rig"

nextTerm :: PerfTerm -> PerfTerm
nextTerm = case _ of
  TMidi -> TOdo
  TOdo -> TRig
  TRig -> TMidi

-- | A rig-only terminal has no local (browser) realisation.
termRigOnly :: PerfTerm -> Boolean
termRigOnly = case _ of
  TRig -> true
  _ -> false

-- ---------------------------------------------------------------------------
-- The canonical ASCII vocabulary for the Perform pipeline / document round-trip.
-- Round-trippable (NOT the glyph labels); `drop24` is a distinct token from the
-- display `drop2&4`. Shared by App's inline-field round-trip and Lepidoptera.
-- ---------------------------------------------------------------------------

printArpDir :: ArpDir -> String
printArpDir = case _ of
  ArpUp -> "up"
  ArpDown -> "down"
  ArpUpDown -> "updown"

parseArpDir :: String -> ArpDir
parseArpDir = case _ of
  "down" -> ArpDown
  "updown" -> ArpUpDown
  _ -> ArpUp

printVoiceShape :: VoiceShape -> String
printVoiceShape = case _ of
  Open -> "open"
  Rootless -> "rootless"
  Drop2 -> "drop2"
  Drop24 -> "drop24"
  Quartal -> "quartal"
  Cluster -> "cluster"

parseVoiceShape :: String -> Maybe VoiceShape
parseVoiceShape = case _ of
  "open" -> Just Open
  "rootless" -> Just Rootless
  "drop2" -> Just Drop2
  "drop24" -> Just Drop24
  "quartal" -> Just Quartal
  "cluster" -> Just Cluster
  _ -> Nothing
