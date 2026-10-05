-- | Triggerfish.Selene.Model — the state of the Selene polysignal rack.
-- |
-- | Selene is a list of **destinations**. A destination is a group of eight
-- | related signals (an "octo" bank of one generator kind) bound to a physical
-- | **target** — eight ES-9 buses, eight ES-5 gates, an ESX-8CV block, an FH-2
-- | bank, or a MIDI channel. You add destinations in groups of eight and
-- | configure each in place; there is no global bank selector. The relation
-- | between a destination's eight slots is the musical idea (a phase-spread LFO
-- | choir, a metric fan of clocks, eight Euclidean rings, a held chord).
-- |
-- | Slot field names are copied verbatim from `Tidal.Selene` (purerl-tidal) so
-- | this ports onto the BEAM cell + the es9-daemon `apply-polysignal` wire
-- | shape. All five families are exposed, polyenv included (2026-08-08).
module Reef.Selene.Model
  ( Selene
  , defaultSelene
  , Destination
  , GenBank(..)
  , GenKind(..)
  , allKinds
  , kindLabel
  , bankKind
  , freshBank
  , Target(..)
  , targetLabel
  , targetWire
  , cycleTarget
  , defaultTargetFor
  , addDestination
  , removeDestination
  , retargetDestination
  , OutputRange(..)
  , rangeLabel
  , rangeToWire
  , rangeOfWire
  , ClockBase(..)
  , clockBaseToWire
  , clockBaseLabel
  , clockBaseBeats
  , ModSlot
  , ClockSlot
  , EuclidSlot
  , PresetNoteSlot
  , EnvSlot
  , defaultEnvSlot
  , slotCount
  , euclidBits
  , noteName
  , lfoValue
  , lfoCyclesShown
  , clampNum
  ) where

import Prelude

import Data.Array (deleteAt, elemIndex, length, mapWithIndex, range, snoc, (!!))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Reef.Numeric (ln, sin) as N

-- ---------------------------------------------------------------------------
-- Generator kinds + their eight-slot banks
-- ---------------------------------------------------------------------------

-- | The shipping polysignal families. All CV/gate sources — the mini-notation
-- | trigger lane that used to live here as `KTrig` was relocated to Balistes'
-- | TIDAL tab, so Selene is purely the ES-9 / FH-2 rack.
-- |
-- | `KEnv` (polyenv) was deferred until 2026-08-08. Both daemons had supported
-- | the family all along; only this UI was missing.
data GenKind = KLfo | KEuclid | KClock | KNote | KEnv

derive instance Eq GenKind

allKinds :: Array GenKind
allKinds = [ KLfo, KEuclid, KClock, KNote, KEnv ]

kindLabel :: GenKind -> String
kindLabel = case _ of
  KLfo -> "POLYLFO"
  KEuclid -> "POLYEUCLID"
  KClock -> "POLYCLOCK"
  KNote -> "POLYNOTE"
  KEnv -> "POLYENV"

-- | An eight-slot bank, typed by its generator. The kind is implied by the
-- | constructor (mirrors `Tidal.Selene`'s `Selene s` sum).
data GenBank
  = GLfo (Array ModSlot)
  | GEuclid (Array EuclidSlot)
  | GClock (Array ClockSlot)
  | GNote (Array PresetNoteSlot)
  | GEnv (Array EnvSlot)

bankKind :: GenBank -> GenKind
bankKind = case _ of
  GLfo _ -> KLfo
  GEuclid _ -> KEuclid
  GClock _ -> KClock
  GNote _ -> KNote
  GEnv _ -> KEnv

-- | The eight slots of an octo bank.
slotCount :: Int
slotCount = 8

-- ---------------------------------------------------------------------------
-- Slot record types (field names match Tidal.Selene exactly)
-- ---------------------------------------------------------------------------

-- | POLYLFO: one continuous waveform per slot = a DC `level` plus a mix of six
-- | shapes, free-running at `rate` Hz from initial `phase`. Amps normalised.
type ModSlot =
  { rate :: Number
  , phase :: Number
  , level :: Number
  , sin :: Number
  , sqr :: Number
  , tri :: Number
  , saw :: Number
  , rnd :: Number
  , nse :: Number
  }

-- | POLYCLOCK: a tempo-locked gate train. `base` / `multiplier` set the
-- | division (beats-per-pulse = base-beats / multiplier), `pulseWidth` the
-- | duty %, `phase` a degrees offset. Rate rides the Link tempo at playback.
type ClockSlot =
  { base :: ClockBase
  , multiplier :: Int
  , pulseWidth :: Int
  , phase :: Int
  }

-- | POLYEUCLID: a Euclidean gate — `beats` pulses spread over `steps`, clocked
-- | at `rate` subdivisions/beat. `accentRate` is an FH-2 feature ES-9 drops.
type EuclidSlot =
  { beats :: Int
  , steps :: Int
  , rate :: Int
  , accentRate :: Int
  }

-- | POLYPRESETNOTE: a static V/oct pitch, one held MIDI note per slot.
type PresetNoteSlot =
  { note :: Int
  }

-- | POLYENV: one MCV-bound ADSR per slot, fired by a MIDI note on the slot's own
-- | channel (envelope N listens on channel N+1 — channels 1..8). Envelope-only:
-- | the MCV emits no gate and no pitch CV, so a note that triggers it has to
-- | carry pitch on a DIFFERENT MCV if you want one.
-- |
-- | Continuous output, so CV-capable banks only — `BankGt` is rejected by the
-- | layout solver, and the daemon answers `capability mismatch` if you try.
-- |
-- | **These fields are exactly the wire contract, not the whole slot.** The
-- | daemon's `decodeEnvSlot` calls `assertKnownKeys`, which REJECTS an unknown
-- | field outright, so this record matches its allow-list rather than the fuller
-- | `PolyEnv.Slot` on the far side.
-- |
-- | `velDepth` is the one that earns its place: bipolar, 64 = no effect, and it
-- | is how MIDI velocity scales the envelope. **It is the only per-note
-- | expression this path can offer** — the shape is config and cannot change per
-- | note — so with it a melodic line has dynamics, and without it every note
-- | comes out identical. Odonus already carries a per-cell velocity, so this
-- | costs no new UI there. (Note it varies the envelope's DEPTH, not its time:
-- | you get loud-and-quiet, not long-and-short.)
-- |
-- | `timeRange` is the firmware's 200ms…50s bucket, and is NOT the wire's
-- | `range`, which is the output VOLTAGE range. Per-slot, so the eight envelopes
-- | can sit at different time scales — per voice, still not per note.
-- |
-- | All three of `depth` / `velDepth` / `timeRange` were plumbed to the hardware
-- | and merely unreachable over the wire until 2026-08-08; `FH2.PolyBank` now
-- | accepts them.
-- |
-- | Omitting per-slot output `range` here deliberately: the destination's
-- | `outputRange` already covers it, and a second place to set the same thing is
-- | the kind of duplication this codebase has spent the week removing.
type EnvSlot =
  { attack :: Int
  , decay :: Int
  , sustain :: Int
  , release :: Int
  , depth :: Int
  , velDepth :: Int
  , timeRange :: Int
  , randomDepth :: Int
  , attackShape :: Int
  , decayShape :: Int
  , releaseShape :: Int
  }

-- | The base an envelope line is READ ONTO and WRITTEN AGAINST — one value, so
-- | the two can't disagree.
-- |
-- | They did. `envLine` elided a field when it matched the musical default
-- | (depth 127, vel 96), while `parseEnv` folded onto the SILENT slot (depth 0,
-- | vel 64). So a full-depth velocity-sensitive envelope printed as bare ADSR
-- | and came back with depth 0 — an envelope that fires and moves nothing — and
-- | with velocity response switched off. Both of the things the FH-2 envelope
-- | work is about, lost on the first round-trip through the text surface, which
-- | is the surface Selene edits through.
-- |
-- | Distinct from `silentEnv` in Source.purs, which is what a MUTED row means:
-- | there, depth 0 is the point. An envelope has no true "off" (depth is not on
-- | the wire), so "muted" has to be spelled as "moves nothing".
defaultEnvSlot :: EnvSlot
defaultEnvSlot =
  { attack: 0, decay: 0, sustain: 0, release: 0
  , depth: 127, velDepth: 96, timeRange: 2
  , randomDepth: 0
  , attackShape: 64, decayShape: 64, releaseShape: 64
  }

-- ---------------------------------------------------------------------------
-- Output target — where a destination's eight outputs physically land
-- ---------------------------------------------------------------------------

-- | A destination's eight outputs go to one of these. CV/gate targets are the
-- | es9-daemon's banks; `Midi` is the Triggerfish extension (route the eight to
-- | a channel). Placement on real FH-2/ES-9 jacks is modelled here; the precise
-- | per-jack assignment UX is a later pass.
data Target
  = ES9Main          -- ES-9 buses 8–15 (panel jacks 1–8)
  | ES9Gt Int        -- ES-5 gate expander block
  | ES9Cv Int        -- ESX-8CV expander block
  | FH2 Int          -- FH-2 + FHX-8 expander bank
  | Midi Int         -- a MIDI channel, 1..16
  | Virtual String   -- no hardware claim; live-control bus prefix

derive instance Eq Target

-- | The hardware a group of eight is (AC's rack, as Triggerfish.Rig has it):
-- | the ES-9's own jacks, the ES-5 then ESX-8GTs, ESX-8CVs, the FH-2's own
-- | eight then FHX-8GTs. A second of a kind is numbered.
targetLabel :: Target -> String
targetLabel = case _ of
  ES9Main -> "ES-9"
  ES9Gt 0 -> "ES-5"
  ES9Gt n -> "ESX-8GT" <> numbered n
  ES9Cv n -> "ESX-8CV" <> numbered (n + 1)
  FH2 0 -> "FH-2"
  FH2 n -> "FHX-8GT" <> numbered n
  Midi n -> "MIDI · CH " <> show n
  Virtual s -> "VIRTUAL · " <> s
  where
  numbered k = if k <= 1 then "" else " " <> show k

targetWire :: Target -> String
targetWire = case _ of
  ES9Main -> "es9main"
  ES9Gt n -> "es9gt" <> show n
  ES9Cv n -> "es98cv" <> show n
  FH2 n -> "fh2_" <> show n
  Midi n -> "midi" <> show n
  Virtual s -> "virtual:" <> s

-- | The cycle of targets the panel walks when you click a destination's chip.
-- | A modest set for now — gate-likes near gate-likes, then MIDI, then virtual.
targetCycle :: Array Target
targetCycle =
  [ ES9Main, ES9Gt 0, ES9Gt 1, ES9Cv 0, FH2 0, Midi 1, Midi 2, Virtual "bus" ]

cycleTarget :: Target -> Target
cycleTarget t =
  let n = fromMaybe 0 (elemIndex t targetCycle)
  in fromMaybe ES9Main (targetCycle !! ((n + 1) `mod` length targetCycle))

-- | A sensible default target when you add a destination of each kind: CV-ish
-- | kinds to CV banks, gate-ish kinds to gates.
defaultTargetFor :: GenKind -> Target
defaultTargetFor = case _ of
  KLfo -> ES9Main
  KNote -> ES9Cv 0
  KEuclid -> ES9Gt 0
  KClock -> ES9Gt 1
  -- Envelopes are CV, so never a gate bank. Defaults to the FH-2 because that is
  -- where the ADSR generator with curve shapes lives.
  KEnv -> FH2 0

-- ---------------------------------------------------------------------------
-- Output range + clock bases
-- ---------------------------------------------------------------------------

data OutputRange
  = Unipolar10V
  | Bipolar5V
  | Unipolar1V
  | Unipolar5V
  | Unipolar8V

derive instance Eq OutputRange

rangeToWire :: OutputRange -> String
rangeToWire = case _ of
  Unipolar10V -> "unipolar10v"
  Bipolar5V -> "bipolar5v"
  Unipolar1V -> "unipolar1v"
  Unipolar5V -> "unipolar5v"
  Unipolar8V -> "unipolar8v"

-- | Inverse of `rangeToWire` — the text surface and the wire share one spelling
-- | rather than growing a second, prettier one to keep in step.
rangeOfWire :: String -> Maybe OutputRange
rangeOfWire = case _ of
  "unipolar10v" -> Just Unipolar10V
  "bipolar5v" -> Just Bipolar5V
  "unipolar1v" -> Just Unipolar1V
  "unipolar5v" -> Just Unipolar5V
  "unipolar8v" -> Just Unipolar8V
  _ -> Nothing

rangeLabel :: OutputRange -> String
rangeLabel = case _ of
  Unipolar10V -> "0–10V"
  Bipolar5V -> "±5V"
  Unipolar1V -> "0–1V"
  Unipolar5V -> "0–5V"
  Unipolar8V -> "0–8V"

data ClockBase
  = ClockWhole
  | ClockHalf
  | ClockQuarter
  | ClockQuarterT
  | ClockEighth
  | ClockEighthT
  | ClockSixteenth
  | ClockSixteenthT
  | ClockThirtySecond
  | ClockThirtySecondT
  | ClockSixtyFourthT

derive instance Eq ClockBase

clockBaseToWire :: ClockBase -> String
clockBaseToWire = case _ of
  ClockWhole -> "whole"
  ClockHalf -> "half"
  ClockQuarter -> "quarter"
  ClockQuarterT -> "qt"
  ClockEighth -> "8th"
  ClockEighthT -> "8t"
  ClockSixteenth -> "16th"
  ClockSixteenthT -> "16t"
  ClockThirtySecond -> "32nd"
  ClockThirtySecondT -> "32t"
  ClockSixtyFourthT -> "64t"

clockBaseLabel :: ClockBase -> String
clockBaseLabel = case _ of
  ClockWhole -> "1/1"
  ClockHalf -> "1/2"
  ClockQuarter -> "1/4"
  ClockQuarterT -> "1/4T"
  ClockEighth -> "1/8"
  ClockEighthT -> "1/8T"
  ClockSixteenth -> "1/16"
  ClockSixteenthT -> "1/16T"
  ClockThirtySecond -> "1/32"
  ClockThirtySecondT -> "1/32T"
  ClockSixtyFourthT -> "1/64T"

clockBaseBeats :: ClockBase -> Number
clockBaseBeats = case _ of
  ClockWhole -> 4.0
  ClockHalf -> 2.0
  ClockQuarter -> 1.0
  ClockQuarterT -> 2.0 / 3.0
  ClockEighth -> 0.5
  ClockEighthT -> 1.0 / 3.0
  ClockSixteenth -> 0.25
  ClockSixteenthT -> 1.0 / 6.0
  ClockThirtySecond -> 0.125
  ClockThirtySecondT -> 1.0 / 12.0
  ClockSixtyFourthT -> 1.0 / 24.0

-- ---------------------------------------------------------------------------
-- The rack
-- ---------------------------------------------------------------------------

-- | A destination: a typed eight-slot bank bound to a physical target, with an
-- | OPTIONAL output range.
-- |
-- | `Nothing` means "don't pin one — let the FH-2 daemon's per-family policy
-- | decide", and that is the normal case. The policy already exists in exactly
-- | one place (`FH2.PolyBank.familyDefaultRange`: gate families and polyenv are
-- | unipolar +5V, LFO and random stay bipolar), so restating it here would be a
-- | second copy to drift.
-- |
-- | It used to be a required field hardcoded to `Bipolar5V` at all five
-- | construction sites, which silently OVERRODE that policy — the field is only
-- | consulted when present. Harmless while the only FH-2 destination was an LFO
-- | (bipolar is right for an LFO) and invisible on ES-9 targets (the range is an
-- | FH-2 jack setting). Envelopes were the first family to expose it: they came
-- | up bipolar and idled at -5V, which is the "all-blue" reading on the FH-2's
-- | display, and the same symptom fh2-config recorded on-rig on 2026-05-13.
-- |
-- | `Just r` is the user pinning it deliberately — e.g. `env fh2_0 unipolar8v`
-- | for more envelope headroom than the +5V default.
type Destination =
  { target :: Target
  , range :: Maybe OutputRange
  , bank :: GenBank
  }

-- | The whole rack — an ordered list of destinations.
type Selene = { destinations :: Array Destination }

-- | Open with one destination of each kind so every visual is on screen,
-- | each on a plausible target.
defaultSelene :: Selene
defaultSelene =
  { destinations:
      [ { target: ES9Main, range: Nothing, bank: freshBank KLfo }
      , { target: ES9Gt 0, range: Nothing, bank: freshBank KEuclid }
      , { target: ES9Gt 1, range: Nothing, bank: freshBank KClock }
      , { target: Midi 1, range: Nothing, bank: freshBank KNote }
      ]
  }

-- | A tasteful eight-slot bank for a kind — eight *related* slots so the "poly"
-- | idea reads immediately.
freshBank :: GenKind -> GenBank
freshBank = case _ of
  KLfo -> GLfo (map lfoSlot ixs)
  KEuclid -> GEuclid (map euclidSlot ixs)
  KClock -> GClock (map clockSlot ixs)
  KNote -> GNote (map noteSlot ixs)
  KEnv -> GEnv (map envSlot ixs)
  where
  ixs = range 0 (slotCount - 1)
  -- eight sines, phase-spread over the cycle (a travelling wave)
  lfoSlot i =
    { rate: 0.5, phase: Int.toNumber i / Int.toNumber slotCount
    , level: 0.0, sin: 0.8, sqr: 0.0, tri: 0.0, saw: 0.0, rnd: 0.0, nse: 0.0
    }
  -- divisions 1/4 ÷ (i+1): a metric fan
  clockSlot i = { base: ClockQuarter, multiplier: i + 1, pulseWidth: 50, phase: 0 }
  -- a spread of classic Euclidean rhythms over 8 or 16 steps
  euclidSlot i =
    let steps = if i < 4 then 8 else 16
        beats = clamp 1 steps (3 + (i `mod` 5))
    in { beats, steps, rate: 4, accentRate: 0 }
  -- C2 G2 C3 D3 E3 G3 C4 E4 — a Cadd9 voicing climbing the bank
  noteSlot i = { note: fromMaybe 60 (chord !! i) }
  chord = [ 36, 43, 48, 50, 52, 55, 60, 64 ]
  -- Eight envelopes walking from plucked to swelling: attack climbs, decay
  -- lengthens, sustain comes up. Reads as one gesture across the bank, which is
  -- the whole point of a "poly" default.
  envSlot i =
    { attack: i * 16
    , decay: 30 + i * 12
    , sustain: i * 14
    , release: 20 + i * 13
    -- velDepth 96 (>64) so velocity opens the envelope from the off: the whole
    -- point of exposing it is that Odonus's per-cell VEL should do something.
    , depth: 127, velDepth: 96, timeRange: 2
    , randomDepth: 0
    , attackShape: 64, decayShape: 64, releaseShape: 64
    }

-- ---------------------------------------------------------------------------
-- Destination edits
-- ---------------------------------------------------------------------------

addDestination :: GenKind -> Selene -> Selene
addDestination kind s =
  s { destinations = snoc s.destinations { target: defaultTargetFor kind, range: Nothing, bank: freshBank kind } }

removeDestination :: Int -> Selene -> Selene
removeDestination i s = s { destinations = fromMaybe s.destinations (deleteAt i s.destinations) }

retargetDestination :: Int -> Selene -> Selene
retargetDestination i s =
  s { destinations = mapWithIndex (\j d -> if j == i then d { target = cycleTarget d.target } else d) s.destinations }

-- ---------------------------------------------------------------------------
-- Derived
-- ---------------------------------------------------------------------------

-- | The Euclidean step pattern (Bresenham): step k is a pulse iff
-- | `(k * beats) mod steps < beats`. The rule the daemon uses.
euclidBits :: EuclidSlot -> Array Boolean
euclidBits sl =
  map (\k -> (k * sl.beats) `mod` sl.steps < sl.beats) (range 0 (sl.steps - 1))

-- | A MIDI note as pitch-class + octave (middle C = 60 = "C4").
noteName :: Int -> String
noteName n =
  let
    pcs = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]
    pc = fromMaybe "?" (pcs !! (n `mod` 12))
    oct = n / 12 - 1
  in
    pc <> show oct

-- | How many waveform cycles to draw for an LFO slot — log-mapped over the
-- | supported rate range and clamped, so the huge frequency span stays legible:
-- | slow LFOs show ~half a cycle, fast ones a few, never a solid blur.
lfoCyclesShown :: Number -> Number
lfoCyclesShown rate =
  let
    lo = N.ln 0.02
    hi = N.ln 20.0
    u = clampNum 0.0 1.0 ((N.ln (clampNum 0.02 20.0 rate) - lo) / (hi - lo))
  in
    0.5 + u * 3.0

-- | Sample a slot's summed waveform at position `u` ∈ [0,1) across one drawn
-- | cell. Returns the raw value (level + Σ amp·shape); the renderer normalises.
-- | Deterministic shapes only (sin/sqr/tri/saw); rnd/nse are shown as a hint by
-- | the renderer, not sampled here.
lfoValue :: ModSlot -> Number -> Number
lfoValue sl u =
  let
    cycles = lfoCyclesShown sl.rate
    t = u * cycles + sl.phase   -- phase in cycles
    frac = t - Int.toNumber (Int.floor t)
    twoPi = 2.0 * 3.141592653589793
    sinW = N.sin (twoPi * t)
    sqrW = if frac < 0.5 then 1.0 else -1.0
    triW = if frac < 0.5 then 4.0 * frac - 1.0 else 3.0 - 4.0 * frac
    sawW = 2.0 * frac - 1.0
  in
    sl.level + sl.sin * sinW + sl.sqr * sqrW + sl.tri * triW + sl.saw * sawW

clampNum :: Number -> Number -> Number -> Number
clampNum lo hi v = if v < lo then lo else if v > hi then hi else v
