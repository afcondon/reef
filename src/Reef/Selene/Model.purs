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
  , retrigBase
  , lfoRateOn
  , cycleTarget
  , defaultTargetFor
  , addDestination
  , removeDestination
  , retargetDestination
  , Jacks
  , allJacks
  , isWholeBank
  , jacksToken
  , jacksOfToken
  , jackNumbers
  , jacksOverlap
  , placeWire
  , samePlace
  , overlaps
  , freshBankOn
  , takeSlots
  , widenBank
  , bankSize
  , bankJacks
  , placesOf
  , VoiceBank
  , VoiceSlot
  , VoiceOut(..)
  , voiceOuts
  , voiceOutsWord
  , voiceOutsOfWord
  , voiceStride
  , voiceChannel
  , OutputRange(..)
  , rangeLabel
  , rangeToWire
  , rangeOfWire
  , ClockBase(..)
  , clockBaseToWire
  , clockBaseLabel
  , clockBaseBeats
  , clockBaseTicks
  , clockTicks
  , clockRatio
  , clockOfTicks
  , clockOfRatio
  , clockOn
  , euclidOn
  , ModSlot
  , LfoWave(..)
  , lfoWaves
  , waveName
  , waveOfName
  , ClockSlot
  , EuclidSlot
  , PresetNoteSlot
  , EnvSlot
  , defaultEnvSlot
  , slotCount
  , euclidBits
  , maxEuclidSteps
  , noteName
  , lfoValue
  , lfoCyclesShown
  , clampNum
  ) where

import Prelude

import Data.Array as Array
import Data.Array (deleteAt, elemIndex, length, mapWithIndex, range, snoc, (!!))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as Str
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
data GenKind = KLfo | KEuclid | KClock | KNote | KEnv | KVoice

derive instance Eq GenKind

allKinds :: Array GenKind
allKinds = [ KLfo, KEuclid, KClock, KNote, KEnv, KVoice ]

kindLabel :: GenKind -> String
kindLabel = case _ of
  KLfo -> "POLYLFO"
  KEuclid -> "POLYEUCLID"
  KClock -> "POLYCLOCK"
  KNote -> "POLYNOTE"
  KEnv -> "POLYENV"
  KVoice -> "POLYVOICE"

-- | An eight-slot bank, typed by its generator. The kind is implied by the
-- | constructor (mirrors `Tidal.Selene`'s `Selene s` sum).
data GenBank
  = GLfo (Array ModSlot)
  | GEuclid (Array EuclidSlot)
  | GClock (Array ClockSlot)
  | GNote (Array PresetNoteSlot)
  | GEnv (Array EnvSlot)
  -- | a voice layout (docs/kb/plans/selene-voice-layouts.md): MIDI voices,
  -- | not free-running signals, laid out as Yarns lays out its parts
  | GVoice VoiceBank

bankKind :: GenBank -> GenKind
bankKind = case _ of
  GVoice _ -> KVoice
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
-- | `wave` is the shape the slot is edited as. While any amp is above zero the
-- | mix speaks for itself; `wave` is what keeps a slot turned all the way down
-- | a square (say), so that turning it back up gives the square again.
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
  , wave :: LfoWave
  }

-- | The six shapes of a POLYLFO slot, in the rack's order.
data LfoWave = WaveSin | WaveSqr | WaveTri | WaveSaw | WaveRnd | WaveNse

derive instance Eq LfoWave

lfoWaves :: Array LfoWave
lfoWaves = [ WaveSin, WaveSqr, WaveTri, WaveSaw, WaveRnd, WaveNse ]

-- | The rack's keyword for a shape.
waveName :: LfoWave -> String
waveName = case _ of
  WaveSin -> "sin"
  WaveSqr -> "sqr"
  WaveTri -> "tri"
  WaveSaw -> "saw"
  WaveRnd -> "rnd"
  WaveNse -> "nse"

waveOfName :: String -> Maybe LfoWave
waveOfName k = Array.find (\w -> waveName w == k) lfoWaves

-- | POLYCLOCK: a tempo-locked gate train. A pulse comes every `base` ×
-- | `multiplier` (the FH-2's own fields, and the wire's), but a clock is
-- | written and shown as its ratio to the beat (`clockRatio`): `×1` the beat,
-- | `÷4` one pulse every four beats, `×4` four a beat, `×3/2` a quarter
-- | triplet. Every ratio is a whole number of ticks (24 a beat), which is
-- | what both rigs play. `pulseWidth` is the duty %, 0 silent; `phase` a
-- | degrees offset. Rate rides the Link tempo at playback.
type ClockSlot =
  { base :: ClockBase
  , multiplier :: Int
  , pulseWidth :: Int
  , phase :: Int
  }

-- | POLYEUCLID: a Euclidean gate — `beats` pulses spread over `steps` (1 to
-- | `maxEuclidSteps`), turned by `rotation` steps (the pattern starts that
-- | many steps later, as Odonus's ring turns), clocked at `rate` steps a beat
-- | (4 = 16ths), each gate half a step long, on every rig. `accentRate` is an
-- | FH-2 feature ES-9 drops.
type EuclidSlot =
  { beats :: Int
  , steps :: Int
  , rotation :: Int
  , rate :: Int
  , accentRate :: Int
  }

-- | The longest Euclid on any rig: the FH-2's patterns and es9-daemon's
-- | pattern word both stop at 32 steps.
maxEuclidSteps :: Int
maxEuclidSteps = 32

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

-- | A voice layout: N independent MIDI voices, each a converter on its own
-- | channel. Each voice's CV outputs (pitch, then `outputs` in the FH-2's
-- | order) take consecutive jacks of the destination's run, so a voice is
-- | `voiceStride` jacks wide; its gate is the k-th jack of `gates`, a run of
-- | a gate bank, gathered there as Yarns keeps its gates on their own jacks.
type VoiceBank =
  { outputs :: Array VoiceOut
  , gates :: Maybe { target :: Target, jacks :: Jacks }
  , voices :: Array VoiceSlot
  }

-- | One voice: the MIDI channel that plays it, and its envelope (used when
-- | the layout has one).
type VoiceSlot = { channel :: Int, env :: EnvSlot }

-- | A voice's CV outputs beyond its pitch, in the order the FH-2 lays them out.
data VoiceOut = VVelocity | VEnvelope | VPressure

derive instance Eq VoiceOut

voiceOuts :: Array VoiceOut
voiceOuts = [ VVelocity, VEnvelope, VPressure ]

-- | The rack's word for a layout's outputs: `pitch`, `pitch+env`,
-- | `pitch+vel+env+pres`.
voiceOutsWord :: Array VoiceOut -> String
voiceOutsWord os = Str.joinWith "+" ([ "pitch" ] <> map short (Array.filter (\o -> Array.elem o os) voiceOuts))
  where
  short = case _ of
    VVelocity -> "vel"
    VEnvelope -> "env"
    VPressure -> "pres"

voiceOutsOfWord :: String -> Maybe (Array VoiceOut)
voiceOutsOfWord w = case Array.uncons (Str.split (Str.Pattern "+") w) of
  Just { head: "pitch", tail } -> map (\os -> Array.filter (\o -> Array.elem o os) voiceOuts) (traverseMaybe out tail)
  _ -> Nothing
  where
  out = case _ of
    "vel" -> Just VVelocity
    "env" -> Just VEnvelope
    "pres" -> Just VPressure
    _ -> Nothing
  traverseMaybe f xs = Array.foldl (\acc x -> (\a b -> Array.snoc a b) <$> acc <*> f x) (Just []) xs

-- | How many jacks one voice takes: its pitch and its other outputs.
voiceStride :: VoiceBank -> Int
voiceStride vb = 1 + length vb.outputs

-- | A voice's default MIDI channel: from 11 (the FH-2 voices' channels,
-- | 11-14), or earlier when there are more voices than 11..16 holds.
voiceChannel :: Int -> Int -> Int
voiceChannel n k = min 11 (17 - n) + k

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
  | FH2 Int          -- the FH-2's own eight (0), then each FHX-8GT (1..)
  | FH2Cv Int        -- FHX-8CV expander, by expansion index (0 = expansion 1)
  | Midi Int         -- a MIDI channel, 1..16
  | Virtual String   -- no hardware claim; live-control bus prefix

derive instance Eq Target

-- | The hardware a group of eight is (AC's rack, as Triggerfish.Rig has it):
-- | the ES-9's own jacks, the ES-5 then ESX-8GTs, ESX-8CVs, the FH-2's own
-- | eight then FHX-8GTs, FHX-8CVs. A second of a kind is numbered.
targetLabel :: Target -> String
targetLabel = case _ of
  ES9Main -> "ES-9"
  ES9Gt 0 -> "ES-5"
  ES9Gt n -> "ESX-8GT" <> numbered n
  ES9Cv n -> "ESX-8CV" <> numbered (n + 1)
  FH2 0 -> "FH-2"
  FH2 n -> "FHX-8GT" <> numbered n
  FH2Cv n -> "FHX-8CV" <> numbered (n + 1)
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
  FH2Cv n -> "fh2cv" <> show n
  Midi n -> "midi" <> show n
  Virtual s -> "virtual:" <> s

-- | The FH-2 output (0-based) a bank's slot 1 sits on, when its LFOs can be
-- | reset: the FH-2's own eight and each FHX-8CV (an FHX-8GT has no LFOs, and
-- | the ES-9's daemon has no reset). Slot k resets output base + k - 1.
retrigBase :: Target -> Maybe Int
retrigBase = case _ of
  FH2 0 -> Just 0
  FH2Cv n -> Just (8 + 8 * n)
  _ -> Nothing

-- | The rate an LFO slot really runs at on a target, when that is not the
-- | rate asked for. The FH-2's free-running LFOs span 0.1 Hz to 10.36 Hz
-- | (measured 2026-10-16; fh2-config `PolyLfo.hzAtSpeed`), so on an FH-2 bank
-- | a slower or faster rate clamps to that end. Nothing elsewhere (the ES-9's
-- | LFOs are computed, and go as slow as asked), and nothing for a rate of
-- | zero or less, a slot that is a level rather than a wave.
lfoRateOn :: Target -> Number -> Maybe Number
lfoRateOn target hz = case target of
  FH2 _ -> fh2
  FH2Cv _ -> fh2
  _ -> Nothing
  where
  clamped = max 0.1 (min 10.36 hz)
  fh2 = if hz > 0.0 && clamped /= hz then Just clamped else Nothing

-- | The cycle of targets the panel walks when you click a destination's chip.
-- | A modest set for now — gate-likes near gate-likes, then MIDI, then virtual.
targetCycle :: Array Target
targetCycle =
  [ ES9Main, ES9Gt 0, ES9Cv 0, FH2 0, FH2 1, FH2Cv 0, Midi 1, Midi 2, Virtual "bus" ]

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
  -- The FHX-8GT, so a new clock and a new rhythm don't meet on the ES-5. There
  -- is no ESX-8GT (`ES9Gt 1`) in AC's rig; defaulting there put a bank no
  -- daemon has on the page (2026-10-19).
  KClock -> FH2 1
  -- Envelopes are CV, so never a gate bank. Defaults to the FH-2 because that is
  -- where the ADSR generator with curve shapes lives.
  KEnv -> FH2 0
  -- MIDI voices: the FH-2 is where the converters are
  KVoice -> FH2 0

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

-- | A base in ticks of 24 per beat, the FH-2's own count (fh2-config
-- | `baseDurationByte`): every clock on every rig is a whole number of them.
clockBaseTicks :: ClockBase -> Int
clockBaseTicks = case _ of
  ClockWhole -> 96
  ClockHalf -> 48
  ClockQuarter -> 24
  ClockQuarterT -> 16
  ClockEighth -> 12
  ClockEighthT -> 8
  ClockSixteenth -> 6
  ClockSixteenthT -> 4
  ClockThirtySecond -> 3
  ClockThirtySecondT -> 2
  ClockSixtyFourthT -> 1

-- | A clock's period in ticks of 24 a beat.
clockTicks :: ClockSlot -> Int
clockTicks sl = clockBaseTicks sl.base * max 1 sl.multiplier

-- | A clock as its ratio to the beat, exact: `×4` four pulses a beat, `÷4`
-- | one every four beats, `×3/2` for the rest (a quarter triplet).
clockRatio :: ClockSlot -> String
clockRatio sl =
  let
    t = clockTicks sl
    g = gcd 24 t
    perBeat = 24 / g
    beats = t / g
  in
    if beats == 1 then "×" <> show perBeat
    else if perBeat == 1 then "÷" <> show beats
    else "×" <> show perBeat <> "/" <> show beats

-- | The base and multiplier of a period in ticks: a quarter's multiple when
-- | it is whole beats, else the longest base that divides it.
clockOfTicks :: Int -> Maybe { base :: ClockBase, multiplier :: Int }
clockOfTicks t
  | t < 1 = Nothing
  | t `mod` 24 == 0 = Just { base: ClockQuarter, multiplier: t / 24 }
  | otherwise =
      Array.find (\b -> t `mod` clockBaseTicks b == 0) longestFirst
        <#> \b -> { base: b, multiplier: t / clockBaseTicks b }
  where
  longestFirst =
    [ ClockWhole, ClockHalf, ClockQuarter, ClockQuarterT, ClockEighth, ClockEighthT
    , ClockSixteenth, ClockSixteenthT, ClockThirtySecond, ClockThirtySecondT, ClockSixtyFourthT ]

-- | A ratio as written: `×4`, `x4`, `*4` or a bare `4` (pulses a beat, and
-- | `×3/2` a fraction of them), `÷4` or `/4` (beats a pulse). Nothing for a
-- | ratio that isn't a whole number of ticks (`×5`: 24/5), which no rig
-- | plays.
clockOfRatio :: String -> Maybe { base :: ClockBase, multiplier :: Int }
clockOfRatio tok = case slower of
  Just n -> positive n >>= \k -> clockOfTicks (24 * k)
  Nothing -> case Str.split (Str.Pattern "/") faster of
    [ p ] -> Int.fromString p >>= positive >>= \k -> ticksOf k 1
    [ p, q ] -> do
      k <- Int.fromString p >>= positive
      d <- Int.fromString q >>= positive
      ticksOf k d
    _ -> Nothing
  where
  slower = firstOf [ "÷", "/" ] >>= Int.fromString
  faster = fromMaybe tok (firstOf [ "×", "x", "*" ])
  firstOf = Array.findMap (\pre -> Str.stripPrefix (Str.Pattern pre) tok)
  positive n = if n >= 1 then Just n else Nothing
  -- k pulses every d beats: a period of 24d/k ticks, when that is whole
  ticksOf k d = if (24 * d) `mod` k == 0 then clockOfTicks (24 * d / k) else Nothing

-- | Why a Euclid slot cannot be played as asked on a target, when it cannot:
-- | the FH-2 then plays it silent, and the page marks it. The FH-2's step is a
-- | whole number of ticks (24 a beat), so `rate` must divide 24 (1, 2, 3, 4,
-- | 6, 8, 12, 24).
euclidOn :: Target -> EuclidSlot -> Maybe String
euclidOn target sl = case target of
  FH2 _ -> fh2
  FH2Cv _ -> fh2
  _ -> Nothing
  where
  fh2
    | sl.rate < 1 || 24 `mod` sl.rate /= 0 =
        Just ("the FH-2's steps are 1, 2, 3, 4, 6, 8, 12 or 24 a beat, not " <> show sl.rate)
    | otherwise = Nothing

-- | Why a clock slot cannot be played as asked on a target, when it cannot:
-- | the FH-2 then plays it silent, and the page marks it. The FH-2's pulse is
-- | 50% or a fixed length in milliseconds, so a duty is exact only at 50%
-- | (or 0, silent); its multiplier stops at 127, and its shift (the phase,
-- | in ticks of 24 a beat) at 127 too. The ES-9 computes its
-- | clocks and plays any of them.
clockOn :: Target -> ClockSlot -> Maybe String
clockOn target sl = case target of
  FH2 _ -> fh2
  FH2Cv _ -> fh2
  _ -> Nothing
  where
  fh2
    | sl.pulseWidth /= 0 && sl.pulseWidth /= 50 =
        Just ("the FH-2's clocks are 50% or a fixed pulse, not " <> show sl.pulseWidth <> "%")
    | sl.multiplier > 127 = Just ("the FH-2's multiplier stops at 127, not " <> show sl.multiplier)
    | shiftTicks > 127 = Just ("the FH-2 shifts a clock by at most 127 ticks of 24 a beat, not " <> show shiftTicks)
    | otherwise = Nothing
  period = clockBaseTicks sl.base * max 1 sl.multiplier
  -- non-negative before `mod`: purerl's Int `mod` truncates
  degrees = ((sl.phase `mod` 360) + 360) `mod` 360
  shiftTicks = ((degrees * period + 180) / 360) `mod` period

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
-- |
-- | `retrig` is a Tidal mini-notation pattern of slot numbers (1..8, or `all`),
-- | kept as text: the rig plays it as LFO resets on the beat (an FH-2 LFO bank
-- | only, whose outputs reset on a note; `FH2.LfoReset`). reef never parses it,
-- | which keeps Littorina out of reef.
-- |
-- | `muted` silences the destination without touching its bank: the daemon is
-- | sent the bank's silent version (`Reef.Selene.Wire.destinationEnvelope`), its
-- | reset pattern stops, and edits made while muted change the bank, so unmuting
-- | brings back the bank as it then is.
-- |
-- | `mutedSlots` does the same for single outputs (0-based): each keeps its
-- | settings and goes out silent, so unmuting one brings it back as it is.
-- |
-- | `jacks` is the run of the target's eight that the destination sits on
-- | (docs/kb/plans/selene-split-banks.md): the whole bank (`allJacks`), or
-- | part of it, as four LFOs on jacks 1-4 beside four pitches on 5-8. The
-- | bank has one slot per jack of the run.
type Destination =
  { target :: Target
  , jacks :: Jacks
  , range :: Maybe OutputRange
  , bank :: GenBank
  , retrig :: Maybe String
  , muted :: Boolean
  , mutedSlots :: Array Int
  }

-- | The whole rack — an ordered list of destinations.
type Selene = { destinations :: Array Destination }

-- | Open with one destination of each kind so every visual is on screen,
-- | each on a plausible target.
defaultSelene :: Selene
defaultSelene =
  { destinations:
      [ { target: ES9Main, jacks: allJacks, range: Nothing, bank: freshBank KLfo, retrig: Nothing, muted: false, mutedSlots: [] }
      , { target: ES9Gt 0, jacks: allJacks, range: Nothing, bank: freshBank KEuclid, retrig: Nothing, muted: false, mutedSlots: [] }
      , { target: FH2 1, jacks: allJacks, range: Nothing, bank: freshBank KClock, retrig: Nothing, muted: false, mutedSlots: [] }
      , { target: Midi 1, jacks: allJacks, range: Nothing, bank: freshBank KNote, retrig: Nothing, muted: false, mutedSlots: [] }
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
  -- eight pitches, the simplest layout; their gates found a place when the
  -- bank is put on the rack (Reef.Selene.Line)
  KVoice -> GVoice { outputs: [], gates: Nothing, voices: map (\k -> { channel: voiceChannel slotCount k, env: envSlot 4 }) ixs }
  where
  ixs = range 0 (slotCount - 1)
  -- eight sines, phase-spread over the cycle (a travelling wave)
  lfoSlot i =
    { rate: 0.5, phase: Int.toNumber i / Int.toNumber slotCount
    , level: 0.0, sin: 0.8, sqr: 0.0, tri: 0.0, saw: 0.0, rnd: 0.0, nse: 0.0
    , wave: WaveSin
    }
  -- a clock divider: the beat, then one pulse every 2, 3 … 8 beats
  clockSlot i = { base: ClockQuarter, multiplier: i + 1, pulseWidth: 50, phase: 0 }
  -- a spread of classic Euclidean rhythms over 8 or 16 steps
  euclidSlot i =
    let steps = if i < 4 then 8 else 16
        beats = clamp 1 steps (3 + (i `mod` 5))
    in { beats, steps, rotation: 0, rate: 4, accentRate: 0 }
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
  s { destinations = snoc s.destinations { target: defaultTargetFor kind, jacks: allJacks, range: Nothing, bank: freshBank kind, retrig: Nothing, muted: false, mutedSlots: [] } }

removeDestination :: Int -> Selene -> Selene
removeDestination i s = s { destinations = fromMaybe s.destinations (deleteAt i s.destinations) }

retargetDestination :: Int -> Selene -> Selene
retargetDestination i s =
  s { destinations = mapWithIndex (\j d -> if j == i then d { target = cycleTarget d.target } else d) s.destinations }

-- ---------------------------------------------------------------------------
-- Runs of jacks (docs/kb/plans/selene-split-banks.md)
-- ---------------------------------------------------------------------------

-- | A contiguous run of a bank's eight jacks, 1-based: `first` and how many.
type Jacks = { first :: Int, count :: Int }

allJacks :: Jacks
allJacks = { first: 1, count: slotCount }

isWholeBank :: Jacks -> Boolean
isWholeBank j = j == allJacks

-- | How a run follows the bank in text: nothing for the whole bank, `:5-8`
-- | for a run, `:3` for one jack.
jacksToken :: Jacks -> String
jacksToken j
  | isWholeBank j = ""
  | j.count == 1 = ":" <> show j.first
  | otherwise = ":" <> show j.first <> "-" <> show (j.first + j.count - 1)

-- | `5-8` or `3` (what follows the colon), inside 1..8, low to high.
jacksOfToken :: String -> Maybe Jacks
jacksOfToken tok = case map Int.fromString (splitDash tok) of
  [ Just a ] | a >= 1 && a <= slotCount -> Just { first: a, count: 1 }
  [ Just a, Just b ] | a >= 1 && b <= slotCount && a <= b -> Just { first: a, count: b - a + 1 }
  _ -> Nothing
  where
  splitDash t = case Str.indexOf (Str.Pattern "-") t of
    Just at -> [ Str.take at t, Str.drop (at + 1) t ]
    Nothing -> [ t ]

-- | The run's jack numbers, 1-based.
jackNumbers :: Jacks -> Array Int
jackNumbers j = range j.first (j.first + j.count - 1)

jacksOverlap :: Jacks -> Jacks -> Boolean
jacksOverlap a b = a.first <= b.first + b.count - 1 && b.first <= a.first + a.count - 1

-- | A destination's place as text: the target's wire token and its run.
placeWire :: Target -> Jacks -> String
placeWire t j = targetWire t <> jacksToken j

samePlace :: forall r s. { target :: Target, jacks :: Jacks | r } -> { target :: Target, jacks :: Jacks | s } -> Boolean
samePlace a b = a.target == b.target && a.jacks == b.jacks

-- | Each pair of destinations that share a jack, said. Empty when none do.
-- | The rack keeps such a pair (the editor never blanks while you type); the
-- | page and the line refuse to make one, and the daemons refuse to play one.
overlaps :: Selene -> Array String
overlaps s = Array.catMaybes do
  i <- range 0 (length places - 1)
  j <- Array.filter (_ > i) (range 0 (length places - 1))
  pure case places !! i, places !! j of
    Just a, Just b | a.target == b.target && jacksOverlap a.jacks b.jacks ->
      Just (placeWire a.target a.jacks <> " and " <> placeWire b.target b.jacks <> " share jacks")
    _, _ -> Nothing
  where
  places = Array.concatMap placesOf s.destinations

-- | Every place a destination holds: its run, and a voice layout's gates.
placesOf :: Destination -> Array { target :: Target, jacks :: Jacks }
placesOf d = [ { target: d.target, jacks: d.jacks } ] <> case d.bank of
  GVoice vb -> Array.fromFoldable vb.gates
  _ -> []

-- | The first `n` jacks' worth of a bank: a bank sized to a run (a slot a
-- | jack; a voice layout's whole voices).
takeSlots :: Int -> GenBank -> GenBank
takeSlots n = case _ of
  GLfo xs -> GLfo (Array.take n xs)
  GEuclid xs -> GEuclid (Array.take n xs)
  GClock xs -> GClock (Array.take n xs)
  GNote xs -> GNote (Array.take n xs)
  GEnv xs -> GEnv (Array.take n xs)
  -- `n` jacks hold n / stride whole voices, and that many gates
  GVoice vb ->
    let m = n / voiceStride vb
    in GVoice vb { voices = Array.take m vb.voices, gates = map (\g -> g { jacks = g.jacks { count = min g.jacks.count m } }) vb.gates }

-- | How many slots (and so jacks) a bank has.
bankSize :: GenBank -> Int
bankSize = case _ of
  GLfo xs -> length xs
  GEuclid xs -> length xs
  GClock xs -> length xs
  GNote xs -> length xs
  GEnv xs -> length xs
  GVoice vb -> length vb.voices

-- | How many jacks a bank drives: a slot each, or a voice's stride each.
bankJacks :: GenBank -> Int
bankJacks = case _ of
  GVoice vb -> length vb.voices * voiceStride vb
  b -> bankSize b

-- | A run's bank spread back over the whole of the bank: its slots on their
-- | jacks, and the fresh eight's slots on the rest.
widenBank :: Jacks -> GenBank -> GenBank
widenBank j bank = case freshBank (bankKind bank), bank of
  GLfo f, GLfo xs -> GLfo (spread f xs)
  GEuclid f, GEuclid xs -> GEuclid (spread f xs)
  GClock f, GClock xs -> GClock (spread f xs)
  GNote f, GNote xs -> GNote (spread f xs)
  GEnv f, GEnv xs -> GEnv (spread f xs)
  -- a layout grows by whole voices, before and after its run
  _, GVoice vb ->
    let
      n = slotCount / voiceStride vb
      before = (j.first - 1) / voiceStride vb
      fresh = map (\k -> { channel: voiceChannel n k, env: (freshVoice k).env }) (range 0 (n - 1))
      vs = Array.take before fresh <> vb.voices <> Array.drop (before + length vb.voices) fresh
    in GVoice vb { voices = Array.take n vs, gates = map (\g -> g { jacks = g.jacks { count = n } }) vb.gates }
  _, b -> b
  where
  spread :: forall a. Array a -> Array a -> Array a
  spread f xs = Array.take (j.first - 1) f <> xs <> Array.drop (j.first - 1 + j.count) f
  freshVoice k = case freshBank KVoice of
    GVoice vb -> fromMaybe { channel: 11, env: defaultEnvSlot } (vb.voices !! k)
    _ -> { channel: 11, env: defaultEnvSlot }

-- | A fresh bank of a kind sized to a run: the fresh eight's first slots.
freshBankOn :: Jacks -> GenKind -> GenBank
freshBankOn j k = takeSlots j.count (freshBank k)

-- ---------------------------------------------------------------------------
-- Derived
-- ---------------------------------------------------------------------------

-- | The Euclidean step pattern (Bresenham), turned: step k is a pulse iff
-- | `((k - rotation) * beats) mod steps < beats`. The rule both daemons are
-- | given, and `Triggerfish.Ui.Euclid.hit`'s.
euclidBits :: EuclidSlot -> Array Boolean
euclidBits sl =
  map (\k -> (wrapStep (k - sl.rotation) * sl.beats) `mod` n < sl.beats) (range 0 (n - 1))
  where
  n = max 1 sl.steps
  -- non-negative before `mod`: purerl's Int `mod` truncates
  wrapStep a = ((a `mod` n) + n) `mod` n

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
