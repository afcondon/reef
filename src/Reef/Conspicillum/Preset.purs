-- | **A preset: a whole scene, a name, and the few knobs worth playing.**
-- |
-- | The scene is kept as its line (`Reef.Conspicillum.Notation`), which is
-- | the canonical human form and round-trips exactly, so a preset reads as the
-- | line that makes it. What a line does not carry sits beside it: the query,
-- | a chord progression, and the knobs.
-- |
-- | A preset is authored as a `PresetSource`, with the line as text and chords
-- | by name, and `resolve`d into a `Preset`, with the line parsed and the chords
-- | looked up. Every source is resolved on both runtimes by the conformance
-- | suite, so a line that does not parse or a chord that does not exist fails
-- | there rather than on stage.
-- |
-- | The presets themselves are in `Reef.Conspicillum.Presets`.
module Reef.Conspicillum.Preset
  ( Bank(..)
  , ResonatorFollow(..)
  , Knob
  , knob
  , NamedChord
  , chords
  , chordNamed
  , ProgressionSource
  , Progression
  , PresetSource
  , Preset
  , resolve
  , bankName
  , resonatorFollowName
  ) where

import Prelude

import Data.Array (find)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Reef.Conspicillum.Corpus (Query)
import Reef.Conspicillum.Harmonic (Target)
import Reef.Conspicillum.Notation (Line, parse)
import Reef.Conspicillum.Parameter (Parameter)

-- | Presets come in banks, by what they are a study of.
data Bank
  = Early
  | Found
  | Polychords
  | Sector
  | Fix
  | Dub
  | Swung
  | Harmony
  | Envelopes
  | Effects
  | Resonators
  | Progressions

derive instance eqBank :: Eq Bank

bankName :: Bank -> String
bankName = case _ of
  Early -> "Early"
  Found -> "Found"
  Polychords -> "Polychords"
  Sector -> "Sector"
  Fix -> "Fix"
  Dub -> "Dub"
  Swung -> "Swing"
  Harmony -> "Harmony"
  Envelopes -> "Envelope"
  Effects -> "Effects"
  Resonators -> "Resonator"
  Progressions -> "Progression"

-- | A knob a preset puts on the panel: a parameter, and what this preset calls
-- | it. The same parameter can be "chaos" in one preset and "jump" in another,
-- | because what it does musically depends on the rest of the scene.
type Knob = { parameter :: Parameter, label :: String }

knob :: Parameter -> String -> Knob
knob parameter label = { parameter, label }

-- ── chords ───────────────────────────────────────────────────────────────────

type NamedChord = { name :: String, target :: Target }

-- | The chords the presets' progressions are written in. Named for what the
-- | two chord-hit corpora actually contain, which sit around E, B and F# and
-- | are full of minor-majors, augmented and half-diminished chords: every
-- | progression here was scored against the material before it was written,
-- | and none goes silent.
chords :: Array NamedChord
chords =
  [ chord "Em(maj7)" [ 4, 7, 11, 3 ] 4 4
  , chord "Bm" [ 11, 2, 6 ] 11 11
  , chord "F#m7b5" [ 6, 9, 0, 4 ] 6 6
  , chord "Em7b5" [ 4, 7, 10, 2 ] 4 4
  , chord "F#m" [ 6, 9, 1 ] 6 6
  , chord "Bdim" [ 11, 2, 5 ] 11 11
  , chord "Bm(maj7)" [ 11, 2, 6, 10 ] 11 11
  , chord "Daug" [ 2, 6, 10 ] 2 6
  , chord "E" [ 4, 8, 11 ] 4 4
  , chord "B7" [ 11, 3, 6, 9 ] 11 11
  , chord "C#dim" [ 1, 4, 7 ] 1 1
  , chord "F#dim" [ 6, 9, 0 ] 6 6
  , chord "F#m(maj7)" [ 6, 9, 1, 5 ] 6 6
  , chord "D" [ 2, 6, 9 ] 2 2
  , chord "D#m" [ 3, 6, 10 ] 3 3
  , chord "G" [ 7, 11, 2 ] 7 7
  ]
  where
  chord name pcs root bass = { name, target: { pcs, root, bass } }

chordNamed :: String -> Maybe NamedChord
chordNamed name = find (\c -> c.name == name) chords

-- ── progressions ─────────────────────────────────────────────────────────────

-- | Whether the resonators retune to the chord as it changes: to its root, its
-- | bass, or (per grain) to each of its tones in turn.
data ResonatorFollow = NoFollow | FollowRoot | FollowBass | FollowChordTones

derive instance eqResonatorFollow :: Eq ResonatorFollow

resonatorFollowName :: ResonatorFollow -> String
resonatorFollowName = case _ of
  NoFollow -> "off"
  FollowRoot -> "root"
  FollowBass -> "bass"
  FollowChordTones -> "chord tones"

-- | A chord a cycle, and how hard the cloud holds to it: `minimumFit` is a
-- | hard cut on how well a sample must match the chord, `strength` a lean
-- | toward the ones that match best (`Reef.Conspicillum.Harmonic`).
type ProgressionSource =
  { chords :: Array String
  , minimumFit :: Number
  , strength :: Number
  , resonatorFollows :: ResonatorFollow
  }

type Progression =
  { chords :: Array NamedChord
  , minimumFit :: Number
  , strength :: Number
  , resonatorFollows :: ResonatorFollow
  }

-- ── presets ──────────────────────────────────────────────────────────────────

type PresetSource =
  { name :: String
  , bank :: Bank
  , about :: String
  , line :: String
  , query :: Query
  , progression :: Maybe ProgressionSource
  , knobs :: Array Knob
  }

type Preset =
  { name :: String
  , bank :: Bank
  , about :: String
  , line :: Line
  , query :: Query
  , progression :: Maybe Progression
  , knobs :: Array Knob
  }

resolve :: PresetSource -> Either String Preset
resolve source = do
  line <- case parse source.line of
    Left problem -> Left (source.name <> ": " <> problem)
    Right parsed -> Right parsed
  progression <- traverse resolveProgression source.progression
  pure
    { name: source.name
    , bank: source.bank
    , about: source.about
    , line
    , query: source.query
    , progression
    , knobs: source.knobs
    }
  where
  resolveProgression p = do
    named <- traverse (\name -> note (source.name <> ": no chord called " <> name) (chordNamed name)) p.chords
    pure { chords: named, minimumFit: p.minimumFit, strength: p.strength, resonatorFollows: p.resonatorFollows }
