-- | **A preset: a whole scene, a name, and the few knobs worth playing.**
-- |
-- | The scene is kept as its line (`Reef.Conspicillum.Notation`), which is
-- | the canonical human form and round-trips exactly, so a preset reads as the
-- | line that makes it, chord progression included. What a line does not carry
-- | sits beside it: the query, and the knobs.
-- |
-- | A preset is authored as a `PresetSource`, with the line as text, and
-- | `resolve`d into a `Preset` with the line parsed. Every source is resolved
-- | on both runtimes by the conformance suite, so a line that does not parse,
-- | or names a chord that does not exist, fails there rather than on stage.
-- |
-- | The presets themselves are in `Reef.Conspicillum.Presets`.
module Reef.Conspicillum.Preset
  ( Bank(..)
  , Knob
  , knob
  , PresetSource
  , Preset
  , resolve
  , bankName
  , PresetOnWire
  , presetOnWire
  ) where

import Prelude

import Data.Either (Either(..))
import Reef.Conspicillum.Corpus (Query)
import Reef.Conspicillum.Notation (Line, parse, print)
import Data.Maybe (Maybe(..))
import Reef.Conspicillum.Parameter (Parameter, identifier)
import Reef.Conspicillum.Protocol (WireQuery, WireSpec, toWireQuery, toWireSpec)

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

-- ── presets ──────────────────────────────────────────────────────────────────

type PresetSource =
  { name :: String
  , bank :: Bank
  , about :: String
  , line :: String
  , query :: Query
  , knobs :: Array Knob
  }

type Preset =
  { name :: String
  , bank :: Bank
  , about :: String
  , line :: Line
  , query :: Query
  , knobs :: Array Knob
  }

resolve :: PresetSource -> Either String Preset
resolve source = case parse source.line of
  Left problem -> Left (source.name <> ": " <> problem)
  Right line -> Right
    { name: source.name
    , bank: source.bank
    , about: source.about
    , line
    , query: source.query
    , knobs: source.knobs
    }

-- ── for pages ────────────────────────────────────────────────────────────────

-- | A preset as plain data for a page written in JavaScript: the scene's wire
-- | spec (the same shape `conspicillum-scene` sends), its wire query, and the
-- | knobs by their stable identifiers. The pages read their presets from this,
-- | so reef is the only copy. `sample` is -1 when the scene plays the whole set.
type PresetOnWire =
  { name :: String
  , bank :: String
  , about :: String
  , line :: String
  , set :: String
  , sample :: Int
  , whole :: Boolean
  , seed :: Int
  , spec :: WireSpec
  , query :: WireQuery
  , knobs :: Array { parameter :: String, label :: String }
  }

presetOnWire :: Preset -> PresetOnWire
presetOnWire p =
  { name: p.name
  , bank: bankName p.bank
  , about: p.about
  , line: print p.line
  , set: p.line.set
  , sample: case p.line.n of
      Just k -> k
      Nothing -> -1
  , whole: case p.line.n of
      Just _ -> false
      Nothing -> true
  , seed: p.line.seed
  , spec: toWireSpec p.line.spec
  , query: toWireQuery p.query
  , knobs: map (\k -> { parameter: identifier k.parameter, label: k.label }) p.knobs
  }
