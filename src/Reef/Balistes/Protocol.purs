-- | `Reef.Balistes.Protocol` — the wire format for the Balistes lockstep handoff:
-- | the frontend encodes its full `BalSim` (engine state + render overlay) to JSON,
-- | pushes it to the rig, and the BEAM `reef_balistes_voice` decodes it and runs the
-- | shared `Reef.Balistes.Sim` from exactly that state. `BalSim` is a flat record of
-- | Int / Array Int, so simple-json's generic `writeJSON`/`readJSON` cover it with no
-- | hand-written codec — the same encode/decode compiling to both the JS frontend and
-- | the BEAM (where readJSON defers to `jsx`).
module Reef.Balistes.Protocol
  ( encodeBalSim
  , decodeBalSim
  , encodeBTagged
  , decodeBTagged
  , encodeFixed
  , decodeFixed
  , encodeTrigKit
  , decodeTrigKit
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..), MultipleErrors)
import Reef.Balistes.Input (BTagged, WireBInput, fromWire, toWire)
import Reef.Balistes.Sim (BalSim)
import Reef.Balistes.Fixed (FixedPattern)
import Reef.Balistes.Trig (TrigKit)
import Simple.JSON (readJSON, writeJSON)

-- | Encode the full Balistes simulation state for the handoff push.
encodeBalSim :: BalSim -> String
encodeBalSim = writeJSON

-- | Decode a pushed handoff payload back into a `BalSim`.
decodeBalSim :: String -> Either MultipleErrors BalSim
decodeBalSim = readJSON

-- | Serialize a tick-tagged Balistes input (`{ tick, input }`), flattening the
-- | input to its `WireBInput` so the whole thing is one simple-json record — the
-- | same discipline `Reef.Protocol.encodeTagged` uses for Odonus.
encodeBTagged :: BTagged -> String
encodeBTagged t = writeJSON { tick: t.tick, input: toWire t.input }

-- | Parse a tick-tagged Balistes input back. An unknown tag → decode error rather
-- | than a silent drop.
decodeBTagged :: String -> Either MultipleErrors BTagged
decodeBTagged s = do
  r <- readJSON s :: Either MultipleErrors { tick :: Int, input :: WireBInput }
  case fromWire r.input of
    Just i -> Right { tick: r.tick, input: i }
    Nothing -> Left (singleton (ForeignError ("Reef.Balistes.Protocol: unknown BInput tag " <> show r.input.tag)))

-- | The fixed-rhythm handoff: the whole `FixedPattern` (already wire-flat — cells
-- | carry condX/condY, not a `TrigCond` — so simple-json's generic instance covers
-- | it). The frontend projects its rich pattern onto this and pushes it; the BEAM
-- | decodes and evals `renderFixed` per step.
encodeFixed :: FixedPattern -> String
encodeFixed = writeJSON

decodeFixed :: String -> Either MultipleErrors FixedPattern
decodeFixed = readJSON

-- | The POLYTRIG handoff: the whole resolved `TrigKit` (an array of `{ note,
-- | onsets }`, already wire-flat — the frontend resolved the mini-notation to
-- | onset fractions before pushing, since reef carries no Tidal parser). The BEAM
-- | decodes and evals `renderTrigStep` per step. Like the fixed rhythm, a POLYTRIG
-- | rack is a pure function of the absolute step, so this snaps with no phase-hold.
encodeTrigKit :: TrigKit -> String
encodeTrigKit = writeJSON

decodeTrigKit :: String -> Either MultipleErrors TrigKit
decodeTrigKit = readJSON
