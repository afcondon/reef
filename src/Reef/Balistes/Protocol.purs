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
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..), MultipleErrors)
import Reef.Balistes.Input (BTagged, WireBInput, fromWire, toWire)
import Reef.Balistes.Sim (BalSim)
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
