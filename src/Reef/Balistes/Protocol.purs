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
  ) where

import Prelude

import Data.Either (Either)
import Foreign (MultipleErrors)
import Reef.Balistes.Sim (BalSim)
import Simple.JSON (readJSON, writeJSON)

-- | Encode the full Balistes simulation state for the handoff push.
encodeBalSim :: BalSim -> String
encodeBalSim = writeJSON

-- | Decode a pushed handoff payload back into a `BalSim`.
decodeBalSim :: String -> Either MultipleErrors BalSim
decodeBalSim = readJSON
