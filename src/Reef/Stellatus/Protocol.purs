-- | `Reef.Stellatus.Protocol` — the wire format for the Stellatus scene handoff.
-- | The frontend parses its text into a fully-resolved `Scene` (slots + glitch +
-- | jump table + seed), encodes it to JSON, and pushes it to the rig once; the
-- | BEAM `reef_stellatus_voice` decodes it and runs the shared
-- | `Reef.Stellatus.Engine` walk from exactly that definition. Because the walk
-- | is a pure function of the scene, this ONE push is the entire cross-runtime
-- | wire — no per-event stream.
-- |
-- | Unlike Vetula/Balistes, `Scene` carries NO closed ADTs — glitch effects are
-- | already `{ kind :: Int, amount :: Number }` and everything else is
-- | Int/Number/String/Boolean/Array/nested-record. So simple-json's generic
-- | `writeJSON`/`readJSON` cover it directly on both runtimes (the BEAM defers to
-- | `jsx`) with no `toWire`/`fromWire` projection.
module Reef.Stellatus.Protocol
  ( encodeScene
  , decodeScene
  ) where

import Data.Either (Either)
import Foreign (MultipleErrors)
import Reef.Stellatus.Engine (Scene)
import Simple.JSON (readJSON, writeJSON)

-- | Encode a resolved scene for the one-shot handoff push.
encodeScene :: Scene -> String
encodeScene = writeJSON

-- | Decode a pushed scene definition back into a `Scene`.
decodeScene :: String -> Either MultipleErrors Scene
decodeScene = readJSON
