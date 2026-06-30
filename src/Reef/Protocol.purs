-- | Reef.Protocol — the wire format for pushing a virtual-module record from a
-- | frontend (Triggerfish, later Calypso) to the BEAM engine that runs it.
-- |
-- | One codec, two runtimes. `encodeOdonus`/`decodeOdonus` are defined ONCE here
-- | and compiled to BOTH the JS frontend (which encodes) and purerl (the BEAM,
-- | which decodes) — so the protocol has the same structural-parity guarantee as
-- | the engine: there is no hand-rolled Erlang decoder to drift from the encoder.
-- |
-- | Codec = simple-json (`ReadForeign`/`WriteForeign`). It's the only JSON codec
-- | in the purerl package set — the project's usual symmetric `codec-argonaut`
-- | isn't available there. simple-json is symmetric in practice: records derive
-- | both classes from one structure; the single hand-written instance below
-- | (`Distribution`) is a matched read/write pair.
-- |
-- | NB this is the WIRE format (transient, machine-to-machine). The on-disk
-- | persistence format is valid Tidal source text (paste-able into Calypso/REPLs,
-- | buildable without parsing) — a different job, deliberately not JSON.
-- |
-- | The whole `Odonus` record travels, runtime fields included: the BEAM voice
-- | simply continues stepping from the frontend's current cursor state, so no
-- | Config/Runtime split is needed yet.
module Reef.Protocol
  ( encodeOdonus
  , decodeOdonus
  ) where

import Data.Either (Either)
import Foreign (MultipleErrors)
import Reef.Odonus (Odonus)
import Simple.JSON (readJSON, writeJSON)

-- The WriteForeign/ReadForeign instance for `Distribution` (the one non-primitive
-- field in Odonus) lives in Reef.Scale beside the type, to stay non-orphan. Every
-- other field is a record/array of primitives that simple-json derives.

-- | Serialize a complete Odonus record to a JSON string for the wire.
encodeOdonus :: Odonus -> String
encodeOdonus = writeJSON

-- | Parse a JSON string from the wire back into an Odonus record.
decodeOdonus :: String -> Either MultipleErrors Odonus
decodeOdonus = readJSON
