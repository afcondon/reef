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
  , encodeInput
  , decodeInput
  , encodeTagged
  , decodeTagged
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..), MultipleErrors)
import Reef.Input (Input, Tagged, WireInput, fromWire, toWire)
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

-- ── the Input protocol (lockstep; see Reef.Input) ────────────────────────────
--
-- An `Input` is a user action as data. On the wire it travels as the flat
-- `WireInput` record (so the codec is simple-json's record instance, same
-- discipline as Odonus above) — `Reef.Input.toWire`/`fromWire` are the bijection.
-- Defined once here, compiled to BOTH the JS frontend (which encodes) and purerl
-- (which decodes), so the input protocol has the same structural-parity guarantee
-- as the engine. Lockstep tags each input with the tick to apply it on.

-- | Serialize one `Input` to a JSON string for the wire.
encodeInput :: Input -> String
encodeInput = writeJSON <<< toWire

-- | Parse a JSON string back into an `Input`. An unknown tag (a `fromWire`
-- | `Nothing`) becomes a decode error rather than a silent drop.
decodeInput :: String -> Either MultipleErrors Input
decodeInput s = do
  w <- readJSON s :: Either MultipleErrors WireInput
  case fromWire w of
    Just i -> Right i
    Nothing -> Left (singleton (ForeignError ("Reef.Protocol: unknown Input tag " <> show w.tag)))

-- | Serialize a tick-tagged input (`{ tick, input }`) to the wire, flattening the
-- | input to its `WireInput` so the whole thing is one simple-json record.
encodeTagged :: Tagged -> String
encodeTagged t = writeJSON { tick: t.tick, input: toWire t.input }

-- | Parse a tick-tagged input back. Unknown input tag → decode error, as above.
decodeTagged :: String -> Either MultipleErrors Tagged
decodeTagged s = do
  r <- readJSON s :: Either MultipleErrors { tick :: Int, input :: WireInput }
  case fromWire r.input of
    Just i -> Right { tick: r.tick, input: i }
    Nothing -> Left (singleton (ForeignError ("Reef.Protocol: unknown Input tag " <> show r.input.tag)))
