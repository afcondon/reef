-- | `Reef.Vetula.Protocol` — the wire format for the Vetula performance handoff: the
-- | frontend encodes the whole `Perf` (progression + voices) to JSON, pushes it to
-- | the rig once, and the BEAM `reef_vetula_voice` decodes it and runs the shared
-- | `Reef.Vetula.Perf` scheduler from exactly that definition. Because a performance
-- | is a pure function of the absolute pulse, this ONE push is the entire cross-
-- | runtime wire — no per-event stream.
-- |
-- | `VDest`/`VRenderer` are closed ADTs, so (as with Balistes' `BInput`) each voice
-- | is projected to a flat `WireVVoice` with the two enums as small ints; everything
-- | else is Int / Array Int / Boolean, which simple-json's generic `writeJSON`/
-- | `readJSON` cover directly on both runtimes (the BEAM defers to `jsx`). An unknown
-- | enum int decodes to a defined default rather than dropping the voice.
module Reef.Vetula.Protocol
  ( WireVVoice
  , WirePerf
  , destToInt
  , destFromInt
  , rendToInt
  , rendFromInt
  , toWirePerf
  , fromWirePerf
  , encodePerf
  , decodePerf
  ) where

import Prelude

import Data.Either (Either)
import Foreign (MultipleErrors)
import Reef.Vetula.Perf (Perf, VChord, VDest(..), VRenderer(..), VVoice)
import Simple.JSON (readJSON, writeJSON)

-- | A voice, wire-flat: the two enums as ints (`dest` 0=midi/1=odo, `renderer`
-- | 0=block/1=arp/2=strum), the rest carried as-is.
type WireVVoice =
  { dest :: Int
  , renderer :: Int
  , channel :: Int
  , durs :: Array Int
  , phase :: Int
  , muted :: Boolean
  }

-- | The whole performance, wire-flat. `VChord` is already a flat record, so the
-- | chords ride through unchanged.
type WirePerf =
  { chords :: Array VChord
  , voices :: Array WireVVoice
  }

destToInt :: VDest -> Int
destToInt = case _ of
  VToMidi -> 0
  VToOdonus -> 1

destFromInt :: Int -> VDest
destFromInt = case _ of
  1 -> VToOdonus
  _ -> VToMidi

rendToInt :: VRenderer -> Int
rendToInt = case _ of
  VBlock -> 0
  VArp -> 1
  VStrummed -> 2

rendFromInt :: Int -> VRenderer
rendFromInt = case _ of
  1 -> VArp
  2 -> VStrummed
  _ -> VBlock

toWireVoice :: VVoice -> WireVVoice
toWireVoice v =
  { dest: destToInt v.dest
  , renderer: rendToInt v.renderer
  , channel: v.channel
  , durs: v.durs
  , phase: v.phase
  , muted: v.muted
  }

fromWireVoice :: WireVVoice -> VVoice
fromWireVoice w =
  { dest: destFromInt w.dest
  , renderer: rendFromInt w.renderer
  , channel: w.channel
  , durs: w.durs
  , phase: w.phase
  , muted: w.muted
  }

toWirePerf :: Perf -> WirePerf
toWirePerf p = { chords: p.chords, voices: map toWireVoice p.voices }

fromWirePerf :: WirePerf -> Perf
fromWirePerf w = { chords: w.chords, voices: map fromWireVoice w.voices }

-- | Encode a performance for the one-shot handoff push.
encodePerf :: Perf -> String
encodePerf = writeJSON <<< toWirePerf

-- | Decode a pushed performance definition back into a `Perf`.
decodePerf :: String -> Either MultipleErrors Perf
decodePerf s = map fromWirePerf (readJSON s)
