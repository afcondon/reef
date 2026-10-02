-- | **Where a hit goes**: the routing table as both runtimes read it.
-- |
-- | Triggerfish's routing table (`Triggerfish.Routing.Model`) says where each
-- | source fans out to: a set of legs, each a destination with a trim. Until
-- | now only the browser honoured it; the rig played every drum hit on the
-- | FH-2, channel 10, whatever the table said. So Solo and Atlantis sounded
-- | different, and nothing said so.
-- |
-- | This module is the part of the table the rig needs, and the one function
-- | that turns a hit into what is sent. The browser resolves its table into a
-- | `DrumRouting` (it knows which ports exist) and pushes it; the rig keeps it;
-- | both call `drumSends` for every hit. `Reef.Conformance.routingRun` holds the
-- | two runtimes to the same answer.
-- |
-- | **Two kinds of leg.** A MIDI leg is a message on a named port, which is
-- | what the browser's `Wire` found too: an FH-2 gate is a note on the FH-2
-- | whose pitch selects the jack, an FH-2 envelope is a note on channel =
-- | envelope. A **voice** leg plays a sample through SuperDirt: rig only,
-- | since a browser cannot send OSC, so in Local mode a lane plays its MIDI
-- | legs and not its voice. The ES-9 kinds are not here; nothing sends them.
module Reef.Routing
  ( RampleLeg
  , Leg
  , VoiceLeg
  , DrumRouting
  , Hit
  , Send(..)
  , legSends
  , drumSends
  , encodeDrumRouting
  , decodeDrumRouting
  , VoiceRouting
  , voiceRoutingSends
  , encodeVoiceRouting
  , decodeVoiceRouting
  ) where

import Prelude

import Data.Array (concatMap, filter, findIndex, head, null, range, (!!))
import Data.Either (Either)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign (MultipleErrors)
import Reef.Rample as Rample
import Simple.JSON (readJSON, writeJSON)

-- | A Rample voice playing a sliced card: the pitch is which slice, sent as
-- | the voice's start-point control `settleMs` ahead of the trigger note.
type RampleLeg =
  { voice :: Int
  , slots :: Int
  , pitchOfSlot0 :: Int
  , settleMs :: Int
  }

-- | One live leg, resolved.
-- |
-- | - `port`: the whole port name. The rig matches names exactly, so the
-- |   browser, which matches the table's needles by substring, sends the name
-- |   it would itself have sent to.
-- | - `channel`: 1..16.
-- | - `note`: what the leg sends, or below 0 for the hit's own note. An FH-2
-- |   gate sends its selector; a Rample, its trigger.
-- | - `offsetMs`: the per-leg trim, so a doubled kick does not flam.
-- | - `rample`: at most one. An array, not a `Maybe`, for the reason
-- |   `Reef.Conspicillum.Protocol` gives: null round-trips through V8 and jsx
-- |   in ways that agree until one day they do not.
type Leg =
  { port :: String
  , channel :: Int
  , note :: Int
  , offsetMs :: Number
  , rample :: Array RampleLeg
  }

-- | A sample voice, as one leg of a lane: a sample of a SuperDirt bank (`s` is
-- | a Quadrat set, `n` the sample in it), the splice window `begin`..`end` in
-- | [0,1], `speed` (negative plays the window backwards), `gain`, the SuperDirt
-- | `orbit`, and `chop`: 1 plays the window whole, N plays it as N slices in
-- | order across the step, the way a ratchet spreads a hit.
type VoiceLeg =
  { s :: String
  , n :: Int
  , begin :: Number
  , end :: Number
  , speed :: Number
  , gain :: Number
  , orbit :: Int
  , chop :: Int
  , offsetMs :: Number
  }

-- | The drum lanes, in kit order: the note that names each lane, the lane's
-- | live MIDI legs, and its voice legs (muted ones are left out by the browser
-- | before it pushes).
type DrumRouting =
  { notes :: Array Int
  , lanes :: Array (Array Leg)
  , voices :: Array (Array VoiceLeg)
  }

-- | One drum hit, as the engines render it. `atMs` is relative: the browser
-- | adds it to now, the rig to the step's wall time. `stepMs` is the length
-- | of a step, which a chopped voice spreads its slices across.
type Hit =
  { note :: Int
  , velocity :: Int
  , atMs :: Number
  , durMs :: Number
  , stepMs :: Number
  }

data Send
  = Note { port :: String, channel :: Int, note :: Int, velocity :: Int, atMs :: Number, durMs :: Number }
  | Control { port :: String, channel :: Int, controller :: Int, value :: Int, atMs :: Number }
  -- | A `/dirt/play`. `amp` carries the velocity, linearly: SuperDirt's `gain`
  -- | goes through a fourth power, and matching that would take a `pow`, which
  -- | JS and the BEAM do not agree on to the last bit.
  | Play { s :: String, n :: Int, begin :: Number, end :: Number, speed :: Number, gain :: Number, amp :: Number, orbit :: Int, atMs :: Number }

derive instance eqSend :: Eq Send

-- | What one hit sends down one leg. A Rample that does not hold the pitch
-- | sends nothing: a silently transposed note is harder to notice than a
-- | missing one.
legSends :: Leg -> Hit -> Array Send
legSends leg hit = case head leg.rample of
  Nothing ->
    [ Note { port: leg.port, channel: leg.channel, note, velocity: hit.velocity, atMs, durMs: hit.durMs } ]
  Just r ->
    case Rample.slotFor { velocity: Nothing, slots: r.slots, pitchOfSlot0: Just r.pitchOfSlot0, slotPitches: Nothing } hit.note of
      Nothing -> []
      Just slot ->
        [ Control
            { port: leg.port, channel: leg.channel, controller: Rample.startCC r.voice
            , value: Rample.ccForSlot slot r.slots, atMs: atMs - toNumber r.settleMs
            }
        , Note { port: leg.port, channel: leg.channel, note, velocity: hit.velocity, atMs, durMs: hit.durMs }
        ]
  where
  atMs = hit.atMs + leg.offsetMs
  note = if leg.note < 0 then hit.note else leg.note

-- | What one hit plays on one voice leg: the window whole, or `chop` slices of
-- | it, in order, spread evenly across the step.
voiceSends :: VoiceLeg -> Hit -> Array Send
voiceSends v hit = map slice (range 0 (pieces - 1))
  where
  pieces = max 1 v.chop
  width = (v.end - v.begin) / toNumber pieces
  amp = defaultAmp * toNumber hit.velocity / 127.0
  slice k = Play
    { s: v.s, n: v.n
    , begin: v.begin + width * toNumber k, end: v.begin + width * toNumber (k + 1)
    , speed: v.speed, gain: v.gain, amp, orbit: v.orbit
    , atMs: hit.atMs + v.offsetMs + hit.stepMs * toNumber k / toNumber pieces
    }

-- | SuperDirt's own default `amp`, which a full-velocity hit keeps.
defaultAmp :: Number
defaultAmp = 0.4

-- | What one drum hit sends, down every leg of its lane.
-- |
-- | A note outside the kit (the Tidal rack can name any) has no lane. It
-- | borrows the first lane's legs, but only those that carry the hit's own
-- | note: a leg that sends a note of its own, a gate selector or a Rample
-- | trigger, belongs to that lane's drum, and would sound the kick. A voice is
-- | a sound of its own for the same reason, so it is not borrowed either.
drumSends :: DrumRouting -> Hit -> Array Send
drumSends routing hit = case findIndex (_ == hit.note) routing.notes of
  Just lane ->
    concatMap (\leg -> legSends leg hit) (legsOf lane)
      <> concatMap (\v -> voiceSends v hit) (fromMaybe [] (routing.voices !! lane))
  Nothing -> concatMap (\leg -> legSends leg hit) (filter carriesOwnNote (legsOf 0))
  where
  legsOf lane = fromMaybe [] (routing.lanes !! lane)
  carriesOwnNote leg = leg.note < 0 && null leg.rample

encodeDrumRouting :: DrumRouting -> String
encodeDrumRouting = writeJSON

decodeDrumRouting :: String -> Either MultipleErrors DrumRouting
decodeDrumRouting = readJSON

-- | **A melodic machine's voices** (Odonus's four heads; later Vetula's), each
-- | with its live MIDI legs, resolved as the drum lanes' are. A voice is a
-- | stream: every leg carries the note it is given (or its own, for a gate or
-- | a Rample trigger), so there is no lane to find, only the voice's index.
type VoiceRouting = { voices :: Array (Array Leg) }

-- | What one note of voice `i` sends, down every leg of that voice. A voice
-- | with no legs (or past the table) sends nothing.
voiceRoutingSends :: VoiceRouting -> Int -> Hit -> Array Send
voiceRoutingSends routing i hit = concatMap (\leg -> legSends leg hit) (fromMaybe [] (routing.voices !! i))

encodeVoiceRouting :: VoiceRouting -> String
encodeVoiceRouting = writeJSON

decodeVoiceRouting :: String -> Either MultipleErrors VoiceRouting
decodeVoiceRouting = readJSON
