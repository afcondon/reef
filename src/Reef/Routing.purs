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
-- | legs and not its voice.
-- |
-- | **ES-9 lines** (2026-10-10, docs/kb/plans/hardware-through-the-rig.md):
-- | a melodic voice may also drive a pitch CV and a gate on the ES-9, played
-- | by the rig through es9-daemon. What a note sends down each kind, legato
-- | and slides included, is `Reef.Articulation`'s.
module Reef.Routing
  ( RampleLeg
  , Leg
  , CvLine
  , Es9Poly
  , Sampler
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
import Reef.Calibration (Table)
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
-- | - `line`: whether legato means anything here (a synth, the continuo), as
-- |   against a TRIGGER fired once per note (an FH-2 envelope or gate, a
-- |   Rample). An FH-2 envelope resolves to the note it is given, as a synth
-- |   does, so only this says it must not be tied or slid.
-- | - `rample`: at most one. An array, not a `Maybe`, for the reason
-- |   `Reef.Conspicillum.Protocol` gives: null round-trips through V8 and jsx
-- |   in ways that agree until one day they do not.
type Leg =
  { port :: String
  , channel :: Int
  , note :: Int
  , offsetMs :: Number
  , rample :: Array RampleLeg
  , line :: Boolean
  }

-- | A mono pitch line on the ES-9: the note as a calibrated voltage on
-- | `pitchBus`, and a gate on `gateBus` (none when empty), es9-daemon's bus
-- | numbers. `table` is the oscillator's calibration (none: a nominal 1 V/oct
-- | from C2), carried in the routing so the rig needs no lookup of its own.
type CvLine =
  { pitchBus :: Int
  , gateBus :: Array Int
  , table :: Array Table
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
  -- | A note held until its `NoteOff`: legato needs the two apart, since a
  -- | held note's end is not known when it starts.
  | NoteOn { port :: String, channel :: Int, note :: Int, velocity :: Int, atMs :: Number }
  | NoteOff { port :: String, channel :: Int, note :: Int, atMs :: Number }
  -- | An ES-9 bus set to `value` (−1..1 is ±10 V), at once.
  | CvSet { bus :: Int, value :: Number, atMs :: Number }
  -- | An ES-9 bus moved to `value` through es9-daemon's smoother, whose lag
  -- | (a time constant, in seconds) stays on the bus until set again.
  | CvSlew { bus :: Int, value :: Number, lagSec :: Number, atMs :: Number }
  -- | A pulse at `value` for `durMs`, then back to 0, timed by es9-daemon to
  -- | the sample.
  | CvPulse { bus :: Int, value :: Number, durMs :: Number, atMs :: Number }

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

-- | A drum routing as written, by this page or an older one: a leg written
-- | before legs said whether they are lines is a trigger, as every drum is.
decodeDrumRouting :: String -> Either MultipleErrors DrumRouting
decodeDrumRouting s = do
  w :: { notes :: Array Int, lanes :: Array (Array LegWire), voices :: Array (Array VoiceLeg) } <- readJSON s
  pure { notes: w.notes, lanes: map (map (legOf false)) w.lanes, voices: w.voices }

-- | A leg as the wire carries it: `line` may be missing, from a page loaded
-- | before legs said (2026-10-10), so a tab left open writes what the rig can
-- | still read.
type LegWire =
  { port :: String
  , channel :: Int
  , note :: Int
  , offsetMs :: Number
  , rample :: Array RampleLeg
  , line :: Maybe Boolean
  }

legOf :: Boolean -> LegWire -> Leg
legOf dflt w = { port: w.port, channel: w.channel, note: w.note, offsetMs: w.offsetMs, rample: w.rample
               , line: fromMaybe dflt w.line }

-- | A polyphonic instrument on the ES-9, its voices chosen by an allocator
-- | (`Reef.Voices`) shared by every head routed to it: `instrument` names the
-- | profile (`saich`, `rings`), `heads` the voices that feed it, `byPitch`
-- | the seating, then where its voices and its one control jack are, as
-- | es9-daemon buses, and each voice's calibration (none: nominal).
type Es9Poly =
  { instrument :: String
  , heads :: Array Int
  , byPitch :: Boolean
  , voiceBuses :: Array Int
  , gateBuses :: Array Int
  , ctrlBus :: Int
  , tables :: Array (Array Table)
  }

-- | A sampler whose voices are chosen by an allocator: the Rample played as
-- | one instrument, each note a slice (`pitchOfSlot0` up, `slots` of them)
-- | struck on whichever of its voices is free, `triggers` being each voice's
-- | trigger note on `port`/`channel`.
type Sampler =
  { heads :: Array Int
  , port :: String
  , channel :: Int
  , triggers :: Array Int
  , slots :: Int
  , pitchOfSlot0 :: Int
  }

-- | **A melodic machine's voices** (Odonus's four heads; Vetula's sixteen
-- | cards), each with its live MIDI legs, resolved as the drum lanes' are, and
-- | its ES-9 lines. A voice is a stream: every leg carries the note it is
-- | given (or its own, for a gate or a Rample trigger), so there is no lane to
-- | find, only the voice's index.
-- | Beside them, the instruments that allocate across voices: `polys` on the
-- | ES-9 and `samplers` over MIDI.
type VoiceRouting =
  { voices :: Array (Array Leg)
  , lines :: Array (Array CvLine)
  , polys :: Array Es9Poly
  , samplers :: Array Sampler
  }

-- | What one note of voice `i` sends, down every leg of that voice. A voice
-- | with no legs (or past the table) sends nothing.
voiceRoutingSends :: VoiceRouting -> Int -> Hit -> Array Send
voiceRoutingSends routing i hit = concatMap (\leg -> legSends leg hit) (fromMaybe [] (routing.voices !! i))

encodeVoiceRouting :: VoiceRouting -> String
encodeVoiceRouting = writeJSON

-- | A voice routing as written, by this page or an older one: a leg without
-- | `line` is a line (a melodic voice's legs were notes), and a routing
-- | without ES-9 lines or instruments has none.
decodeVoiceRouting :: String -> Either MultipleErrors VoiceRouting
decodeVoiceRouting s = do
  w :: { voices :: Array (Array LegWire), lines :: Maybe (Array (Array CvLine))
       , polys :: Maybe (Array Es9Poly), samplers :: Maybe (Array Sampler) } <- readJSON s
  pure
    { voices: map (map (legOf true)) w.voices
    , lines: fromMaybe [] w.lines
    , polys: fromMaybe [] w.polys
    , samplers: fromMaybe [] w.samplers
    }
