-- | **How a melodic voice's notes are played**, down every kind of leg: what
-- | one step of Odonus sends, ties, slides and ratchets included.
-- |
-- | The rig plays this (`reef_voice`), and the page in Solo, so there is one
-- | articulation rather than a page-only one the rig lacked: until 2026-10-10
-- | the rig could not glide at all, and only the page could drive an ES-9 line
-- | (docs/kb/plans/hardware-through-the-rig.md). Everything here is a pure
-- | function of the routing, the step and each voice's HELD note, so
-- | `Reef.Conformance.articulationRun` holds the two runtimes to one answer.
-- |
-- | **The held note.** A GLIDE cell holds its note into the next one, as on a
-- | 303. So the voice remembers the note it is holding (`held`), and the next
-- | note it plays arrives as a slide:
-- |
-- |   * on a MIDI LINE (a synth): the new note starts before the held one
-- |     ends, with portamento on (CC 65), so a mono synth glides without a new
-- |     gate (Yarns's LG = AUTO); a tie (the same note) just carries on;
-- |   * on an ES-9 LINE: the pitch slews to the new note (`slideMs`) and the
-- |     gate, already high, stays high: no edge, no retrigger;
-- |   * on a TRIGGER (an FH-2 envelope or gate) or a Rample: nothing is held,
-- |     since there is no pitch to slide along; each note is a strike.
-- |
-- | A plain note steps and gates as itself, so consecutive plain notes
-- | retrigger.
module Reef.Articulation
  ( Played
  , StepIn
  , playStep
  , releaseAll
  , slideMs
  , gateVolts
  , normalise
  , nominalVolts
  ) where

import Prelude

import Data.Array (concatMap, foldl, head, length, null, range, updateAt, (!!))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Reef.Calibration (realiseNote)
import Reef.Odonus (Fired, Odonus)
import Reef.Rample as Rample
import Reef.Render (gateMs, subOffsetMs)
import Reef.Routing (CvLine, Leg, Send(..), VoiceRouting)

-- | What a step played, and what each voice holds after it.
type Played = { sends :: Array Send, held :: Array (Maybe Int) }

-- | One model step: the model after it (for note lengths), the step's length
-- | in ms, what each voice held before it, the voices just muted (their held
-- | notes are let go first), and the notes fired, each with its velocity (the
-- | page adds accent and humanise; the rig plays the cell's own).
type StepIn =
  { odo :: Odonus
  , stepMs :: Number
  , held :: Array (Maybe Int)
  , newlyMuted :: Array Int
  , notes :: Array { fired :: Fired, velocity :: Int }
  }

-- | Every send of one step, in time order within each voice, `atMs` from the
-- | step's onset; and each voice's held note after it.
playStep :: VoiceRouting -> StepIn -> Played
playStep routing s =
  foldl note released s.notes
  where
  released = foldl
    (\acc v -> { sends: acc.sends <> releaseVoice routing v (join (acc.held !! v)) 0.0
               , held: fromMaybe acc.held (updateAt v Nothing acc.held) })
    { sends: [], held: s.held }
    s.newlyMuted
  note acc n =
    let
      f = n.fired
      v = f.headIdx
      prev = join (acc.held !! v)
      at = subOffsetMs s.stepMs f
      gate = gateMs s.odo s.stepMs f
      one = { pitch: f.pitch, velocity: n.velocity, glide: f.glide, ratchet: f.ratchet
            , gateMs: gate, atMs: at, prev }
      next = if f.glide then Just f.pitch else Nothing
    in
      { sends: acc.sends <> voiceSends routing v one
      , held: fromMaybe acc.held (updateAt v next acc.held) }

-- | Let every voice go: what a stop sends, so no held note or open gate
-- | outlives the transport.
releaseAll :: VoiceRouting -> Array (Maybe Int) -> Array Send
releaseAll routing held =
  concatMap (\v -> releaseVoice routing v (join (held !! v)) 0.0)
    (range 0 (max (length routing.voices) (length routing.lines) - 1))

type Note =
  { pitch :: Int, velocity :: Int, glide :: Boolean, ratchet :: Int
  , gateMs :: Number, atMs :: Number, prev :: Maybe Int }

voiceSends :: VoiceRouting -> Int -> Note -> Array Send
voiceSends routing v n =
  concatMap (legSends n) (fromMaybe [] (routing.voices !! v))
    <> concatMap (lineSends n) (fromMaybe [] (routing.lines !! v))

-- | A voice's held note let go: its note-off on every MIDI line, and its gate
-- | closed on every ES-9 line.
releaseVoice :: VoiceRouting -> Int -> Maybe Int -> Number -> Array Send
releaseVoice routing v held at =
  concatMap midiOff (fromMaybe [] (routing.voices !! v))
    <> concatMap gateOff (fromMaybe [] (routing.lines !! v))
  where
  midiOff leg = case held of
    Just q | leg.line && null leg.rample -> [ NoteOff { port: leg.port, channel: leg.channel, note: q, atMs: at + leg.offsetMs } ]
    _ -> []
  gateOff line = map (\bus -> CvSet { bus, value: 0.0, atMs: at }) line.gateBus

-- ---------------------------------------------------------------------------
-- MIDI legs
-- ---------------------------------------------------------------------------

legSends :: Note -> Leg -> Array Send
legSends n leg = case head leg.rample of
  Just r -> rampleSends n leg r
  Nothing
    | leg.line -> lineLegSends n leg
    | otherwise -> triggerSends n leg

-- | A TRIGGER (an FH-2 envelope or gate): fired once, at the note's velocity
-- | and for its gate, so a sustaining envelope follows the gate. A ratchet is
-- | not subdivided: retriggering an envelope per ratchet is a different
-- | musical decision from the one the grid recorded.
triggerSends :: Note -> Leg -> Array Send
triggerSends n leg =
  [ Note { port: leg.port, channel: leg.channel, note: if leg.note < 0 then n.pitch else leg.note
         , velocity: n.velocity, atMs: n.atMs + leg.offsetMs, durMs: n.gateMs } ]

-- | A Rample voice: the pitch is a slice, sent as a start-point control a
-- | settle ahead of the trigger. A tie keeps the slice ringing. A pitch the
-- | card does not hold is dropped, not transposed. Ratchets ARE subdivided:
-- | retriggering a sample is exactly what a ratchet means on a sampler.
rampleSends :: Note -> Leg -> { voice :: Int, slots :: Int, pitchOfSlot0 :: Int, settleMs :: Int } -> Array Send
rampleSends n leg r
  | n.glide && n.prev == Just n.pitch = []
  | otherwise =
      case Rample.slotFor { velocity: Nothing, slots: r.slots, pitchOfSlot0: Just r.pitchOfSlot0, slotPitches: Nothing } n.pitch of
        Nothing -> []
        Just slot ->
          [ Control { port: leg.port, channel: leg.channel, controller: Rample.startCC r.voice
                    , value: Rample.ccForSlot slot r.slots, atMs: t - toNumber r.settleMs } ]
            <> ratchets n \at dur -> Note { port: leg.port, channel: leg.channel, note: leg.note, velocity: n.velocity, atMs: at, durMs: dur }
  where
  t = n.atMs + leg.offsetMs

-- | A LINE (a synth, the continuo): the legato machine.
lineLegSends :: Note -> Leg -> Array Send
lineLegSends n leg = case n.prev of
  -- a tie: the held note carries on, and ends here if this note does not hold
  Just q | q == n.pitch ->
    if n.glide then [] else [ NoteOff { port, channel, note: q, atMs: t + n.gateMs } ]
  -- a slide: the new note starts before the held one ends, with portamento on
  Just q ->
    [ porta 127 ]
      <> (if n.glide then [ NoteOn { port, channel, note: n.pitch, velocity: n.velocity, atMs: t } ]
          else [ Note { port, channel, note: n.pitch, velocity: n.velocity, atMs: t, durMs: n.gateMs } ])
      <> [ NoteOff { port, channel, note: q, atMs: t + overlap } ]
  -- a fresh note: portamento off; held into the next if it glides
  Nothing ->
    [ porta 0 ]
      <> (if n.glide then [ NoteOn { port, channel, note: n.pitch, velocity: n.velocity, atMs: t } ]
          else ratchets n \at dur -> Note { port, channel, note: n.pitch, velocity: n.velocity, atMs: at, durMs: dur })
  where
  port = leg.port
  channel = leg.channel
  t = n.atMs + leg.offsetMs
  -- Portamento on or off, never its time: the glide time is the synth's own
  -- setting (a fixed CC 5 overwrote Yarns's PO on every slide).
  porta value = Control { port, channel, controller: 65, value, atMs: t }
  -- the overlap that makes it a slide, kept inside a short note's gate, or a
  -- mono synth would fall back to the held pitch when the new note ended
  overlap = if n.glide then slideMs else min slideMs (n.gateMs / 2.0)

-- | A note's hits: one for its gate, or `ratchet` evenly spaced in it, each
-- | sounding 85% of its slot. A glide is one sustained event, so it is never
-- | ratcheted.
ratchets :: Note -> (Number -> Number -> Send) -> Array Send
ratchets n mk =
  let rat = if n.ratchet < 1 || n.glide then 1 else n.ratchet
  in
    if rat <= 1 then [ mk n.atMs n.gateMs ]
    else
      let sub = n.gateMs / toNumber rat
      in map (\k -> mk (n.atMs + toNumber k * sub) (sub * 0.85)) (range 0 (rat - 1))

-- ---------------------------------------------------------------------------
-- ES-9 lines
-- ---------------------------------------------------------------------------

-- | The slide's length. A 303 slides in a fixed time, about 60 ms, whatever
-- | the tempo, and that constancy is part of its sound. Also the overlap of a
-- | MIDI slide.
slideMs :: Number
slideMs = 60.0

-- | es9-daemon's slew is a first-order smoother, so its lag is a TIME
-- | CONSTANT: a lag of 60 ms covers only 63% of the interval in 60 ms. A third
-- | of the slide arrives within 5% of the note in `slideMs`.
slideLagSec :: Number
slideLagSec = slideMs / 3.0 / 1000.0

-- | And the lag is STICKY per bus: a set leaves it as it was, so after one
-- | slide every later note would glide too. A plain note therefore goes out
-- | as a slew at es9-daemon's own default (`DEFAULT_LAG_SEC`).
stepLagSec :: Number
stepLagSec = 0.005

-- | How long the pitch is given to settle before the gate rises, so the
-- | envelope never opens on the previous note's pitch.
settleMs :: Number
settleMs = 2.0

-- | A gate's level.
gateVolts :: Number
gateVolts = 5.0

-- | es9-daemon takes −1.0..+1.0 as ±10 V.
normalise :: Number -> Number
normalise volts = volts / 10.0

-- | An uncalibrated oscillator: a straight 1 V/oct from C2 = 0 V, as the
-- | sweeps assume.
nominalVolts :: Int -> Number
nominalVolts note = (toNumber note - 36.0) / 12.0

-- | A note on an ES-9 line: the pitch, through the oscillator's calibration;
-- | then the gate, held through a slide.
lineSends :: Note -> CvLine -> Array Send
lineSends n line = pitch <> gates
  where
  value = normalise (maybe (nominalVolts n.pitch) (\t -> realiseNote t (toNumber n.pitch)) (head line.table))
  high = normalise gateVolts
  pitch = case n.prev of
    Just q
      | q /= n.pitch -> [ CvSlew { bus: line.pitchBus, value, lagSec: slideLagSec, atMs: n.atMs } ]
      | otherwise -> []
    Nothing -> [ CvSlew { bus: line.pitchBus, value, lagSec: stepLagSec, atMs: n.atMs } ]
  gates = map gate line.gateBus
  gate bus
    -- held into the next note; that note (or a release) ends it
    | n.glide = CvSet { bus, value: high, atMs: n.atMs }
    | otherwise = case n.prev of
        -- already high from the held note: on to this note's end, no new edge
        Just _ -> CvPulse { bus, value: high, durMs: n.gateMs, atMs: n.atMs }
        -- a fresh note: pitch first, then the gate
        Nothing -> CvPulse { bus, value: high, durMs: n.gateMs, atMs: n.atMs + settleMs }

