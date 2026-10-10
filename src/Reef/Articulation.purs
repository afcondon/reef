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
-- | **A line is one mono voice.** A plain note on a MIDI line goes out as a
-- | note-on, and the voice remembers when it ends (`sounding`); its note-off
-- | is sent by the step that end falls in, unless the voice's next note comes
-- | first. Then the next note decides: the same pitch is cut and struck again
-- | (a retrigger), a different pitch starts before the old one is let go (the
-- | legato a GATE over 100% asks for, without portamento). A fixed-length
-- | note would end on its own clock instead, and with GATE over 100% its
-- | note-off landed after the next note-on: a repeated pitch was cut 41 ms in,
-- | and Yarns fell back to stale held notes (AC, 2026-10-10, "wildly wrong").
module Reef.Articulation
  ( Played
  , StepIn
  , Sounding
  , playStep
  , releaseAll
  , plainSends
  , newlyMuted
  , polyStates
  , slideMs
  , gateVolts
  , normalise
  , nominalVolts
  ) where

import Prelude

import Data.Array (catMaybes, concat, concatMap, drop, elem, take, filter, foldl, groupAllBy, head, length, mapWithIndex, null, range, unsnoc, updateAt, zipWith, (!!))
import Data.Array.NonEmpty as NEA
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Tuple (Tuple(..))
import Reef.Calibration (realiseNote)
import Reef.Odonus (Fired, Odonus)
import Reef.Rample as Rample
import Reef.Render (gateMs, subOffsetMs)
import Reef.Routing (CvLine, Es9Poly, Leg, Sampler, Send(..), VoiceRouting)
import Reef.Voices as RV

-- | What a step played, what each voice holds after it, and each
-- | polyphonic instrument's allocator (`polys` then `samplers`, in the
-- | routing's order).
type Played =
  { sends :: Array Send
  , held :: Array (Maybe Int)
  , sounding :: Array (Maybe Sounding)
  , polys :: Array RV.Voices
  }

-- | A plain note a MIDI line is playing, and when it ends, in `nowMs`'s
-- | time: its note-off is still to be sent.
type Sounding = { pitch :: Int, untilMs :: Number }

-- | One model step: the model after it (for note lengths), the step's length
-- | in ms, what each voice held before it, the voices just muted (their held
-- | notes are let go first), and the notes fired, each with its velocity (the
-- | page adds accent and humanise; the rig plays the cell's own).
-- | `nowMs` is the step's onset on any steady clock (the rig's wall time, the
-- | page's): an allocator remembers when its notes end, across steps, so it
-- | works in absolute time, and its decisions come back relative to the step.
-- | `polys` are the allocators as the last step left them (`polyStates` to
-- | start, or when the routing's instruments change).
type StepIn =
  { odo :: Odonus
  , stepMs :: Number
  , nowMs :: Number
  , polys :: Array RV.Voices
  , held :: Array (Maybe Int)
  , sounding :: Array (Maybe Sounding)
  , newlyMuted :: Array Int
  , notes :: Array { fired :: Fired, velocity :: Int }
  }

-- | Every send of one step, in time order within each voice, `atMs` from the
-- | step's onset; and each voice's held note after it.
playStep :: VoiceRouting -> StepIn -> Played
playStep routing s =
  let
    voiced = retire (foldl note released s.notes)
    timed = map (\n -> { headIdx: n.fired.headIdx, pitch: n.fired.pitch, velocity: n.velocity
                        , atMs: subOffsetMs s.stepMs n.fired, gateMs: gateMs s.odo s.stepMs n.fired }) s.notes
    poly = playPolys routing s.nowMs timed
      (if length s.polys == length routing.polys + length routing.samplers then s.polys else polyStates routing)
  in
    { sends: voiced.sends <> poly.sends, held: voiced.held, sounding: voiced.sounding, polys: poly.states }
  where
  released = foldl
    (\acc v -> { sends: acc.sends <> releaseVoice routing v (heldOrSounding acc.held acc.sounding v) 0.0
               , held: fromMaybe acc.held (updateAt v Nothing acc.held)
               , sounding: fromMaybe acc.sounding (updateAt v Nothing acc.sounding) })
    { sends: [], held: s.held, sounding: s.sounding }
    s.newlyMuted
  note acc n =
    let
      f = n.fired
      v = f.headIdx
      prev = join (acc.held !! v)
      at = subOffsetMs s.stepMs f
      gate = gateMs s.odo s.stepMs f
      -- the voice's plain note: ended before this one (its note-off is due
      -- now), or still sounding under it (this note cuts it)
      { ended, cut } = case join (acc.sounding !! v) of
        Just so
          | so.untilMs <= s.nowMs + at -> { ended: noteOffs routing v so.pitch (so.untilMs - s.nowMs), cut: Nothing }
          | otherwise -> { ended: [], cut: Just so.pitch }
        Nothing -> { ended: [], cut: Nothing }
      one = { pitch: f.pitch, velocity: n.velocity, glide: f.glide, ratchet: f.ratchet
            , gateMs: gate, atMs: at, prev, cut, mono: true }
      next = if f.glide then Just f.pitch else Nothing
      sounds = if f.glide then Nothing else Just { pitch: f.pitch, untilMs: s.nowMs + noteEnd one }
    in
      { sends: acc.sends <> ended <> voiceSends routing v one
      , held: fromMaybe acc.held (updateAt v next acc.held)
      , sounding: fromMaybe acc.sounding (updateAt v sounds acc.sounding) }
  -- a plain note that ends before the next step: its note-off now
  retire acc =
    foldl
      (\a (Tuple v so) -> case so of
          Just x | x.untilMs < s.nowMs + s.stepMs ->
            { sends: a.sends <> noteOffs routing v x.pitch (max 0.0 (x.untilMs - s.nowMs))
            , held: a.held
            , sounding: fromMaybe a.sounding (updateAt v Nothing a.sounding) }
          _ -> a)
      acc
      (mapWithIndex Tuple acc.sounding)

-- | What a voice holds, by a glide or as a plain note still sounding.
heldOrSounding :: Array (Maybe Int) -> Array (Maybe Sounding) -> Int -> Maybe Int
heldOrSounding held sounding v = case join (held !! v) of
  Just q -> Just q
  Nothing -> map _.pitch (join (sounding !! v))

-- | Where a plain note ends, from the step's onset: its gate, or the last
-- | ratchet's, which sounds 85% of its slot.
noteEnd :: Note -> Number
noteEnd n =
  let rat = if n.ratchet < 1 || isJust n.prev then 1 else n.ratchet
  in if rat <= 1 then n.atMs + n.gateMs
     else n.atMs + n.gateMs - 0.15 * n.gateMs / toNumber rat

-- | Let every voice go: what a stop sends, so no held note, open gate or
-- | sounding allocated voice outlives the transport.
releaseAll :: VoiceRouting -> Array (Maybe Int) -> Array (Maybe Sounding) -> Array RV.Voices -> Array Send
releaseAll routing held sounding polys =
  concatMap (\v -> releaseVoice routing v (heldOrSounding held sounding v) 0.0)
    (range 0 (max (length routing.voices) (length routing.lines) - 1))
    <> concat (zipWith (\p v -> polySends p 0.0 0.0 (RV.allOff 0.0 v).emits) routing.polys (take (length routing.polys) polys))

-- | One note played as itself, held into nothing: what a replayed loop sends
-- | for each recorded note (it kept the notes, not their slides). Every leg
-- | and line of voice `v`, as a fresh note.
plainSends :: VoiceRouting -> Int -> { pitch :: Int, velocity :: Int, gateMs :: Number, atMs :: Number } -> Array Send
plainSends routing v n =
  voiceSends routing v { pitch: n.pitch, velocity: n.velocity, glide: false, ratchet: 1
                       , gateMs: n.gateMs, atMs: n.atMs, prev: Nothing, cut: Nothing, mono: false }

-- | The heads muted between two states of the model: what was playing and is
-- | now silent, whose held notes must be let go.
newlyMuted :: Odonus -> Odonus -> Array Int
newlyMuted before after =
  catMaybes (mapWithIndex (\i h -> if h.mute && not (wasMuted i) then Just i else Nothing) after.heads)
  where
  wasMuted i = maybe true _.mute (before.heads !! i)

type Note =
  { pitch :: Int, velocity :: Int, glide :: Boolean, ratchet :: Int
  , gateMs :: Number, atMs :: Number, prev :: Maybe Int
  -- the voice's plain note still sounding under this one (a line cuts it)
  , cut :: Maybe Int
  -- a line plays its plain notes as held notes, ended by the machine; a
  -- replayed loop (`plainSends`) plays them at fixed length
  , mono :: Boolean
  }

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

-- | A plain note's end on a voice's MIDI lines. Its ES-9 lines need nothing:
-- | their gate was a timed pulse.
noteOffs :: VoiceRouting -> Int -> Int -> Number -> Array Send
noteOffs routing v q at =
  concatMap off (fromMaybe [] (routing.voices !! v))
  where
  off leg
    | leg.line && null leg.rample = [ NoteOff { port: leg.port, channel: leg.channel, note: q, atMs: at + leg.offsetMs } ]
    | otherwise = []

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
  -- a tie: the held note carries on; a plain one is now sounding, and the
  -- machine ends it (or, off a line, it ends here)
  Just q | q == n.pitch ->
    if n.glide || n.mono then [] else [ NoteOff { port, channel, note: q, atMs: t + n.gateMs } ]
  -- a slide: the new note starts before the held one ends, with portamento on
  Just q ->
    [ porta 127 ]
      <> (if n.glide || n.mono then [ on t ]
          else [ Note { port, channel, note: n.pitch, velocity: n.velocity, atMs: t, durMs: n.gateMs } ])
      <> [ NoteOff { port, channel, note: q, atMs: t + overlap } ]
  -- a fresh note: portamento off; held into the next if it glides
  Nothing -> case n.cut of
    -- the same pitch still sounding: let it go, then strike again
    Just c | c == n.pitch -> [ porta 0, NoteOff { port, channel, note: c, atMs: t } ] <> fresh
    -- another pitch still sounding: the new one first, legato, then let go
    Just c -> [ porta 0 ] <> fresh <> [ NoteOff { port, channel, note: c, atMs: t } ]
    Nothing -> [ porta 0 ] <> fresh
  where
  on at = NoteOn { port, channel, note: n.pitch, velocity: n.velocity, atMs: at }
  fresh
    | n.glide = [ on t ]
    | otherwise =
        let hits = ratchets n \at dur -> Note { port, channel, note: n.pitch, velocity: n.velocity, atMs: at, durMs: dur }
        in if not n.mono then hits
           -- the last hit is held, and the machine ends it
           else case unsnoc hits of
             Just { init, last: Note l } -> init <> [ on l.atMs ]
             _ -> hits
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

-- ---------------------------------------------------------------------------
-- Instruments that allocate across voices
-- ---------------------------------------------------------------------------

-- | Each instrument's allocator, empty: the ES-9 instruments then the
-- | samplers, as the routing lists them.
polyStates :: VoiceRouting -> Array RV.Voices
polyStates routing =
  map (\p -> RV.empty (seated p (profileOf p.instrument))) routing.polys
    <> map (const (RV.empty RV.rample)) routing.samplers

-- | The allocator's profile for an instrument named in the routing.
profileOf :: String -> RV.Instrument
profileOf = case _ of
  "rings" -> RV.rings
  _ -> RV.saich

-- | How the instrument seats its notes: by pitch when the routing asks, which
-- | an instrument that allocates for itself ignores (it has one bus, and no
-- | seating to order).
seated :: Es9Poly -> RV.Instrument -> RV.Instrument
seated p inst = case inst.silencing of
  RV.SelfAllocating _ -> inst
  _ -> inst { order = if p.byPitch then RV.ByPitch else RV.Arrival }

type Timed = { headIdx :: Int, pitch :: Int, velocity :: Int, atMs :: Number, gateMs :: Number }

-- | One step through every allocator. Elapsed notes are retired first, at the
-- | step's onset, so a note arriving now can take a voice that just freed;
-- | then the step's notes in groups by their time in it (a fast head's two
-- | notes are two chords, not one), each instrument taking the heads routed
-- | to it.
playPolys :: VoiceRouting -> Number -> Array Timed -> Array RV.Voices -> { sends :: Array Send, states :: Array RV.Voices }
playPolys routing nowMs notes states =
  let
    -- equal lengths on both sides: the BEAM's zipWith refuses unequal ones
    es9 = zipWith (\p v -> es9Poly p (v { inst = seated p v.inst })) routing.polys (take (length routing.polys) states)
    smp = zipWith sampler routing.samplers (drop (length routing.polys) states)
    all = es9 <> smp
  in
    { sends: concatMap _.sends all, states: map _.state all }
  where
  groups = map (\g -> { at: (NEA.head g).atMs, notes: NEA.toArray g }) (groupAllBy (comparing _.atMs) notes)

  es9Poly p v0 =
    let
      start = RV.expireAt nowMs v0
      step acc g =
        let
          ex = RV.expireAt (nowMs + g.at) acc.v
          mine = filter (\n -> elem n.headIdx p.heads) g.notes
          on = foldl (\a n -> let r = RV.noteOn (nowMs + g.at) n.pitch n.gateMs a.v in { v: r.voices, sends: a.sends <> polySends p nowMs (nowMs + g.at) r.emits })
                 { v: ex.voices, sends: polySends p nowMs (nowMs + g.at) ex.emits } mine
        in { v: on.v, sends: acc.sends <> on.sends }
      done = foldl step { v: start.voices, sends: polySends p nowMs nowMs start.emits } groups
    in { sends: done.sends, state: done.v }

  sampler smp v0 =
    let
      layer = { velocity: Nothing, slots: smp.slots, pitchOfSlot0: Just smp.pitchOfSlot0, slotPitches: Nothing }
      settle = case RV.rample.silencing of
        RV.PerVoiceStrike ps -> ps.settleMs
        _ -> 40.0
      step acc g =
        let
          -- a pitch the card does not hold is refused before it can take a voice
          mine = filter (\n -> elem n.headIdx smp.heads && isJust (Rample.slotFor layer n.pitch)) g.notes
          start = nowMs + g.at - settle
          ex = RV.expireAt start acc.v
          on = foldl (\a n -> let r = RV.noteOn start n.pitch n.gateMs a.v in { v: r.voices, sends: a.sends <> samplerSends smp layer nowMs n r.emits })
                 { v: ex.voices, sends: [] } mine
        in { v: on.v, sends: acc.sends <> on.sends }
      done = foldl step { v: v0, sends: [] } groups
    in { sends: done.sends, state: done.v }

-- | An ES-9 instrument's decisions as sends, `nowMs` the step's onset and
-- | `t` the moment they were made. A pitch is set, never slewed: its voice is
-- | silent or has just ended, so there is nothing to glide from. The voice
-- | count SLEWS, and that is the note-off envelope: the Saïch's mixer
-- | crossfades between plateaus over the ramp. A self-allocating instrument's
-- | strum is a pulse, timed to the sample by es9-daemon.
polySends :: Es9Poly -> Number -> Number -> Array RV.Emit -> Array Send
polySends p nowMs t = concatMap \e -> case e.action of
  RV.Pitch voice note -> case p.voiceBuses !! voice of
    Just bus -> [ CvSet { bus, value: normalise (voltsFor voice note), atMs: e.atMs - nowMs } ]
    Nothing -> []
  RV.Gate voice on -> case p.gateBuses !! voice of
    Just bus -> [ CvSet { bus, value: normalise (if on then gateVolts else 0.0), atMs: e.atMs - nowMs } ]
    Nothing -> []
  RV.Mix _ volts ->
    [ CvSlew { bus: p.ctrlBus, value: normalise volts, lagSec: max 0.0 (e.atMs - t) / 1000.0, atMs: t - nowMs } ]
  RV.Trigger _ durMs ->
    [ CvPulse { bus: p.ctrlBus, value: normalise gateVolts, durMs, atMs: e.atMs - nowMs } ]
  -- `Es9Poly` has no decay bus yet, so a Rings note is not shaped (as from
  -- the page before; the gap is the routing's, not this module's)
  RV.Decay _ _ -> []
  where
  voltsFor voice note = case join (map head (p.tables !! voice)) of
    Just table -> realiseNote table (toNumber note)
    Nothing -> nominalVolts note

-- | A sampler's decisions as sends: a voice's slice as its start-point
-- | control, then its trigger at the note's velocity, for the note's gate.
samplerSends :: Sampler -> Rample.Layer -> Number -> Timed -> Array RV.Emit -> Array Send
samplerSends smp layer nowMs n = concatMap \e -> case e.action of
  RV.Pitch i pitch -> case Rample.slotFor layer pitch of
    Just slot -> [ Control { port: smp.port, channel: smp.channel, controller: Rample.startCC (i + 1)
                           , value: Rample.ccForSlot slot smp.slots, atMs: e.atMs - nowMs } ]
    Nothing -> []
  RV.Trigger i _ -> case smp.triggers !! i of
    Just note -> [ Note { port: smp.port, channel: smp.channel, note, velocity: n.velocity
                        , atMs: e.atMs - nowMs, durMs: n.gateMs } ]
    Nothing -> []
  _ -> []
