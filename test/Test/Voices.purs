-- | Tests for voice allocation onto a one-output, one-ended-removal instrument.
-- |
-- | The interesting case is the middle release: a note ends while notes above
-- | AND below it are still sounding. The hardware cannot silence a voice in the
-- | middle, so a survivor must migrate down — and how many oscillators that
-- | disturbs is the whole quality question.
module Test.Voices (voicesTests) where

import Prelude

import Data.Array (catMaybes, filter, mapWithIndex)
import Data.Foldable (foldl)
import Effect (Effect)
import Reef.Voices (Action(..), Assign(..), Overflow(..), Release(..), Silencing(..), Voices, check, empty, expireAt, noteOn, saich, sounding)
import Data.Maybe (isJust, isNothing)
import Test.Assert (assertEqual', assertTrue')

-- | Pitches currently held, by voice index, ignoring free voices.
held :: Voices -> Array { at :: Int, pitch :: Int }
held v = catMaybes (mapWithIndex (\i ms -> map (\s -> { at: i, pitch: s.pitch }) ms) v.slots)

pitches :: Array Action -> Array Action
pitches = filter case _ of
  Pitch _ _ -> true
  _ -> false

mixes :: Array Action -> Array Action
mixes = filter case _ of
  Mix _ _ -> true
  _ -> false

voicesTests :: Effect Unit
voicesTests = do
  -- Notes fill from voice 0 upward, and the mix CV tracks the count. Contiguity
  -- is not tidiness: the mixer removes from one end, so a gap would silence the
  -- wrong note.
  let s0 = empty saich
      r1 = noteOn 0.0 60 1000.0 s0
      r2 = noteOn 0.0 64 500.0 r1.voices
      r3 = noteOn 0.0 67 1000.0 r2.voices
  assertEqual' "three notes occupy voices 0,1,2"
    { actual: held r3.voices, expected: [ {at:0,pitch:60}, {at:1,pitch:64}, {at:2,pitch:67} ] }
  assertEqual' "the third note sets the three-voice CV"
    { actual: mixes (map _.action r3.emits), expected: [ Mix 3 2.675 ] }

  -- THE CASE. Release the MIDDLE note of three — voice 1, with voices 0 and 2
  -- still sounding. The hardware cannot silence a voice in the middle, so the
  -- note on voice 2 migrates down to voice 1 and the count drops to two, fading
  -- voice 2 out from under it. (Getting this scenario wrong is easy: give the
  -- TOP note the short duration instead and nothing migrates at all, which is
  -- correct behaviour for a different case.)
  let mid = expireAt 600.0 r3.voices
  assertEqual' "middle release: exactly ONE oscillator migrates"
    { actual: pitches (map _.action mid.emits), expected: [ Pitch 1 67 ] }
  assertEqual' "and the survivors are contiguous from voice 0"
    { actual: held mid.voices, expected: [ {at:0,pitch:60}, {at:1,pitch:67} ] }

  -- Ordering is the artefact-avoidance: repitch NOW, drop the count after the
  -- ramp, so the migrating note is covered by itself rather than by a hole.
  assertEqual' "repitch is emitted before the count change"
    { actual: map _.atMs mid.emits, expected: [ 600.0, 625.0 ] }

  -- The minimal-moves rule earns its keep here. Release voice 0 of four and the
  -- naive shuffle-everyone-down moves three oscillators; taking the TOP survivor
  -- into the hole moves one.
  let f0 = empty saich
      f1 = (noteOn 0.0 60 100.0 f0).voices
      f2 = (noteOn 0.0 62 900.0 f1).voices
      f3 = (noteOn 0.0 64 900.0 f2).voices
      f4 = (noteOn 0.0 65 900.0 f3).voices
      low = expireAt 200.0 f4
  assertEqual' "releasing voice 0 of four moves exactly one oscillator"
    { actual: pitches (map _.action low.emits), expected: [ Pitch 0 65 ] }
  assertEqual' "leaving 62 and 64 untouched where they were"
    { actual: held low.voices, expected: [ {at:0,pitch:65}, {at:1,pitch:62}, {at:2,pitch:64} ] }

  -- A release that costs nothing must cost nothing: dropping the TOP note needs
  -- no migration at all, only a count change.
  let t4 = empty saich
      t1 = (noteOn 0.0 60 900.0 t4).voices
      t2 = (noteOn 0.0 64 900.0 t1).voices
      t3 = (noteOn 0.0 67 100.0 t2).voices
      top = expireAt 200.0 t3
  assertEqual' "releasing the top note migrates nobody"
    { actual: pitches (map _.action top.emits), expected: [] }
  assertEqual' "and just drops to the two-voice CV"
    { actual: mixes (map _.action top.emits), expected: [ Mix 2 1.3 ] }

  -- Overflow is dropped, not stolen. A missing note is a quieter mistake than a
  -- stolen one, and the caller can see it happened.
  let o4 = (noteOn 0.0 71 900.0 f4).voices
  assertEqual' "a fifth simultaneous note is refused, leaving the four intact"
    { actual: sounding o4, expected: 4 }

  -- Nothing expiring emits nothing, so a scheduler can call this every tick.
  let quiet = expireAt 10.0 r3.voices
  assertEqual' "no expiry, no emissions"
    { actual: map _.action quiet.emits, expected: [] }

  -- The decomposition earns its keep by REFUSING incoherent instruments. Each
  -- of these would be a stuck note on stage rather than a stylistic misstep.
  assertTrue' "the measured Saich is coherent" (isNothing (check saich))

  let rr = saich { assign = RoundRobin }
  assertTrue' "round-robin on a one-ended mixer is refused" (isJust (check rr))

  let hole = saich { release = LeaveHole }
  assertTrue' "leaving a hole on a one-ended mixer is refused" (isJust (check hole))

  let short = saich { silencing = CountCV { plateaus: [ 0.0, 1.0, 2.0 ], rampMs: 25.0 } }
  assertTrue' "a plateau table that does not cover every count is refused"
    (isJust (check short))

  -- Per-voice gating permits everything the other cannot, which is the whole
  -- reason the capability is a separate axis from the policies.
  let poly = { voices: 4, silencing: PerVoiceGate, assign: RoundRobin
             , overflow: StealOldest, release: LeaveHole }
  assertTrue' "round-robin and holes are fine when voices gate independently"
    (isNothing (check poly))

  -- And it behaves differently: a released note closes its own gate and nothing
  -- migrates, because nothing has to.
  let p0 = empty poly
      p1 = (noteOn 0.0 60 900.0 p0).voices
      p2 = (noteOn 0.0 64 100.0 p1).voices
      p3 = (noteOn 0.0 67 900.0 p2).voices
      pr = expireAt 200.0 p3
  assertEqual' "a gated instrument just closes the gate of the note that ended"
    { actual: map _.action pr.emits, expected: [ Gate 1 false ] }
  assertEqual' "and leaves the survivors exactly where they were"
    { actual: held pr.voices, expected: [ {at:0,pitch:60}, {at:2,pitch:67} ] }

  -- Round-robin walks the ring rather than falling back to voice 0, which is
  -- the point: a repeated note should not retrigger the same oscillator.
  let q0 = empty poly
      q1 = (noteOn 0.0 60 50.0 q0).voices
      q2 = (noteOn 100.0 62 50.0 (expireAt 60.0 q1).voices).voices
  assertEqual' "the second note takes voice 1, not the freed voice 0"
    { actual: held q2, expected: [ {at:1,pitch:62} ] }

  -- Stealing, when asked for. StealOldest takes the longest-running note.
  let s4 = foldNotes poly [ {p:60,on:0.0}, {p:62,on:10.0}, {p:64,on:20.0}, {p:65,on:30.0} ]
      st = noteOn 40.0 71 900.0 s4
  assertEqual' "StealOldest takes the voice whose note began first"
    { actual: held st.voices
    , expected: [ {at:0,pitch:71}, {at:1,pitch:62}, {at:2,pitch:64}, {at:3,pitch:65} ] }
  where
  foldNotes inst ns =
    let go v n = (noteOn n.on n.p 900.0 v).voices
    in foldl go (empty inst) ns
