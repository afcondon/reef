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
import Reef.Voices (Action(..), allOff, Assign(..), Order(..), Overflow(..), Release(..), Silencing(..), Voices, check, decayVoltsFor, empty, expireAt, noteOn, qd, rings, saich, sounding)
import Data.Maybe (Maybe(..), isJust, isNothing)
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
             , overflow: StealOldest, release: LeaveHole, order: Arrival }
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
  -- Stopping is an ACT. Without this the last chord drones for ever on a module
  -- whose oscillators never stop — observed on the rack 2026-08-11, where
  -- halting Odonus left four voices sounding indefinitely because nothing was
  -- calling expireAt any more.
  let d0 = empty saich
      d1 = (noteOn 0.0 60 9999.0 d0).voices
      d2 = (noteOn 0.0 64 9999.0 d1).voices
      off = allOff 100.0 d2
  assertEqual' "all-off drives the voice count to zero"
    { actual: map _.action off.emits, expected: [ Mix 0 0.125 ] }
  assertEqual' "and forgets every voice"
    { actual: sounding off.voices, expected: 0 }
  assertEqual' "all-off on an already-silent instrument says nothing"
    { actual: map _.action (allOff 100.0 (empty saich)).emits, expected: [] }

  -- A gated instrument closes its gates instead, one per sounding voice.
  let g2 = (noteOn 0.0 64 9999.0 (noteOn 0.0 60 9999.0 (empty poly)).voices).voices
  assertEqual' "a gated instrument closes each open gate"
    { actual: map _.action (allOff 50.0 g2).emits
    , expected: [ Gate 0 false, Gate 1 false ] }

  -- ByPitch: voice 0 is always the lowest sounding note, so its CV can be
  -- split to another oscillator and mean something stable. The cost is movement
  -- — a new bass note shifts everything above it up a voice — which is the
  -- exact opposite trade from Arrival, and why it is a mode rather than a fix.
  let sorted = saich { order = ByPitch }
  assertTrue' "ByPitch is coherent on the Saich" (isNothing (check sorted))
  assertTrue' "but not together with round-robin, which would be ignored"
    (isJust (check (sorted { assign = RoundRobin })))

  let b0 = empty sorted
      b1 = (noteOn 0.0 67 900.0 b0).voices     -- G
      b2 = (noteOn 10.0 72 900.0 b1).voices    -- C above it: stays above
  assertEqual' "a higher note seats above without disturbing the lower"
    { actual: held b2, expected: [ {at:0,pitch:67}, {at:1,pitch:72} ] }

  let b3 = noteOn 20.0 60 900.0 b2      -- C below both: pushes them up
  assertEqual' "a new BASS note takes voice 0 and shifts the rest up"
    { actual: held b3.voices
    , expected: [ {at:0,pitch:60}, {at:1,pitch:67}, {at:2,pitch:72} ] }
  assertEqual' "emitting one repitch per voice that actually changed, low first"
    { actual: pitches (map _.action b3.emits)
    , expected: [ Pitch 0 60, Pitch 1 67, Pitch 2 72 ] }

  -- And the property the splitter depends on: after ANY change, voice 0 holds
  -- the lowest note. Here the bass is released, so the next-lowest must descend.
  let b4 = (noteOn 30.0 55 40.0 b3.voices).voices   -- lower still, short
      b5 = expireAt 100.0 b4
  assertEqual' "when the bass ends, the next-lowest takes voice 0"
    { actual: held b5.voices
    , expected: [ {at:0,pitch:60}, {at:1,pitch:67}, {at:2,pitch:72} ] }

  -- Arrival, for contrast, leaves them where they were: same notes, different
  -- seats, far fewer emissions.
  let a2 = (noteOn 10.0 72 900.0 (noteOn 0.0 67 900.0 (empty saich)).voices).voices
      a3 = noteOn 20.0 60 900.0 a2
  assertEqual' "Arrival puts the bass in the next free voice and moves nobody"
    { actual: held a3.voices
    , expected: [ {at:0,pitch:67}, {at:1,pitch:72}, {at:2,pitch:60} ] }
  assertEqual' "one repitch, not three"
    { actual: pitches (map _.action a3.emits), expected: [ Pitch 2 60 ] }

  -- ---------------------------------------------------------------------
  -- Rings: the module allocates, we only strum
  -- ---------------------------------------------------------------------

  -- A chord cannot arrive at one instant through one bus, so it spreads. Each
  -- trigger follows ITS OWN pitch by settleMs — the ordering that matters, since
  -- the module samples the CV at the edge and a trigger that overtook its pitch
  -- would sound the previous note again.
  let k0 = empty rings
      k1 = noteOn 100.0 60 500.0 k0
      k2 = noteOn 100.0 64 500.0 k1.voices
      k3 = noteOn 100.0 67 500.0 k2.voices
  assertEqual' "the first note is strummed at once, pitch then trigger"
    { actual: k1.emits
    , expected:
        [ { atMs: 100.0, action: Pitch 0 60 }
        , { atMs: 104.0, action: Trigger 0 5.0 }
        ]
    }
  assertEqual' "the second waits one strum gap"
    { actual: k2.emits
    , expected:
        [ { atMs: 112.0, action: Pitch 0 64 }
        , { atMs: 116.0, action: Trigger 0 5.0 }
        ]
    }
  assertEqual' "and the third another, so the chord is a 24 ms arpeggio"
    { actual: k3.emits
    , expected:
        [ { atMs: 124.0, action: Pitch 0 67 }
        , { atMs: 128.0, action: Trigger 0 5.0 }
        ]
    }
  -- Every pitch is clear of the trigger before it, which is the invariant the
  -- strumMs >= settleMs + triggerMs check exists to guarantee.
  assertTrue' "no note's pitch lands before the previous trigger has finished"
    (124.0 >= 116.0 + 5.0)

  -- A note arriving long after the last one is not delayed: the spread is a
  -- minimum gap, not a quantisation.
  let k4 = noteOn 1000.0 72 500.0 k3.voices
  assertEqual' "a note well clear of the last strum goes out immediately"
    { actual: map _.atMs k4.emits, expected: [ 1000.0, 1004.0 ] }

  -- We take no slot, because we allocate nothing. `sounding` must not claim
  -- otherwise — a count we cannot observe would be believed.
  assertEqual' "no slots are taken" { actual: sounding k4.voices, expected: 0 }

  -- No note-off exists, so time passing emits nothing and stopping emits
  -- nothing. Unlike the Saich, that is not a stuck drone: what is ringing rings
  -- out on the module's own decay.
  assertEqual' "expiry has nothing to retire"
    { actual: (expireAt 5000.0 k4.voices).emits, expected: [] }
  assertEqual' "stopping leaves it to ring out"
    { actual: (allOff 5000.0 k4.voices).emits, expected: [] }

  -- The policies are all vacuous here, and `check` refuses them rather than
  -- ignoring them — a setting that looks live and does nothing is the failure
  -- this decomposition exists to prevent.
  assertTrue' "round-robin on a self-allocating instrument is refused"
    (isJust (check rings { assign = RoundRobin }))
  assertTrue' "compaction on a self-allocating instrument is refused"
    (isJust (check rings { release = Compact }))
  assertTrue' "more than one bus is refused"
    (isJust (check rings { voices = 4 }))
  assertTrue' "a strum gap too short to fit its own settle and pulse is refused"
    (isJust (check rings { silencing = SelfAllocating { settleMs: 4.0, triggerMs: 5.0, strumMs: 6.0 } }))
  assertEqual' "as configured, Rings is coherent"
    { actual: check rings, expected: Nothing }

  -- ---------------------------------------------------------------------
  -- QD: a bank of struck voices, allocated by us
  -- ---------------------------------------------------------------------

  -- The payoff over Rings: four buses hold four pitches, so a chord lands at ONE
  -- instant instead of spreading. Every note's trigger sits settleMs after its
  -- own pitch, and no two notes share a voice.
  let w0 = empty qd
      w1 = noteOn 100.0 60 500.0 w0
      w2 = noteOn 100.0 64 500.0 w1.voices
      w3 = noteOn 100.0 67 500.0 w2.voices
  assertEqual' "three simultaneous notes are struck at the same instant"
    { actual: map _.atMs (w1.emits <> w2.emits <> w3.emits)
    , expected: [ 100.0, 100.0, 102.0, 100.0, 100.0, 102.0, 100.0, 100.0, 102.0 ] }
  assertEqual' "each takes its own voice, decay and pitch before the trigger"
    { actual: map _.action w3.emits
    , expected: [ Decay 2 (decayVoltsFor dm 500.0), Pitch 2 67, Trigger 2 5.0 ] }

  -- Round-robin, and it matters: re-striking a voice cuts its decay, so the
  -- fourth note takes voice 3 and a fifth wraps to the longest-idle.
  let w4 = noteOn 100.0 72 500.0 w3.voices
  assertEqual' "the fourth note fills the last voice"
    { actual: held w4.voices
    , expected: [ {at:0,pitch:60}, {at:1,pitch:64}, {at:2,pitch:67}, {at:3,pitch:72} ] }
  let w5 = noteOn 110.0 76 500.0 w4.voices
  assertEqual' "a fifth note steals the voice struck longest ago"
    { actual: held w5.voices
    , expected: [ {at:0,pitch:76}, {at:1,pitch:64}, {at:2,pitch:67}, {at:3,pitch:72} ] }

  -- Gate length becomes a VOLTAGE, monotonically and logarithmically. This is
  -- the third thing duration has meant: a note-off time on the Saich, nothing
  -- at all on Rings, a decay CV here.
  assertTrue' "a longer note asks for a higher decay CV"
    (decayVoltsFor dm 1000.0 > decayVoltsFor dm 200.0)
  assertTrue' "the map is logarithmic, not linear — 40->200ms spans as much as 200->1000"
    (let a = decayVoltsFor dm 200.0 - decayVoltsFor dm 40.0
         b = decayVoltsFor dm 1000.0 - decayVoltsFor dm 200.0
     in a - b < 0.01 && b - a < 0.01)
  assertEqual' "and it clamps rather than running off the ends"
    { actual: decayVoltsFor dm 99999.0, expected: 5.0 }

  -- No note-off exists, so nothing is emitted when a note's time is up or when
  -- the transport stops — the sample rings out on its own envelope.
  assertEqual' "expiry frees the voice silently"
    { actual: (expireAt 5000.0 w5.voices).emits, expected: [] }
  assertEqual' "and a voice freed that way is available again"
    { actual: sounding (expireAt 5000.0 w5.voices).voices, expected: 0 }
  assertEqual' "stopping leaves the samples to ring out"
    { actual: (allOff 5000.0 w5.voices).emits, expected: [] }

  -- Both refusals come from one fact: a struck voice holds a sounding sample.
  assertTrue' "compaction is refused — the sound is in that voice"
    (isJust (check qd { release = Compact }))
  assertTrue' "pitch-ordered seating is refused — it could only re-strike"
    (isJust (check qd { order = ByPitch, assign = LowestFree }))
  assertEqual' "as configured, QD is coherent"
    { actual: check qd, expected: Nothing }

  where
  dm = { minMs: 40.0, maxMs: 2000.0, minV: 0.0, maxV: 5.0 }
  foldNotes inst ns =
    let go v n = (noteOn n.on n.p 900.0 v).voices
    in foldl go (empty inst) ns
