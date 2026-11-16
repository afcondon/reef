-- | Vetula's cards naming a saved progression (step 4b): the three card forms,
-- | the lookup, and what Vetula must leave alone.
module Test.Cards (cardsTests) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Reef.Vetula.Lepidoptera (cardProgression, parseCard, parseCardIn, printProgression, printProgressionIn, progressionKey, progressionOfKey, readProgression, readStaged, printCard, printClip, readStagedClip, stageClip)
import Test.Assert (assert', assertEqual')

cardsTests :: Effect Unit
cardsTests = do
  let
    bolt = [ [ 57, 72, 76, 81 ], [ 53, 72, 77, 81 ], [ 55, 74, 79, 83 ] ]
    lookup = case _ of
      "bolt-tractor-horse" -> Just { chords: bolt, beats: [], clip: Nothing }
      "tapped" -> Just { chords: bolt, beats: [ 4, 2, 2 ], clip: Nothing }
      "clip:gem-truck" -> Just (readStagedClip "gem-truck" (printClip truck))
      _ -> Nothing
    truck =
      { name: "gem-truck", ticks: 528
      , notes: [ { at: 0, pitch: 60, vel: 85, gate: 22, head: 0 }, { at: 24, pitch: 63, vel: 62, gate: 90, head: 2 } ] }
    chordsSeq = map (\s -> { chords: s.chords, seqText: s.seqText, channel: s.channel })

  assertEqual' "a progression round-trips through its stage text"
    { actual: readProgression (printProgression bolt), expected: bolt }
  assertEqual' "a tapped rhythm round-trips through the stage text"
    { actual: readStaged (printProgressionIn [ 4, 2, 2 ] bolt), expected: { chords: bolt, beats: [ 4, 2, 2 ], clip: Nothing } }
  assertEqual' "an untapped progression has no rhythm"
    { actual: (readStaged (printProgression bolt)).beats, expected: [] }
  assertEqual' "vetula \"name\" over a tapped progression plays it as tapped"
    { actual: chordsSeq (parseCardIn lookup 4 "vetula \"tapped\" # arpup 8")
    , expected: Just { chords: bolt, seqText: "[0@4 1@2 2@2]/2", channel: 4 } }
  assertEqual' "its own sequence still wins over the rhythm"
    { actual: map _.seqText (parseCardIn lookup 4 "vetula \"tapped\" \"0 1\""), expected: Just "0 1" }
  assertEqual' "progression keys"
    { actual: progressionOfKey (progressionKey "bolt-tractor-horse"), expected: Just "bolt-tractor-horse" }

  assertEqual' "vetula \"name\": channel from the card, one chord per bar"
    { actual: chordsSeq (parseCardIn lookup 4 "vetula \"bolt-tractor-horse\" # arpup 8")
    , expected: Just { chords: bolt, seqText: "<0 1 2>", channel: 4 } }
  assert' "vetula \"name\" keeps its layers"
    (map _.stack (parseCardIn lookup 4 "vetula \"bolt-tractor-horse\" # arpup 8")
      == map _.stack (parseCard "ch4 - \"\" # arpup 8"))
  assertEqual' "vetula \"name\" \"seq\": its own sequence"
    { actual: chordsSeq (parseCardIn lookup 2 "vetula \"bolt-tractor-horse\" \"0 0 1 2\"")
    , expected: Just { chords: bolt, seqText: "0 0 1 2", channel: 2 } }
  assertEqual' "an unknown name is silent, not refused"
    { actual: chordsSeq (parseCardIn lookup 2 "vetula \"no-such-thing\"")
    , expected: Just { chords: [], seqText: "", channel: 2 } }
  assertEqual' "chN name: chords by name"
    { actual: chordsSeq (parseCardIn lookup 9 "ch3 bolt-tractor-horse \"0 1\"")
    , expected: Just { chords: bolt, seqText: "0 1", channel: 3 } }
  assertEqual' "chN \"name\": a quoted name is a name"
    { actual: chordsSeq (parseCardIn lookup 6 "ch3 \"bolt-tractor-horse\" # voice drop2 # arpup 4 # every 4 # strum 14")
    , expected: Just { chords: bolt, seqText: "<0 1 2>", channel: 3 } }
  assertEqual' "cardProgression: chN \"name\""
    { actual: cardProgression "ch3 \"bolt-tractor-horse\" # arpup 4", expected: Just "bolt-tractor-horse" }
  assertEqual' "chords written in are unchanged"
    { actual: chordsSeq (parseCardIn lookup 9 "ch3 \"<[a3,c5,e5,a5]>\" \"0\"")
    , expected: Just { chords: [ [ 45, 60, 64, 69 ] ], seqText: "0", channel: 3 } }

  assertEqual' "cardProgression: vetula form"
    { actual: cardProgression "vetula \"bolt-tractor-horse\" # arpup 8", expected: Just "bolt-tractor-horse" }
  assertEqual' "cardProgression: chN name"
    { actual: cardProgression "ch3 bolt-tractor-horse \"0 1\"", expected: Just "bolt-tractor-horse" }
  assertEqual' "cardProgression: chords written in name nothing"
    { actual: cardProgression "ch3 \"<[a3,c5,e5,a5]>\" \"0\"", expected: Nothing }
  assertEqual' "cardProgression: a sourceless card names nothing"
    { actual: cardProgression "ch3 - \"0(3,8)\"", expected: Nothing }
  assertEqual' "a clip round-trips through its stage text"
    { actual: (readStagedClip "gem-truck" (printClip truck)).clip, expected: Just truck }
  assertEqual' "chN clip:name: the clip by name, its layers kept"
    { actual: map (\s -> { clip: s.clip, channel: s.channel, layers: Array.length s.stack }) (parseCardIn lookup 9 "ch3 clip:gem-truck \"\" # slow 2")
    , expected: Just { clip: Just truck, channel: 3, layers: 1 } }
  assertEqual' "clip \"name\": a voice, as Limulus writes it"
    { actual: map (\s -> { clip: s.clip, channel: s.channel, layers: Array.length s.stack }) (parseCardIn lookup 4 "clip \"gem-truck\" # slow 2")
    , expected: Just { clip: Just truck, channel: 4, layers: 1 } }
  assertEqual' "a clip card prints as it reads"
    { actual: map printCard (parseCardIn lookup 9 "ch3 clip:gem-truck \"\" # slow 2"), expected: Just "ch3 clip:gem-truck \"\" # slow 2" }
  assertEqual' "an unknown clip is silent, not refused"
    { actual: map _.clip (parseCardIn lookup 4 "clip \"no-such\""), expected: Just Nothing }
  assertEqual' "cardProgression: a clip is not a progression"
    { actual: cardProgression "ch3 clip:gem-truck \"\" # slow 2", expected: Nothing }
  assertEqual' "a library clip in ticks: 120 bpm, a sixteenth is 24"
    { actual: stageClip "x" { id: "x", events: [ { pitch: 60, headIdx: 1, fireUnixMicros: 125000.0, vel: 90, gateMs: 250.0 } ], lenMicros: 5510000.0, heads: 1, capturedMicros: 0.0, source: "t", name: "x", tags: [], notes: "", bpm: Just 120.0, key: Nothing, context: Nothing }
    , expected: { name: "x", ticks: 1056, notes: [ { at: 24, pitch: 60, vel: 90, gate: 48, head: 1 } ] } }
  log "Vetula cards by name: OK"
