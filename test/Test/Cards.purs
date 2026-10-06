-- | Vetula's cards naming a saved progression (step 4b): the three card forms,
-- | the lookup, and what Vetula must leave alone.
module Test.Cards (cardsTests) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Reef.Vetula.Lepidoptera (cardProgression, parseCard, parseCardIn, printProgression, progressionKey, progressionOfKey, readProgression)
import Test.Assert (assert', assertEqual')

cardsTests :: Effect Unit
cardsTests = do
  let
    bolt = [ [ 57, 72, 76, 81 ], [ 53, 72, 77, 81 ], [ 55, 74, 79, 83 ] ]
    lookup = case _ of
      "bolt-tractor-horse" -> Just bolt
      _ -> Nothing
    chordsSeq = map (\s -> { chords: s.chords, seqText: s.seqText, channel: s.channel })

  assertEqual' "a progression round-trips through its stage text"
    { actual: readProgression (printProgression bolt), expected: bolt }
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
  log "Vetula cards by name: OK"
