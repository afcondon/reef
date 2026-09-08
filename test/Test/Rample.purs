-- | Tests for addressing a Rample from a described card.
-- |
-- | The numbers here are not invented. They are what the reference realiser
-- | (`SamplesProject/tools/rample-play.py`) produces for the piano card built
-- | on 2026-09-08, and what that card was heard to play: an even chromatic
-- | sweep across sixty-four slices, four dynamics on one pitch, and chords
-- | across four voices. Holding the two implementations to the same figures is
-- | the whole point of writing them down twice.
module Test.Rample (rampleTests) where

import Prelude

import Data.Array (index)
import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Console (log)
import Reef.Rample (Kit, Layer, Message(..), ccForSlot, layerFor, note, slotFor, startCC)
import Test.Assert (assertEqual', assertTrue')

-- | The piano card, as its index describes it: four identical voices, each
-- | four velocity layers of sixty-four slices starting at MIDI 36 (C2).
pianoLayer :: Int -> Layer
pianoLayer velocity =
  { velocity: Just velocity
  , slots: 64
  , pitchOfSlot0: Just 36
  , slotPitches: Nothing
  }

piano :: Kit
piano =
  { bankSelect: 15
  , program: 0
  , channel: 1
  , voices: map
      (\v -> { voice: v, trigger: Just (59 + v), layers: map pianoLayer [ 16, 48, 79, 111 ] })
      [ 1, 2, 3, 4 ]
  }

rampleTests :: Effect Unit
rampleTests = do
  -- The start point is the fifth parameter of its voice, on every voice.
  assertEqual' "voice 1 start point" { actual: startCC 1, expected: 14 }
  assertEqual' "voice 4 start point" { actual: startCC 4, expected: 44 }

  -- The three figures the reference realiser produces at the corners, and the
  -- ones the module was heard to play as an even chromatic run.
  assertEqual' "slot 0 of 64" { actual: ccForSlot 0 64, expected: 1 }
  assertEqual' "slot 24 of 64 — middle C" { actual: ccForSlot 24 64, expected: 49 }
  assertEqual' "slot 63 of 64" { actual: ccForSlot 63 64, expected: 126 }

  -- Strictly rising across the whole run. Two pitches resolving to one control
  -- value is the failure that sounds like a repeated semitone, which is what
  -- the sweep was listening for.
  let ccs = map (\k -> ccForSlot k 64) (rangeTo 63)
  assertTrue' "every slot gets a higher control value than the last"
    (strictlyRising ccs)

  -- A chromatic layer answers by arithmetic, and declines outside its range
  -- rather than clamping — a pitch the card does not hold has no nearest
  -- neighbour worth playing.
  assertEqual' "C4 is slot 24" { actual: slotFor (pianoLayer 79) 60, expected: Just 24 }
  assertEqual' "C2 is slot 0" { actual: slotFor (pianoLayer 79) 36, expected: Just 0 }
  assertTrue' "a note below the card is refused" (isNothing (slotFor (pianoLayer 79) 35))
  assertTrue' "a note above the card is refused" (isNothing (slotFor (pianoLayer 79) 100))

  -- A set that is not a chromatic run answers by lookup. The Kalimba is the
  -- case: eleven tines in thumb order, which is not pitch order.
  let kalimba =
        { velocity: Nothing
        , slots: 12
        , pitchOfSlot0: Nothing
        , slotPitches: Just [ 79, 69, 76, 62, 72, 55, 74, 67, 78, 71, 81 ]
        }
  assertEqual' "G5 is the first tine" { actual: slotFor kalimba 79, expected: Just 0 }
  assertEqual' "G3 is the sixth" { actual: slotFor kalimba 55, expected: Just 5 }
  assertTrue' "a note the instrument does not have is refused"
    (isNothing (slotFor kalimba 60))

  -- Velocity picks the nearest declared layer.
  let layers = map pianoLayer [ 16, 48, 79, 111 ]
  assertEqual' "a whisper takes the softest layer"
    { actual: map _.velocity (layerFor layers 1), expected: Just (Just 16) }
  assertEqual' "full force takes the loudest"
    { actual: map _.velocity (layerFor layers 127), expected: Just (Just 111) }
  assertEqual' "and the middle takes the middle"
    { actual: map _.velocity (layerFor layers 70), expected: Just (Just 79) }

  -- A note is a control change and then a trigger, in that order, separated by
  -- the settle time. The order is the whole of why this module exists: a
  -- trigger arriving first plays the previous slice, which sounds like a wrong
  -- note rather than like a fault.
  case note piano { voice: 0, pitch: 60, velocity: 100, atMs: 1000.0, settleMs: 40.0, durMs: 500.0 } of
    Nothing -> assertTrue' "middle C on voice 1 should be playable" false
    Just msgs -> do
      assertEqual' "three messages" { actual: length' msgs, expected: 3 }
      assertEqual' "the start point comes first, one settle early"
        { actual: map _.atMs (index msgs 0), expected: Just 960.0 }
      assertEqual' "and it addresses slot 24"
        { actual: map _.message (index msgs 0), expected: Just (CC 1 14 49) }
      assertEqual' "then the trigger, on time"
        { actual: map _.message (index msgs 1), expected: Just (NoteOn 1 60 100) }
      assertEqual' "and it ends when it was told to"
        { actual: map _.atMs (index msgs 2), expected: Just 1500.0 }

  -- Each voice has its own start point and its own trigger, which is what
  -- makes four of them four independent players of one instrument.
  case note piano { voice: 2, pitch: 67, velocity: 100, atMs: 0.0, settleMs: 40.0, durMs: 100.0 } of
    Nothing -> assertTrue' "voice 3 should be playable" false
    Just msgs -> do
      -- G4 is slot 31, and 63 is what the reference realiser sends for it.
      -- Written as a literal rather than as the expression that produced it:
      -- comparing a function to itself proves only that it is deterministic.
      assertEqual' "voice 3 addresses its own start point, at slot 31"
        { actual: map _.message (index msgs 0), expected: Just (CC 1 34 63) }
      assertEqual' "and triggers its own note"
        { actual: map _.message (index msgs 1), expected: Just (NoteOn 1 62 100) }

  log "Reef Rample addressing: OK"

length' :: forall a. Array a -> Int
length' = foldlLen 0
  where
  foldlLen n xs = case index xs n of
    Nothing -> n
    Just _ -> foldlLen (n + 1) xs

rangeTo :: Int -> Array Int
rangeTo n = go 0 []
  where
  go i acc = if i > n then acc else go (i + 1) (acc <> [ i ])

strictlyRising :: Array Int -> Boolean
strictlyRising xs = go 1
  where
  go i = case index xs i, index xs (i - 1) of
    Just a, Just b -> a > b && go (i + 1)
    _, _ -> true
