-- | Playing a Squarp Rample from a card that has been described.
-- |
-- | Every other instrument in this package is told a pitch and sends a pitch.
-- | The Rample cannot be: a MIDI note reaching it selects a *voice and a
-- | layer*, never a pitch, so a melody sent to it as notes arrives as rhythm
-- | with the tune discarded. Pitch on this module is the START POINT — which
-- | slice of a concatenated file to play — and that is a control change.
-- |
-- | So a note here is two messages in a fixed order:
-- |
-- |     CC (voice * 10 + 4)  <- the slice holding this pitch
-- |     note-on <trigger>    <- 40 ms later, at the velocity that picks a layer
-- |
-- | and the order is load-bearing. A trigger arriving before its control
-- | change plays the *previous* slice, which sounds like a wrong note rather
-- | than like a fault, so `Reef.Voices.rample` carries the settle time and this
-- | module refuses to emit a trigger without one.
-- |
-- | ## Why an index is a parameter and not a constant
-- |
-- | Nothing about a card says what its slices mean. A voice holding one
-- | six-minute file might be a field recording or sixty-four piano notes, and
-- | the arithmetic cannot even narrow it — 384 seconds divides exactly by all
-- | eight divisions the module's SLICER offers. So the mapping from pitch to
-- | slice is written down when the card is compiled and read back here. This
-- | module knows how to *address* a Rample; the index knows what is on one.
module Reef.Rample
  ( Layer
  , Voice
  , Kit
  , Message(..)
  , Timed
  , startCC
  , ccForSlot
  , slotFor
  , layerFor
  , voiceOf
  , select
  , note
  , render
  ) where

import Prelude

import Data.Array (filter, findIndex, index, snoc, sortBy)
import Data.Foldable (foldl)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Reef.Voices (Action(..), Emit)

-- | One layer of one voice, as the card's index describes it.
-- |
-- | `velocity` is the dynamic this layer stands for — the module picks a layer
-- | by incoming velocity when the kit is in VELOCITY mode, so this is what
-- | says which layer *is* which. `pitchOfSlot0` and `slotPitches` are the two
-- | ways a set of slices can be laid out: a chromatic run from a base note, or
-- | an explicit pitch per slice for a set that is not a run — a kalimba in
-- | tine order, a rack of unrelated chords.
type Layer =
  { velocity :: Maybe Int
  , slots :: Int
  , pitchOfSlot0 :: Maybe Int
  , slotPitches :: Maybe (Array Int)
  }

type Voice =
  { voice :: Int
  -- ^ 1-4, as the panel numbers them
  , trigger :: Maybe Int
  -- ^ the MIDI note that fires it, from `SETTINGS > SPx`. Not knowable from
  -- the card; the manifest says, and a voice without one cannot be played.
  , layers :: Array Layer
  }

type Kit =
  { bankSelect :: Int
  , program :: Int
  , channel :: Int
  , voices :: Array Voice
  }

data Message
  = CC Int Int Int
  -- ^ channel, controller, value
  | NoteOn Int Int Int
  -- ^ channel, note, velocity
  | NoteOff Int Int
  -- ^ channel, note
  | ProgramChange Int Int
  -- ^ channel, program

derive instance eqMessage :: Eq Message

instance showMessage :: Show Message where
  show (CC c n v) = "CC " <> show c <> " " <> show n <> " " <> show v
  show (NoteOn c n v) = "NoteOn " <> show c <> " " <> show n <> " " <> show v
  show (NoteOff c n) = "NoteOff " <> show c <> " " <> show n
  show (ProgramChange c p) = "PC " <> show c <> " " <> show p

type Timed = { atMs :: Number, message :: Message }

-- | The start point for a voice: `CC(voice * 10 + 4)`.
-- |
-- | The same numbering the module's MIDI implementation uses throughout —
-- | `voice * 10 + p`, with `p` the parameter — so 4 is the start point on
-- | every voice.
startCC :: Int -> Int
startCC voice = voice * 10 + 4

-- | The control value landing in the MIDDLE of slot `k` of `n`.
-- |
-- | The middle rather than an edge. The card stores the start point 0-254 and
-- | a control change is 0-127, so a control step is not quite one slice; aiming
-- | at the centre leaves the most room for that to be slightly wrong before it
-- | lands in the neighbouring slot. Measured even and one-to-one at 64 slices;
-- | at 128 every slice is still reachable but with half the slack.
ccForSlot :: Int -> Int -> Int
ccForSlot k n
  | n <= 0 = 0
  | otherwise =
      let v = round (127.0 * (toNumber k + 0.5) / toNumber n)
      in if v < 0 then 0 else if v > 127 then 127 else v

-- | Which slice of a layer holds a pitch, if any of them does.
slotFor :: Layer -> Int -> Maybe Int
slotFor layer pitch = case layer.slotPitches of
  Just ps -> findIndex (_ == pitch) ps
  Nothing -> layer.pitchOfSlot0 >>= \base ->
    let k = pitch - base
    in if k >= 0 && k < layer.slots then Just k else Nothing

-- | Which layer a velocity selects.
-- |
-- | The module does this itself in VELOCITY mode; we work it out too, because
-- | the answer decides which layer's slot table applies — and a card whose
-- | layers cover different pitch ranges would otherwise be addressed against
-- | the wrong one.
-- |
-- | Nearest declared velocity wins. Layers without a declared velocity are not
-- | candidates: an undeclared layer is one whose dynamic nobody wrote down,
-- | and guessing at it would be worse than declining.
layerFor :: Array Layer -> Int -> Maybe Layer
layerFor layers velocity =
  case sortBy (comparing (\l -> abs' (fromMaybe 0 l.velocity - velocity)))
         (filter (\l -> l.velocity /= Nothing) layers) of
    [] -> index layers 0
    sorted -> index sorted 0
  where
  abs' n = if n < 0 then -n else n

-- | The voice record for a physical voice index, 0-based as `Reef.Voices`
-- | counts them.
voiceOf :: Kit -> Int -> Maybe Voice
voiceOf kit i = index kit.voices i

-- | The messages that put a kit in front of the module.
-- |
-- | Bank select then program change, which is the order the module expects and
-- | the order that means a program change never lands in the previous bank.
select :: Kit -> Array Message
select kit =
  [ CC kit.channel 0 kit.bankSelect
  , ProgramChange kit.channel kit.program
  ]

-- | One note on one voice: the start point, then the trigger, in that order
-- | and with that gap.
-- |
-- | `Nothing` when the pitch is not on the card, or the voice has no trigger
-- | note. Both are refusals rather than approximations: a pitch this card does
-- | not hold has no nearest neighbour worth playing, and a voice whose trigger
-- | nobody has set cannot be reached at all.
note
  :: Kit
  -> { voice :: Int, pitch :: Int, velocity :: Int, atMs :: Number, settleMs :: Number, durMs :: Number }
  -> Maybe (Array Timed)
note kit spec = do
  v <- voiceOf kit spec.voice
  trigger <- v.trigger
  layer <- layerFor v.layers spec.velocity
  slot <- slotFor layer spec.pitch
  let cc = ccForSlot slot layer.slots
  pure
    [ { atMs: spec.atMs - spec.settleMs
      , message: CC kit.channel (startCC v.voice) cc
      }
    , { atMs: spec.atMs, message: NoteOn kit.channel trigger spec.velocity }
    , { atMs: spec.atMs + spec.durMs, message: NoteOff kit.channel trigger }
    ]

-- | Turn what `Reef.Voices` decided into what the module needs.
-- |
-- | `Voices` answers "which voice"; this answers "how to say it". The split is
-- | the reason the Rample needed no new allocation logic: it is the QuadDrum's
-- | shape with a different way of pronouncing a pitch.
-- |
-- | `Decay` and `Mix` actions are dropped, and dropping them is correct rather
-- | than lazy — the Rample has neither control, and `Reef.Voices.rample`
-- | declares as much, so an emitted one would be a bug upstream.
render :: Kit -> Number -> Number -> Array Emit -> Array Timed
render kit settleMs durMs = foldl step []
  where
  step acc e = case e.action of
    Pitch v pitch ->
      acc <> fromMaybe []
        ( note kit
            { voice: v
            , pitch
            , velocity: 100
            , atMs: e.atMs
            , settleMs
            , durMs
            }
        )
    Gate v on ->
      case voiceOf kit v >>= _.trigger of
        Nothing -> acc
        Just t ->
          snoc acc
            { atMs: e.atMs
            , message: if on then NoteOn kit.channel t 100 else NoteOff kit.channel t
            }
    _ -> acc
