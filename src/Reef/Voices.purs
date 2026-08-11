-- | Mapping a stream of notes onto a physical instrument's voices.
-- |
-- | Harmonia answers "which pitches", and answers it statelessly: a `Voicing` is
-- | an `Array Int` and a `VoicingStrategy` is `Voicing -> Voicing`. This module
-- | answers a different question — "which oscillator" — and it cannot be
-- | stateless, because which oscillator is free is a fact about history rather
-- | than about pitch. That is the whole reason it lives here and not there.
-- |
-- | The motivating instrument is the Instruo Saïch: four oscillators with four
-- | independent V/oct inputs but ONE output, mixed through VCAs under a single
-- | CV. Two consequences drive everything below.
-- |
-- | **Its oscillators never stop.** The mix CV is the only thing that can
-- | silence one, so a voice count IS the note-off mechanism. Measured on the
-- | rig 2026-08-11: 0.125 V for silence, then 0.675 / 1.300 / 2.675 / 4.350 V
-- | for one to four voices.
-- |
-- | **Voices are removed from one end.** So N sounding notes must occupy voices
-- | 0..N-1 contiguously, and a note released from the middle forces a survivor
-- | to migrate down. That migration is the interesting part: see `compact`.
module Reef.Voices
  ( Instrument
  , saich
  , Slot
  , Voices
  , empty
  , sounding
  , Action(..)
  , Emit
  , noteOn
  , expireAt
  , mixVoltsFor
  ) where

import Prelude

import Data.Array (catMaybes, filter, index, length, mapWithIndex, range, sortBy, updateAt)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe, maybe)

-- | A physical instrument whose voices reach one output through a shared
-- | voice-count control.
-- |
-- | `plateaus` is indexed BY VOICE COUNT — `plateaus !! 2` is the CV that leaves
-- | two voices sounding — so it has `voices + 1` entries, silence first. Those
-- | numbers are measured per module (`deepstar mixprofile`), never assumed:
-- | which direction the CV runs depends on an attenuverter.
-- |
-- | `rampMs` is how long to take moving between plateaus, and it is the note-off
-- | envelope. The mixer crossfades rather than steps — on the Saïch the 2→3 fade
-- | is 1.1 V wide — so the hardware supplies the fade and the ramp time decides
-- | its duration. Short enough to mask a migration, long enough not to click.
type Instrument =
  { voices :: Int
  , plateaus :: Array Number
  , rampMs :: Number
  }

-- | The Saïch as measured on 2026-08-11 (ES-9 jack 5 → mix CV, voices arriving
-- | 1→4 as the CV rises).
saich :: Instrument
saich =
  { voices: 4
  , plateaus: [ 0.125, 0.675, 1.300, 2.675, 4.350 ]
  , rampMs: 25.0
  }

-- | One physical voice's occupancy. `offAt` is when the note ends, in the same
-- | millisecond timebase the caller uses for `expireAt`.
type Slot = { pitch :: Int, offAt :: Number }

-- | Which note each physical voice is holding. `slots` always has
-- | `inst.voices` entries; `Nothing` is a free voice.
type Voices =
  { inst :: Instrument
  , slots :: Array (Maybe Slot)
  }

empty :: Instrument -> Voices
empty inst = { inst, slots: map (const Nothing) (range 1 inst.voices) }

-- | How many voices are currently holding a note.
sounding :: Voices -> Int
sounding v = length (filter isJust' v.slots)
  where
  isJust' = maybe false (const true)

-- | What the caller must do, and when.
-- |
-- | Ordering is load-bearing and is why these carry a time rather than being a
-- | list to apply at once. When a note migrates between oscillators it is
-- | briefly sounding on BOTH — set the arriving voice's pitch first and the
-- | departing one covers the move, so the listener hears the note continuously.
-- | Drop the count first and there is an audible hole instead.
data Action
  = Pitch Int Int
  -- ^ physical voice index, MIDI note
  | Mix Int Number
  -- ^ voice count, and the CV volts that produce it

derive instance eqAction :: Eq Action

instance showAction :: Show Action where
  show = case _ of
    Pitch v p -> "Pitch " <> show v <> " " <> show p
    Mix n cv -> "Mix " <> show n <> " " <> show cv

type Emit = { atMs :: Number, action :: Action }

-- | The CV that leaves `n` voices sounding, clamped to what the instrument has.
mixVoltsFor :: Instrument -> Int -> Number
mixVoltsFor inst n =
  let
    i = if n < 0 then 0 else if n > inst.voices then inst.voices else n
  in
    fromMaybe 0.0 (index inst.plateaus i)

-- | Sound a note now.
-- |
-- | Fills the lowest free voice, which is the only choice that keeps the
-- | sounding set contiguous from voice 0 — and contiguity is not a preference
-- | here, it is what the hardware's one-ended removal requires.
-- |
-- | With every voice busy the note is DROPPED rather than stealing one. Stealing
-- | is a much louder artefact than a missing note, and on a rig whose polyphony
-- | ceiling is known in advance the honest fix is to send fewer notes. The
-- | caller can see it happened: the returned emits are empty.
noteOn :: Number -> Int -> Number -> Voices -> { voices :: Voices, emits :: Array Emit }
noteOn now pitch durMs v =
  case freeSlot v of
    Nothing -> { voices: v, emits: [] }
    Just i ->
      let
        slots' = fromMaybe v.slots (updateAt i (Just { pitch, offAt: now + durMs }) v.slots)
        v' = v { slots = slots' }
        n = sounding v'
      in
        { voices: v'
        , emits:
            -- Pitch before count, so the voice is already on the right note when
            -- the mixer fades it in. The other way round fades in a stale pitch.
            [ { atMs: now, action: Pitch i pitch }
            , { atMs: now, action: Mix n (mixVoltsFor v.inst n) }
            ]
        }

freeSlot :: Voices -> Maybe Int
freeSlot v = firstJust (mapWithIndex pick v.slots)
  where
  pick i = case _ of
    Nothing -> Just i
    Just _ -> Nothing

-- | Retire every note whose time is up, compact the survivors, and say what to
-- | emit. Call this whenever time has advanced — a note's end is not an event
-- | the sequencer sends, it is a deadline set when the note began.
expireAt :: Number -> Voices -> { voices :: Voices, emits :: Array Emit }
expireAt now v =
  let
    live = map (\ms -> ms >>= \s -> if s.offAt > now then Just s else Nothing) v.slots
    before = sounding v
    kept = { inst: v.inst, slots: live }
    after = sounding kept
  in
    if after == before then { voices: v, emits: [] }
    else
      let
        moves = compact live
        v' = v { slots = applyMoves v.inst.voices live moves }
        -- Repitch first, then reduce the count. Every migrating note is sounding
        -- on its old voice throughout the repitch, so it covers its own move; the
        -- count drop then fades the vacated voice out underneath it.
        pitchEmits = map (\m -> { atMs: now, action: Pitch m.to m.pitch }) moves
        mixEmit = { atMs: now + v.inst.rampMs, action: Mix after (mixVoltsFor v.inst after) }
      in
        { voices: v', emits: pitchEmits <> [ mixEmit ] }

type Move = { from :: Int, to :: Int, pitch :: Int }

-- | Work out which survivors must migrate so the sounding set occupies voices
-- | 0..k-1.
-- |
-- | Survivors already low enough stay put; only those above the new ceiling move
-- | down into the holes. That is the MINIMUM number of oscillators disturbed,
-- | and it matters: the obvious alternative — shuffle everyone down one — moves
-- | three voices where this moves one, and every move is an audible event.
-- |
-- | Movers fill holes in slot order, lowest to lowest. With at most four voices
-- | the alternative — searching the assignment that minimises total pitch jump —
-- | would change the outcome rarely and make it unpredictable, and predictable
-- | matters more here: the same musical figure should migrate the same way every
-- | time, or the artefact becomes a different artefact on each repeat.
compact :: Array (Maybe Slot) -> Array Move
compact live =
  let
    k = length (filter (maybe false (const true)) live)
    occupied = catMaybes (mapWithIndex (\i ms -> map (\s -> { at: i, slot: s }) ms) live)
    movers = filter (\o -> o.at >= k) occupied
    holes = filter (\i -> maybe true (const false) (flatten (index live i))) (range' 0 k)
  in
    pairUp (sortBy (comparing _.at) movers) holes

  where
  flatten = case _ of
    Just x -> x
    Nothing -> Nothing

  pairUp ms hs = case ms, hs of
    _, [] -> []
    [], _ -> []
    _, _ ->
      let
        pairs = zipWith' (\m h -> { from: m.at, to: h, pitch: m.slot.pitch }) ms hs
      in
        pairs

range' :: Int -> Int -> Array Int
range' lo hi = if hi <= lo then [] else range lo (hi - 1)

zipWith' :: forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c
zipWith' f as bs = catMaybes (mapWithIndex (\i a -> map (f a) (index bs i)) as)

applyMoves :: Int -> Array (Maybe Slot) -> Array Move -> Array (Maybe Slot)
applyMoves _ live moves =
  let
    cleared = foldl (\acc m -> fromMaybe acc (updateAt m.from Nothing acc)) live moves
    place acc m = case index live m.from of
      Just (Just s) -> fromMaybe acc (updateAt m.to (Just s) acc)
      _ -> acc
  in
    foldl place cleared moves

firstJust :: forall a. Array (Maybe a) -> Maybe a
firstJust = foldl (\acc x -> case acc of
  Just _ -> acc
  Nothing -> x) Nothing
