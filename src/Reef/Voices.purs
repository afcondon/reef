-- | Mapping a stream of notes onto a physical instrument's voices.
-- |
-- | Harmonia answers "which pitches", statelessly: `VoicingStrategy = Voicing ->
-- | Voicing`. This answers "which oscillator", and cannot be stateless, because
-- | which oscillator is free is a fact about history rather than about pitch.
-- | That is why it lives here and not there.
-- |
-- | ## The decomposition
-- |
-- | "Voice stealing", "subtraction", "round-robin" sound like alternatives and
-- | are not — they are values from three different sets, plus one fact about
-- | the hardware:
-- |
-- |   * `Silencing` — a CAPABILITY. How can this module silence a voice at all?
-- |     Measured (see `deepstar mixprofile`), never chosen.
-- |   * `Assign` — which voice takes a new note.
-- |   * `Overflow` — what happens when none is free.
-- |   * `Release` — what happens to the hole a finished note leaves.
-- |
-- | Separating them is what makes this describe modules nobody here owns. It
-- | also makes illegal combinations checkable: round-robin on a `CountCV`
-- | module is not a preference but an incoherence, because it opens holes in
-- | the middle that the hardware cannot express, and notes you released would
-- | go on sounding. See `check`.
module Reef.Voices
  ( Silencing(..)
  , Assign(..)
  , Overflow(..)
  , Release(..)
  , Instrument
  , check
  , saich
  , Slot
  , Voices
  , empty
  , sounding
  , Action(..)
  , Emit
  , noteOn
  , expireAt
  , allOff
  , mixVoltsFor
  ) where

import Prelude

import Data.Array (catMaybes, filter, index, length, mapWithIndex, range, sortBy, updateAt)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)

-- | How a module can stop a voice sounding.
-- |
-- | This is the axis that constrains the others, because it decides whether a
-- | silent voice can sit BETWEEN two sounding ones.
data Silencing
  = PerVoiceGate
  -- ^ Each voice has its own gate or VCA — an FH-2 polyenv, most poly synths.
  -- Any voice can be silenced independently, so holes are free.
  | CountCV { plateaus :: Array Number, rampMs :: Number }
  -- ^ One shared control sets how many voices reach the output, removing them
  -- from one end. `plateaus` is indexed BY VOICE COUNT, silence first, so it
  -- has `voices + 1` entries. `rampMs` is how long to take travelling between
  -- them, and it IS the note-off envelope: such mixers crossfade rather than
  -- step (the Saïch's 2→3 fade is 1.1 V wide), so the hardware supplies the
  -- fade and this decides its duration.
  | AlwaysDroning
  -- ^ No per-voice control of any kind: a bare VCO patched to V/oct only. It
  -- can be repitched but never silenced, so it has no note-off. Worth naming
  -- rather than pretending otherwise — it is what a plain CV destination is.

derive instance eqSilencing :: Eq Silencing

-- | Which voice takes a new note.
data Assign
  = LowestFree
  -- ^ The lowest idle voice. Keeps the sounding set contiguous from voice 0,
  -- which `CountCV` requires.
  | RoundRobin
  -- ^ The next voice after the last one used, wrapping. Spreads work across
  -- oscillators so a repeated note does not retrigger the same envelope, which
  -- matters on analogue voices with a long release.

derive instance eqAssign :: Eq Assign

-- | What happens when every voice is busy.
data Overflow
  = Drop
  -- ^ Refuse the note. A missing note is a quieter mistake than a stolen one,
  -- and on a rig whose polyphony ceiling is known the honest fix is upstream.
  | StealOldest
  -- ^ Take the voice whose note started longest ago.
  | StealNearest
  -- ^ Take the voice whose current pitch is closest to the arriving note, so
  -- the oscillator jumps the shortest distance.

derive instance eqOverflow :: Eq Overflow

-- | What happens to the gap a finished note leaves.
data Release
  = Compact
  -- ^ Migrate survivors down so the sounding set stays contiguous from voice 0.
  -- Required by `CountCV`; pointless but harmless otherwise.
  | LeaveHole
  -- ^ Let the voice fall silent where it is. Only coherent when a voice can be
  -- silenced individually.

derive instance eqRelease :: Eq Release

type Instrument =
  { voices :: Int
  , silencing :: Silencing
  , assign :: Assign
  , overflow :: Overflow
  , release :: Release
  }

-- | Reject policies the hardware cannot honour. `Nothing` means coherent.
-- |
-- | The failures here are not stylistic. A hole in the middle of a `CountCV`
-- | instrument is a note that will not stop, which on stage is a stuck drone
-- | rather than a wrong choice — so it is worth refusing at load time instead of
-- | discovering in a rehearsal.
check :: Instrument -> Maybe String
check inst = case inst.silencing of
  CountCV cv
    | length cv.plateaus /= inst.voices + 1 ->
        Just $ "a CountCV instrument needs one plateau per voice count including "
          <> "silence — " <> show (inst.voices + 1) <> " for "
          <> show inst.voices <> " voices, got " <> show (length cv.plateaus)
    | inst.assign /= LowestFree ->
        Just "a CountCV instrument removes voices from one end, so notes must fill \
             \from voice 0 — round-robin would open holes it cannot silence"
    | inst.release /= Compact ->
        Just "a CountCV instrument cannot silence a voice in the middle, so a \
             \released note must be compacted away — LeaveHole would leave it sounding"
    | otherwise -> Nothing
  AlwaysDroning
    | inst.release == Compact ->
        Just "a droning instrument has no note-off, so there is no hole to compact"
    | otherwise -> Nothing
  PerVoiceGate -> Nothing

-- | The Instruo Saïch as measured on 2026-08-11: four oscillators, one output,
-- | voices arriving 1→4 as the mix CV rises (ES-9 jack 5).
saich :: Instrument
saich =
  { voices: 4
  , silencing: CountCV { plateaus: [ 0.125, 0.675, 1.300, 2.675, 4.350 ], rampMs: 25.0 }
  , assign: LowestFree
  , overflow: Drop
  , release: Compact
  }

-- | One physical voice's occupancy. `onAt` and `offAt` share the caller's
-- | millisecond timebase; `onAt` exists so `StealOldest` has something to sort
-- | by that survives notes of different lengths.
type Slot = { pitch :: Int, onAt :: Number, offAt :: Number }

type Voices =
  { inst :: Instrument
  , slots :: Array (Maybe Slot)
  , nextRR :: Int
  }

empty :: Instrument -> Voices
empty inst =
  { inst, slots: map (const Nothing) (range 1 inst.voices), nextRR: 0 }

sounding :: Voices -> Int
sounding v = length (filter (maybe false (const true)) v.slots)

-- | What the caller must do, and when.
-- |
-- | These carry a time because ORDER is load-bearing. When a note migrates
-- | between oscillators it is briefly sounding on both — set the arriving
-- | voice's pitch first and the departing one covers the move, so the listener
-- | hears the note continuously. Silence first and there is an audible hole.
data Action
  = Pitch Int Int
  -- ^ physical voice index, MIDI note
  | Gate Int Boolean
  -- ^ physical voice index, on/off — only for `PerVoiceGate` instruments
  | Mix Int Number
  -- ^ voice count, and the CV volts that produce it — only for `CountCV`

derive instance eqAction :: Eq Action

instance showAction :: Show Action where
  show = case _ of
    Pitch v p -> "Pitch " <> show v <> " " <> show p
    Gate v on -> "Gate " <> show v <> " " <> show on
    Mix n cv -> "Mix " <> show n <> " " <> show cv

type Emit = { atMs :: Number, action :: Action }

-- | The CV that leaves `n` voices sounding. Zero for instruments with no such
-- | control, whose callers should not be asking.
mixVoltsFor :: Instrument -> Int -> Number
mixVoltsFor inst n = case inst.silencing of
  CountCV cv ->
    let i = if n < 0 then 0 else if n > inst.voices then inst.voices else n
    in fromMaybe 0.0 (index cv.plateaus i)
  _ -> 0.0

-- | Sound a note now.
noteOn :: Number -> Int -> Number -> Voices -> { voices :: Voices, emits :: Array Emit }
noteOn now pitch durMs v =
  case pickVoice now pitch v of
    Nothing -> { voices: v, emits: [] }
    Just i ->
      let
        slot = { pitch, onAt: now, offAt: now + durMs }
        slots' = fromMaybe v.slots (updateAt i (Just slot) v.slots)
        v' = v { slots = slots', nextRR = (i + 1) `mod` v.inst.voices }
        n = length (filter (maybe false (const true)) slots')
      in
        { voices: v'
        -- Pitch before anything that makes the voice audible, so it is already
        -- on the right note when it arrives. The other order sounds a stale one.
        , emits: [ { atMs: now, action: Pitch i pitch } ] <> audible v.inst now i n
        }

-- | The emissions that make voice `i` heard, given how this module silences.
audible :: Instrument -> Number -> Int -> Int -> Array Emit
audible inst now i n = case inst.silencing of
  PerVoiceGate -> [ { atMs: now, action: Gate i true } ]
  CountCV _ -> [ { atMs: now, action: Mix n (mixVoltsFor inst n) } ]
  AlwaysDroning -> []

-- | Choose the voice for an arriving note: `Assign` while one is free, then
-- | `Overflow`.
pickVoice :: Number -> Int -> Voices -> Maybe Int
pickVoice _ pitch v =
  case freeBy v.inst.assign v of
    Just i -> Just i
    Nothing -> case v.inst.overflow of
      Drop -> Nothing
      StealOldest -> minBy _.onAt (occupied v)
      StealNearest -> minBy (\s -> abs' (s.pitch - pitch)) (occupied v)
  where
  abs' n = if n < 0 then -n else n

freeBy :: Assign -> Voices -> Maybe Int
freeBy a v = case a of
  LowestFree -> firstFree 0
  RoundRobin -> firstFreeFrom v.nextRR 0
  where
  isFree i = maybe true isNothing (index v.slots i)
  firstFree i
    | i >= v.inst.voices = Nothing
    | isFree i = Just i
    | otherwise = firstFree (i + 1)
  -- Walk the whole ring from nextRR so a busy run does not fall back to voice 0.
  firstFreeFrom start k
    | k >= v.inst.voices = Nothing
    | isFree ((start + k) `mod` v.inst.voices) = Just ((start + k) `mod` v.inst.voices)
    | otherwise = firstFreeFrom start (k + 1)

occupied :: Voices -> Array { at :: Int, slot :: Slot }
occupied v = catMaybes (mapWithIndex (\i ms -> map (\s -> { at: i, slot: s }) ms) v.slots)

-- | The index of the occupied voice minimising `f`. Ties go to the lowest
-- | index, so stealing is deterministic — two runtimes must agree, and so must
-- | two performances of the same figure.
minBy :: forall b. Ord b => (Slot -> b) -> Array { at :: Int, slot :: Slot } -> Maybe Int
minBy f = map _.at <<< foldl step Nothing
  where
  step acc o = case acc of
    Nothing -> Just o
    Just best -> if f o.slot < f best.slot then Just o else Just best

-- | Retire every note whose time is up, apply `Release`, and say what to emit.
-- | Call whenever time has advanced: a note's end is a deadline set when it
-- | began, not an event the sequencer sends.
expireAt :: Number -> Voices -> { voices :: Voices, emits :: Array Emit }
expireAt now v =
  let
    live = map (\ms -> ms >>= \s -> if s.offAt > now then Just s else Nothing) v.slots
    before = sounding v
    after = length (filter (maybe false (const true)) live)
    goneAt = catMaybes (mapWithIndex (\i ms -> if wasHere i && isNothing ms then Just i else Nothing) live)
    wasHere i = maybe false (maybe false (const true)) (index v.slots i)
  in
    if after == before then { voices: v, emits: [] }
    else case v.inst.silencing, v.inst.release of
      -- Independently gateable: close the gates that ended, leave the rest be.
      PerVoiceGate, _ ->
        { voices: v { slots = live }
        , emits: map (\i -> { atMs: now, action: Gate i false }) goneAt
        }
      -- Nothing can be silenced; the note simply keeps sounding until repitched.
      AlwaysDroning, _ ->
        { voices: v { slots = live }, emits: [] }
      CountCV cv, _ ->
        let
          moves = compact live
          slots' = applyMoves live moves
          pitchEmits = map (\m -> { atMs: now, action: Pitch m.to m.pitch }) moves
          mixEmit = { atMs: now + cv.rampMs, action: Mix after (mixVoltsFor v.inst after) }
        in
          { voices: v { slots = slots' }
          -- Repitch first, then reduce the count: a migrating note is sounding
          -- on its old voice throughout, so it covers its own move; the count
          -- drop then fades that voice out from under it.
          , emits: pitchEmits <> [ mixEmit ]
          }

-- | Silence everything, now.
-- |
-- | Stopping is an ACT, not the absence of ticks. `expireAt` only retires notes
-- | whose time is up, so a transport that simply stops calling it leaves the
-- | last chord sounding for ever — which on a module whose oscillators never
-- | stop is an infinite drone, not a lingering release.
-- |
-- | `AlwaysDroning` returns nothing, honestly: such an instrument has no way to
-- | be silenced and the caller should not believe otherwise.
allOff :: Number -> Voices -> { voices :: Voices, emits :: Array Emit }
allOff now v =
  { voices: v { slots = map (const Nothing) v.slots }
  , emits: case v.inst.silencing of
      PerVoiceGate ->
        map (\o -> { atMs: now, action: Gate o.at false }) (occupied v)
      CountCV _ ->
        if sounding v == 0 then []
        else [ { atMs: now, action: Mix 0 (mixVoltsFor v.inst 0) } ]
      AlwaysDroning -> []
  }

type Move = { from :: Int, to :: Int, pitch :: Int }

-- | Which survivors must migrate so the sounding set occupies voices 0..k-1.
-- |
-- | Survivors already low enough stay put; only those above the new ceiling drop
-- | into the holes. That is the MINIMUM number of oscillators disturbed, and it
-- | matters: the obvious alternative — shuffle everyone down one — moves three
-- | voices where this moves one, and every move is an audible event.
-- |
-- | Movers fill holes in slot order. With at most a handful of voices, searching
-- | for the assignment that minimises total pitch jump would change the outcome
-- | rarely and make it unpredictable, and predictability matters more: the same
-- | figure should migrate the same way each time, or the artefact becomes a
-- | different artefact on every repeat.
compact :: Array (Maybe Slot) -> Array Move
compact live =
  let
    k = length (filter (maybe false (const true)) live)
    movers = filter (\o -> o.at >= k) (catMaybes (mapWithIndex (\i ms -> map (\s -> { at: i, slot: s }) ms) live))
    holes = filter (\i -> maybe true isNothing (index live i)) (upto k)
  in
    zipWith' (\m h -> { from: m.at, to: h, pitch: m.slot.pitch })
      (sortBy (comparing _.at) movers)
      holes

upto :: Int -> Array Int
upto hi = if hi <= 0 then [] else range 0 (hi - 1)

zipWith' :: forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c
zipWith' f as bs = catMaybes (mapWithIndex (\i a -> map (f a) (index bs i)) as)

applyMoves :: Array (Maybe Slot) -> Array Move -> Array (Maybe Slot)
applyMoves live moves =
  let
    cleared = foldl (\acc m -> fromMaybe acc (updateAt m.from Nothing acc)) live moves
    place acc m = case index live m.from of
      Just (Just s) -> fromMaybe acc (updateAt m.to (Just s) acc)
      _ -> acc
  in
    foldl place cleared moves
