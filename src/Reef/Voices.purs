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
  , Order(..)
  , Instrument
  , DecayMap
  , decayVoltsFor
  , check
  , saich
  , rings
  , qd
  , rample
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
import Reef.Numeric (ln)

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
  | PerVoiceStrike { settleMs :: Number, triggerMs :: Number, decay :: Maybe DecayMap }
  -- ^ n voices, each with its own pitch bus and its own trigger, and no
  -- note-off: a bank of struck things. Four monophonic resonators, or a quad
  -- sample player like the vpme QD.
  --
  -- WE allocate here — that is the whole difference from `SelfAllocating` — so
  -- every policy applies, and two of them finally earn themselves:
  --
  --   * `RoundRobin` / `StealOldest` become musically right rather than merely
  --     legal. Re-striking a voice cuts its decay short, so the longest-idle
  --     one is the one to take. On a `CountCV` module round-robin is REFUSED
  --     (it opens holes the hardware cannot silence); here it is the default.
  --   * A chord arrives at ONE INSTANT, because n buses hold n pitches. That is
  --     the direct payoff over `SelfAllocating`, whose strum spread was never a
  --     musical choice — it was one wire.
  --
  -- `ByPitch` is incoherent: a struck voice holds a sounding sample and cannot
  -- be re-seated, so pitch order could only be honoured by re-striking.
  | SelfAllocating { settleMs :: Number, triggerMs :: Number, strumMs :: Number }
  -- ^ The module allocates for itself, behind a single note input: one pitch
  -- bus and one trigger. Rings in polyphonic mode, Yarns, Plaits — set a pitch,
  -- fire the trigger, and the module decides which of ITS voices takes the note
  -- and which it steals.
  --
  -- So this side does no allocation at all. It never refuses a note, has no
  -- note-off, and cannot say how many voices are sounding — which is why none
  -- of the constructors above fits, and why the fields here are all about TIME
  -- rather than about voices:
  --
  --   * `settleMs` — how far the pitch must LEAD the trigger. The module
  --     samples the CV at the trigger edge, so a simultaneous pair gives it
  --     whatever the bus held a moment ago: the previous note, played twice.
  --     This is the one field the hardware dictates.
  --   * `triggerMs` — pulse width the module will see.
  --   * `strumMs` — the gap between successive notes. A chord cannot arrive at
  --     one instant here, because two notes need two edges with two different
  --     pitches between them; it is spread, and the input is called STRUM for
  --     exactly that reason. `check` holds it at or above `settleMs +
  --     triggerMs`, below which the notes would overlap and one would take the
  --     other's pitch.
  --
  -- Deliberately NOT modelled: which of the module's own voices holds what. We
  -- cannot observe it, and a model of it would be believed.

derive instance eqSilencing :: Eq Silencing

-- | Gate length as a control voltage, for a voice whose decay is a CV input.
-- |
-- | Deliberately NOT a calibration table, and this is the one place all session
-- | where that is the right answer. A decay CV scales the envelope of whatever
-- | SAMPLE is loaded, so there is no module-level volts-to-milliseconds truth
-- | to measure — it would be per-sample, and the samples are the part being
-- | played with. So this is a musical control with endpoints dialled by ear,
-- | and the artefact should not pretend otherwise.
-- |
-- | Logarithmic between the endpoints, because decay is heard that way: the
-- | step from 50 ms to 100 ms is the step from 1 s to 2 s.
type DecayMap = { minMs :: Number, maxMs :: Number, minV :: Number, maxV :: Number }

-- | The decay CV for a note of this length, clamped to the map's range.
decayVoltsFor :: DecayMap -> Number -> Number
decayVoltsFor m durMs =
  let
    lo = max 1.0 m.minMs
    hi = max (lo + 1.0) m.maxMs
    d = if durMs < lo then lo else if durMs > hi then hi else durMs
    t = ln (d / lo) / ln (hi / lo)
  in
    m.minV + t * (m.maxV - m.minV)

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

-- | Where a note SITS among the voices, as distinct from which voice it was
-- | given when it arrived.
data Order
  = Arrival
  -- ^ Notes stay on whatever voice they were assigned, and move only when
  -- compaction forces it. Disturbs the fewest oscillators, which is the fewest
  -- audible events.
  | ByPitch
  -- ^ Voice 0 always holds the lowest sounding note, voice 1 the next, and so
  -- on. The exact OPPOSITE trade to `Arrival`: it disturbs as many oscillators
  -- as it takes, because a new bass note shifts every other voice up one.
  --
  -- Worth that cost when a voice's IDENTITY matters outside the allocator —
  -- splitting voice 0's CV to double the bass line on another oscillator, say.
  -- Then "voice 0" has to mean something stable, and arrival order does not.

derive instance eqOrder :: Eq Order

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
  , order :: Order
  }

-- | Reject policies the hardware cannot honour. `Nothing` means coherent.
-- |
-- | The failures here are not stylistic. A hole in the middle of a `CountCV`
-- | instrument is a note that will not stop, which on stage is a stuck drone
-- | rather than a wrong choice — so it is worth refusing at load time instead of
-- | discovering in a rehearsal.
check :: Instrument -> Maybe String
check inst
  -- Pitch order decides where a note sits, so there is nothing left for an
  -- assignment policy to choose. Accepting both would silently honour one.
  | inst.order == ByPitch && inst.assign /= LowestFree =
      Just "ByPitch decides a note's voice from its pitch, so it cannot also be \
           \assigned round-robin — one of the two would be silently ignored"
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
  -- Every allocation policy is vacuous here, and a vacuous setting is worse
  -- than a rejected one: it looks like it did something. The whole point of
  -- splitting capability from policy is that the combination is checkable, so
  -- refuse rather than silently ignore.
  SelfAllocating sa
    | inst.voices /= 1 ->
        Just "a self-allocating instrument is driven through ONE pitch bus and one \
             \trigger, however many voices it has internally — set voices to 1 and \
             \let the module do its own allocating"
    | inst.assign /= LowestFree ->
        Just "a self-allocating instrument chooses its own voice, so there is \
             \nothing here for an assignment policy to decide"
    | inst.overflow /= Drop ->
        Just "a self-allocating instrument never runs out from this side — it \
             \steals internally — so an overflow policy would never be reached"
    | inst.release /= LeaveHole ->
        Just "a self-allocating instrument has no note-off, so there is no hole to \
             \compact and nothing to compact it into"
    | inst.order /= Arrival ->
        Just "a self-allocating instrument has one bus, so there is no seating to \
             \order — its voices are not addressable from here"
    | sa.strumMs < sa.settleMs + sa.triggerMs ->
        Just $ "strumMs must be at least settleMs + triggerMs ("
          <> show (sa.settleMs + sa.triggerMs) <> " ms), or two notes of a chord \
             \overlap and the second takes the first's pitch"
    | otherwise -> Nothing
  -- We allocate here, so Assign and Overflow are live choices and nothing is
  -- refused on their account. The two that ARE refused both come from the same
  -- fact: a struck voice holds a sounding sample and cannot be moved.
  PerVoiceStrike _
    | inst.release /= LeaveHole ->
        Just "a struck voice cannot be compacted — the sound is in that voice and \
             \moving the note would mean striking it again somewhere else"
    | inst.order /= Arrival ->
        Just "a struck voice cannot be re-seated by pitch — it is already sounding, \
             \so pitch order could only be honoured by re-striking it"
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
  , order: Arrival
  }

-- | Mutable Instruments Rings in POLYPHONIC mode (2 or 4 voices, set on the
-- | module — we neither know nor need to know which).
-- |
-- | Only correct in polyphonic mode. In monophonic mode Rings tracks V/oct
-- | CONTINUOUSLY, so setting the pitch for a new note would bend the note still
-- | ringing; polyphonic mode latches the pitch at the strum edge, which is what
-- | makes a stream of independent notes possible through one input.
-- |
-- | `settleMs` is a conservative 4 ms — a CV set on the previous scheduler pass
-- | is long settled, and the cost of being generous is only strum spread.
rings :: Instrument
rings =
  { voices: 1
  , silencing: SelfAllocating { settleMs: 4.0, triggerMs: 5.0, strumMs: 12.0 }
  , assign: LowestFree
  , overflow: Drop
  , release: LeaveHole
  , order: Arrival
  }

-- | The vpme QD (Quad Drum Voice) with its extender: four voices, each with a
-- | trigger, a pitch CV and a decay CV.
-- |
-- | Only an INSTRUMENT when the four voices hold the same sample — a marimba in
-- | all four is a polyphonic marimba. Load four different drum samples and it is
-- | not this at all but a drumkit, where a note picks its voice by identity and
-- | nothing is allocated. The SD card decides which, which is a good reason for
-- | the card's contents and this setting to be versioned together.
-- |
-- | The decay endpoints are a starting guess to be dialled by ear; see
-- | `DecayMap` for why they are not measured.
-- | Squarp Rample, playing a sliced card.
-- |
-- | Structurally the QuadDrum's twin — four independent voices, each its own
-- | output, each silenced by being re-struck — and it differs in two ways that
-- | both come from measurement rather than from the manual.
-- |
-- | `settleMs` is **40**, and it is not a guess about MIDI jitter: a note on
-- | this module is a start-point CC followed by a trigger, and the CC must land
-- | first or the trigger plays the previous slice. Forty milliseconds was
-- | measured to be enough on 2026-09-08; how much less would do is not known,
-- | so this is the figure that is known to work rather than the smallest one.
-- |
-- | And there is no decay CV. The Rample's decay is whatever the sample does,
-- | which on a sliced card is the slice's own length — so unlike the QuadDrum
-- | there is nothing to send, and `Decay` actions must never be emitted for it.
rample :: Instrument
rample =
  { voices: 4
  , silencing: PerVoiceStrike
      { settleMs: 40.0
      , triggerMs: 5.0
      , decay: Nothing
      }
  -- Longest-idle first, and steal the longest-idle when full. Re-triggering a
  -- voice cuts the note it was playing, so both are the same instinct: leave a
  -- sounding note alone for as long as there is anything else to use.
  , assign: RoundRobin
  , overflow: StealOldest
  -- Each voice has its own output and can be re-struck alone, so a silent
  -- voice between two sounding ones costs nothing. Compaction would move a
  -- note that is still ringing to a different output, which is worse.
  , release: LeaveHole
  , order: Arrival
  }

qd :: Instrument
qd =
  { voices: 4
  , silencing: PerVoiceStrike
      { settleMs: 2.0
      , triggerMs: 5.0
      , decay: Just { minMs: 40.0, maxMs: 2000.0, minV: 0.0, maxV: 5.0 }
      }
  -- Longest-idle first, and steal the longest-idle when full: re-striking a
  -- voice cuts its decay, so both policies are the same musical instinct.
  , assign: RoundRobin
  , overflow: StealOldest
  , release: LeaveHole
  , order: Arrival
  }

-- | One physical voice's occupancy. `onAt` and `offAt` share the caller's
-- | millisecond timebase; `onAt` exists so `StealOldest` has something to sort
-- | by that survives notes of different lengths.
type Slot = { pitch :: Int, onAt :: Number, offAt :: Number }

type Voices =
  { inst :: Instrument
  , slots :: Array (Maybe Slot)
  , nextRR :: Int
  , lastAt :: Number
  -- ^ When the most recent note was PLACED, which is not always `now`: a
  -- self-allocating instrument spreads a chord, so the third note of one is
  -- scheduled ahead of the moment it arrived. Unused by every other capability,
  -- which places notes the instant they come.
  }

empty :: Instrument -> Voices
empty inst =
  { inst, slots: map (const Nothing) (range 1 inst.voices), nextRR: 0, lastAt: 0.0 }

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
  | Decay Int Number
  -- ^ physical voice index, decay CV volts — only for `PerVoiceStrike` with a
  -- decay map. Must precede the trigger, like `Pitch`: the module samples at
  -- the edge.
  | Trigger Int Number
  -- ^ physical voice index, pulse width in ms — only for `SelfAllocating`.
  --
  -- Distinct from `Gate _ true` because it is not an interval with an end: a
  -- gate says "this note is sounding NOW", a trigger says "take the pitch on
  -- the bus". Two notes in one chord need two edges, so the pulse must FALL
  -- between them — which a gate, being held, cannot do.

derive instance eqAction :: Eq Action

instance showAction :: Show Action where
  show = case _ of
    Pitch v p -> "Pitch " <> show v <> " " <> show p
    Gate v on -> "Gate " <> show v <> " " <> show on
    Mix n cv -> "Mix " <> show n <> " " <> show cv
    Decay v cv -> "Decay " <> show v <> " " <> show cv
    Trigger v ms -> "Trigger " <> show v <> " " <> show ms

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
noteOn now pitch durMs v = case v.inst.silencing of
  SelfAllocating sa -> strum sa now pitch v
  PerVoiceStrike ps -> strike ps now pitch durMs v
  _ -> allocate now pitch durMs v

-- | Strike one voice of a bank of struck things: set its decay and its pitch,
-- | let them settle, fire its trigger.
-- |
-- | Unlike `strum` this allocates properly, so `Assign` and `Overflow` decide
-- | which voice — and unlike `allocate` there is no note-off to schedule, so the
-- | slot's end time exists only to make a voice available again and to give
-- | `StealOldest` something to sort by.
-- |
-- | Nothing is emitted for the OTHER voices. They are sounding samples that
-- | cannot be moved, and re-pitching one would bend a note already in flight.
strike
  :: { settleMs :: Number, triggerMs :: Number, decay :: Maybe DecayMap }
  -> Number -> Int -> Number -> Voices -> { voices :: Voices, emits :: Array Emit }
strike ps now pitch durMs v = case pickVoice now pitch v of
  Nothing -> { voices: v, emits: [] }
  Just i ->
    let
      slot = { pitch, onAt: now, offAt: now + durMs }
      placed = fromMaybe v.slots (updateAt i (Just slot) v.slots)
    in
      { voices: v { slots = placed, nextRR = (i + 1) `mod` v.inst.voices, lastAt = now }
      , emits:
          maybe [] (\m -> [ { atMs: now, action: Decay i (decayVoltsFor m durMs) } ]) ps.decay
            <> [ { atMs: now, action: Pitch i pitch }
               , { atMs: now + ps.settleMs, action: Trigger i ps.triggerMs }
               ]
      }

-- | Hand a note to an instrument that allocates for itself: set the pitch, wait
-- | for it to settle, fire the trigger.
-- |
-- | No slot is taken, because nothing here is allocated — the only state that
-- | moves is `lastAt`, and it exists so a chord SPREADS. Two notes arriving at
-- | the same instant cannot both be strummed at it: one bus can hold one pitch,
-- | so the second waits `strumMs` and the chord becomes an arpeggio a few
-- | milliseconds wide. That is not a workaround; it is what strumming is.
-- |
-- | The duration is dropped, and deliberately: this instrument decides how long
-- | its own notes ring. Pretending otherwise would put a note-off in the model
-- | that nothing could send.
strum
  :: { settleMs :: Number, triggerMs :: Number, strumMs :: Number }
  -> Number -> Int -> Voices -> { voices :: Voices, emits :: Array Emit }
strum sa now pitch v =
  let at = max now (v.lastAt + sa.strumMs)
  in
    { voices: v { lastAt = at }
    , emits:
        [ { atMs: at, action: Pitch 0 pitch }
        , { atMs: at + sa.settleMs, action: Trigger 0 sa.triggerMs }
        ]
    }

allocate :: Number -> Int -> Number -> Voices -> { voices :: Voices, emits :: Array Emit }
allocate now pitch durMs v =
  case pickVoice now pitch v of
    Nothing -> { voices: v, emits: [] }
    Just i ->
      let
        slot = { pitch, onAt: now, offAt: now + durMs }
        placed = fromMaybe v.slots (updateAt i (Just slot) v.slots)
        seated = reorder v.inst placed
        v' = v { slots = seated, nextRR = (i + 1) `mod` v.inst.voices, lastAt = now }
        n = length (filter (maybe false (const true)) seated)
      in
        { voices: v'
        -- Pitch before anything that makes the voice audible, so every voice is
        -- already on the right note when it arrives. The other order sounds a
        -- stale one — and under ByPitch there may be several to settle, because
        -- a new bass note shifts everything above it up a voice.
        , emits: pitchDiffs now v.slots seated <> audible v.inst now i n
        }

-- | The emissions that make voice `i` heard, given how this module silences.
audible :: Instrument -> Number -> Int -> Int -> Array Emit
audible inst now i n = case inst.silencing of
  PerVoiceGate -> [ { atMs: now, action: Gate i true } ]
  CountCV _ -> [ { atMs: now, action: Mix n (mixVoltsFor inst n) } ]
  AlwaysDroning -> []
  -- Unreachable: `strum` never goes through the allocator. Listed rather than
  -- caught by a wildcard so that adding a capability breaks this instead of
  -- silently emitting nothing.
  SelfAllocating _ -> []
  -- Unreachable: `strike` emits its own trigger, since only it knows the voice.
  PerVoiceStrike _ -> []

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
      -- No slots are ever filled, so this cannot be reached — and if it were,
      -- there is no note-off to send: the module rings its own notes out and
      -- steals from itself when it needs the voice.
      SelfAllocating _, _ ->
        { voices: v, emits: [] }
      -- The slot frees so the voice can be struck again; nothing is emitted,
      -- because the sample rings out on its own envelope. `LeaveHole` is the
      -- only coherent release here and `check` enforces it.
      PerVoiceStrike _, _ ->
        { voices: v { slots = live }, emits: [] }
      CountCV cv, _ ->
        let
          slots' = reorder v.inst (applyMoves live (compact live))
          pitchEmits = pitchDiffs now v.slots slots'
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
      -- Nothing to send, and saying so is the honest answer: the module's decay
      -- is its own. A stop leaves what is ringing to ring out, which is a
      -- release, not the infinite drone a `CountCV` instrument gives you.
      SelfAllocating _ -> []
      -- Same as a resonator: what is sounding rings out. There is no off.
      PerVoiceStrike _ -> []
  }

-- | Re-seat the live notes according to `Order`.
-- |
-- | Applied AFTER the arrival-order arrangement rather than instead of it, so
-- | the two policies share one implementation of counting, compaction and
-- | overflow, and only the seating differs. It cannot change how many voices
-- | sound, so nothing downstream of the count is affected.
reorder :: Instrument -> Array (Maybe Slot) -> Array (Maybe Slot)
reorder inst slots = case inst.order of
  Arrival -> slots
  ByPitch ->
    let sorted = sortBy (comparing _.pitch) (catMaybes slots)
    in map (\i -> index sorted i) (upto inst.voices)

-- | One `Pitch` emission per voice whose note changed, comparing before to
-- | after.
-- |
-- | Diffing rather than reporting the moves means a voice that ends up back
-- | where it started emits nothing, and — under `ByPitch` — a cascade that
-- | shifts three notes up produces exactly three emissions rather than a
-- | re-statement of every voice. A redundant repitch is not silent on an
-- | analogue oscillator; it is a discontinuity.
pitchDiffs :: Number -> Array (Maybe Slot) -> Array (Maybe Slot) -> Array Emit
pitchDiffs now before after =
  catMaybes (mapWithIndex step after)
  where
  step i ms = case ms of
    Nothing -> Nothing
    Just s ->
      case index before i of
        Just (Just old) | old.pitch == s.pitch -> Nothing
        _ -> Just { atMs: now, action: Pitch i s.pitch }

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
