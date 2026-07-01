-- | `Reef.Input` — the live-control surface of the Odonus module as SERIALISABLE
-- | data, the semantic layer the lockstep co-simulation rides on
-- | (reef/docs/PLAN-lockstep-cosimulation.md, P3).
-- |
-- | Lockstep syncs INPUTS, not state: both runtimes run the same deterministic
-- | module from a shared seed and exchange only the sparse user actions, each
-- | tick-tagged ("apply at tick N"). For that, every gesture a user can make
-- | (every setter in `Reef.Odonus` + the gen-config + the one-shot rolls) has to
-- | be a value you can put on the wire and replay — that is the `Input` ADT here.
-- |
-- |   • `Input`      — one constructor per user action (data, not a function).
-- |   • `SimState`   — the WHOLE synced state the module steps: the Odonus record,
-- |                    the gen-source config (`runGen` consumes it), the Marbles
-- |                    pad (bias/spread), and the one shared PRNG seed.
-- |   • `applyInput` — the pure interpreter: `Input -> SimState -> SimState`,
-- |                    dispatching to the existing `Reef.Odonus` / `Reef.Gen`
-- |                    setters. Most inputs touch only `odo`; the gen-config ones
-- |                    touch `gen`; the rolls thread the seed (which is why the
-- |                    interpreter works over `SimState`, not `Odonus`).
-- |   • `Tagged`     — `{ tick, input }`, the tick-tagged unit broadcast on the wire.
-- |
-- | The wire mapping (`toWire`/`fromWire`) flattens the ADT to a single record so
-- | the codec is just simple-json's record instance — the only JSON codec in the
-- | purerl set, and the same flat-record discipline `Reef.Protocol` already uses
-- | for the whole Odonus. `Reef.Protocol` wraps these into encode/decode strings.
module Reef.Input
  ( Input(..)
  , SimState
  , applyInput
  , applyInputs
  , Tagged
  , WireInput
  , w0
  , toWire
  , fromWire
  , WireSim
  , WireGenSource
  , toWireSim
  , fromWireSim
  ) where

import Prelude

import Data.Array (findIndex, (!!))
import Data.Foldable (foldl)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Reef.Gen (GenKind, GenSource, genKinds, rollAllNotes, rollChords, seedMelody, setAmt, setRate, toggleGen)
import Reef.Marbles (Seed)
import Reef.Odonus
  ( Odonus, numChordTable
  , clearPitchSet, cyclePattern, cycleRoot, cycleScaleType, fanOffsets, followChord
  , nudgeOffsets, setAllNotes, setCellDur, setCellRatchet, setCellVel, setChordFeed
  , setChordPeriod, setChordPicks, setDegShift, setGatePct, setHeadDir, setHeadEuclidSteps
  , setHeadLen, setHeadMask, setHeadOffset, setHeadPulses, setHeadSpeedIx, setHeadTransp
  , setNote, setNotes, setOctaveShift, setPitchSet, setRandScale, setRoot, setSpread
  , staggerLengths, toggleChord, toggleDistribution, toggleGate, toggleGlide, toggleHeadMute
  , toggleScaleNote, toggleSkip, unifyHeads
  )
import Reef.PitchSet (PitchSet)

-- ── the synced simulation state ──────────────────────────────────────────────

-- | Everything both runtimes step in lockstep: the sequencer record, the
-- | gen-source config, the Marbles pad (as Numbers, set from integer per-mille
-- | inputs so the wire stays integer), and the single PRNG seed shared by both
-- | the autonomous per-tick `runGen` and the one-shot roll inputs.
type SimState =
  { odo :: Odonus
  , gen :: Array GenSource
  , spread :: Number
  , bias :: Number
  , seed :: Seed
  }

-- ── the input surface ────────────────────────────────────────────────────────

-- | A user action as data. One constructor per `Reef.Odonus` / `Reef.Gen` setter
-- | a UI can drive, so any gesture can be broadcast tick-tagged and replayed
-- | identically on both runtimes.
data Input
  -- cells
  = ToggleSkip Int
  | ToggleGate Int
  | ToggleGlide Int
  | SetNote Int Int          -- cell, value
  | SetAllNotes Int
  | SetNotes (Array Int)
  | SetCellDur Int Int
  | SetCellRatchet Int Int
  | SetCellVel Int Int
  -- heads
  | SetHeadSpeedIx Int Int   -- head, value
  | SetHeadDir Int Int
  | SetHeadTransp Int Int
  | SetHeadOffset Int Int
  | SetHeadLen Int Int
  | SetHeadPulses Int Int
  | SetHeadEuclidSteps Int Int
  | ToggleHeadMute Int
  | SetHeadMask Int
  | CyclePattern Int
  -- quantizer
  | CycleRoot Int            -- direction
  | CycleScaleType Int
  | SetRandScale Int
  | ToggleScaleNote Int      -- pitch-class
  | SetSpread Int            -- scale spread k
  | ToggleDistribution
  | SetRoot Int
  | SetOctaveShift Int
  | SetDegShift Int
  | SetGatePct Int
  | SetPitchSet PitchSet
  | ClearPitchSet
  -- chord overlay
  | SetChordPicks (Array Int)
  | SetChordFeed (Array (Array Int))
  | FollowChord (Maybe (Array Int))
  | ToggleChord
  | SetChordPeriod Int
  -- Reichian phase macros
  | UnifyHeads
  | FanOffsets Int
  | StaggerLengths Int
  | NudgeOffsets Int
  -- gen-source config (mutates the synced gen array)
  | ToggleGen GenKind
  | SetRate GenKind Int
  | SetAmt GenKind Int
  -- Marbles pad (per-mille on the wire → Number in state)
  | SetGenSpread Int
  | SetGenBias Int
  -- one-shot rolls (thread the shared seed)
  | RollAllNotes
  | RollChords
  | SeedMelody

-- | A tick-tagged input: apply it on tick `tick`. Both runtimes apply at exactly
-- | this tick → identical evolution. The frontend tags with `currentTick + buffer`
-- | (a small lookahead so both sides receive it first); see the plan, P4.
type Tagged = { tick :: Int, input :: Input }

-- ── the interpreter ──────────────────────────────────────────────────────────

onOdo :: (Odonus -> Odonus) -> SimState -> SimState
onOdo f s = s { odo = f s.odo }

onGen :: (Array GenSource -> Array GenSource) -> SimState -> SimState
onGen f s = s { gen = f s.gen }

-- | Apply one input to the synced state. Pure: same state + same input → same
-- | next state on either runtime (the lockstep guarantee). The rolls and pad are
-- | the only constructors that touch anything beyond `odo`.
applyInput :: Input -> SimState -> SimState
applyInput = case _ of
  ToggleSkip i -> onOdo (toggleSkip i)
  ToggleGate i -> onOdo (toggleGate i)
  ToggleGlide i -> onOdo (toggleGlide i)
  SetNote i v -> onOdo (setNote i v)
  SetAllNotes v -> onOdo (setAllNotes v)
  SetNotes ns -> onOdo (setNotes ns)
  SetCellDur i v -> onOdo (setCellDur i v)
  SetCellRatchet i v -> onOdo (setCellRatchet i v)
  SetCellVel i v -> onOdo (setCellVel i v)
  SetHeadSpeedIx h v -> onOdo (setHeadSpeedIx h v)
  SetHeadDir h v -> onOdo (setHeadDir h v)
  SetHeadTransp h v -> onOdo (setHeadTransp h v)
  SetHeadOffset h v -> onOdo (setHeadOffset h v)
  SetHeadLen h v -> onOdo (setHeadLen h v)
  SetHeadPulses h v -> onOdo (setHeadPulses h v)
  SetHeadEuclidSteps h v -> onOdo (setHeadEuclidSteps h v)
  ToggleHeadMute h -> onOdo (toggleHeadMute h)
  SetHeadMask m -> onOdo (setHeadMask m)
  CyclePattern h -> onOdo (cyclePattern h)
  CycleRoot d -> onOdo (cycleRoot d)
  CycleScaleType d -> onOdo (cycleScaleType d)
  SetRandScale ix -> onOdo (setRandScale ix)
  ToggleScaleNote pc -> onOdo (toggleScaleNote pc)
  SetSpread k -> onOdo (setSpread k)
  ToggleDistribution -> onOdo toggleDistribution
  SetRoot pc -> onOdo (setRoot pc)
  SetOctaveShift n -> onOdo (setOctaveShift n)
  SetDegShift n -> onOdo (setDegShift n)
  SetGatePct n -> onOdo (setGatePct n)
  SetPitchSet ps -> onOdo (setPitchSet ps)
  ClearPitchSet -> onOdo clearPitchSet
  SetChordPicks ps -> onOdo (setChordPicks ps)
  SetChordFeed pcs -> onOdo (setChordFeed pcs)
  FollowChord mpcs -> onOdo (followChord mpcs)
  ToggleChord -> onOdo toggleChord
  SetChordPeriod v -> onOdo (setChordPeriod v)
  UnifyHeads -> onOdo unifyHeads
  FanOffsets n -> onOdo (fanOffsets n)
  StaggerLengths n -> onOdo (staggerLengths n)
  NudgeOffsets d -> onOdo (nudgeOffsets d)
  ToggleGen k -> onGen (toggleGen k)
  SetRate k v -> onGen (setRate k v)
  SetAmt k v -> onGen (setAmt k v)
  SetGenSpread m -> \s -> s { spread = toNumber m / 1000.0 }
  SetGenBias m -> \s -> s { bias = toNumber m / 1000.0 }
  RollAllNotes -> \s ->
    let r = rollAllNotes s.spread s.bias s.odo s.seed
    in s { odo = r.odo, seed = r.seed }
  RollChords -> \s ->
    let r = rollChords numChordTable s.seed
    in s { odo = setChordPicks r.picks s.odo, seed = r.seed }
  SeedMelody -> \s ->
    let r = seedMelody [] s.odo s.seed
    in s { odo = r.odo, seed = r.seed }

-- | Apply a batch of inputs in order (e.g. every input scheduled for one tick).
applyInputs :: Array Input -> SimState -> SimState
applyInputs ins s = foldl (flip applyInput) s ins

-- ── the flat wire mapping ────────────────────────────────────────────────────

-- | A single flat record every `Input` collapses into, so the codec is just
-- | simple-json's record instance (cross-runtime, no hand-written sum decoder to
-- | drift). `tag` discriminates the constructor; the rest are the union of all
-- | payloads — each constructor fills the subset it needs, the others stay at
-- | their `w0` defaults.
type WireInput =
  { tag :: String
  , a :: Int                   -- first int arg / GenKind code / pitch-class / direction
  , b :: Int                   -- second int arg (value)
  , ns :: Array Int            -- SetNotes / SetChordPicks payload
  , pcs :: Array (Array Int)   -- SetChordFeed; FollowChord uses [theSet] / [] for Just/Nothing
  , ps :: Maybe PitchSet       -- SetPitchSet payload
  }

w0 :: WireInput
w0 = { tag: "", a: 0, b: 0, ns: [], pcs: [], ps: Nothing }

-- GenKind ↔ Int via its position in `genKinds` (stable, the UI's own order).
kindCode :: GenKind -> Int
kindCode k = fromMaybe 0 (findIndex (_ == k) genKinds)

kindOf :: Int -> Maybe GenKind
kindOf i = genKinds !! i

toWire :: Input -> WireInput
toWire = case _ of
  ToggleSkip i -> w0 { tag = "ToggleSkip", a = i }
  ToggleGate i -> w0 { tag = "ToggleGate", a = i }
  ToggleGlide i -> w0 { tag = "ToggleGlide", a = i }
  SetNote i v -> w0 { tag = "SetNote", a = i, b = v }
  SetAllNotes v -> w0 { tag = "SetAllNotes", a = v }
  SetNotes ns -> w0 { tag = "SetNotes", ns = ns }
  SetCellDur i v -> w0 { tag = "SetCellDur", a = i, b = v }
  SetCellRatchet i v -> w0 { tag = "SetCellRatchet", a = i, b = v }
  SetCellVel i v -> w0 { tag = "SetCellVel", a = i, b = v }
  SetHeadSpeedIx h v -> w0 { tag = "SetHeadSpeedIx", a = h, b = v }
  SetHeadDir h v -> w0 { tag = "SetHeadDir", a = h, b = v }
  SetHeadTransp h v -> w0 { tag = "SetHeadTransp", a = h, b = v }
  SetHeadOffset h v -> w0 { tag = "SetHeadOffset", a = h, b = v }
  SetHeadLen h v -> w0 { tag = "SetHeadLen", a = h, b = v }
  SetHeadPulses h v -> w0 { tag = "SetHeadPulses", a = h, b = v }
  SetHeadEuclidSteps h v -> w0 { tag = "SetHeadEuclidSteps", a = h, b = v }
  ToggleHeadMute h -> w0 { tag = "ToggleHeadMute", a = h }
  SetHeadMask m -> w0 { tag = "SetHeadMask", a = m }
  CyclePattern h -> w0 { tag = "CyclePattern", a = h }
  CycleRoot d -> w0 { tag = "CycleRoot", a = d }
  CycleScaleType d -> w0 { tag = "CycleScaleType", a = d }
  SetRandScale ix -> w0 { tag = "SetRandScale", a = ix }
  ToggleScaleNote pc -> w0 { tag = "ToggleScaleNote", a = pc }
  SetSpread k -> w0 { tag = "SetSpread", a = k }
  ToggleDistribution -> w0 { tag = "ToggleDistribution" }
  SetRoot pc -> w0 { tag = "SetRoot", a = pc }
  SetOctaveShift n -> w0 { tag = "SetOctaveShift", a = n }
  SetDegShift n -> w0 { tag = "SetDegShift", a = n }
  SetGatePct n -> w0 { tag = "SetGatePct", a = n }
  SetPitchSet ps -> w0 { tag = "SetPitchSet", ps = Just ps }
  ClearPitchSet -> w0 { tag = "ClearPitchSet" }
  SetChordPicks ps -> w0 { tag = "SetChordPicks", ns = ps }
  SetChordFeed pcs -> w0 { tag = "SetChordFeed", pcs = pcs }
  FollowChord mpcs -> w0 { tag = "FollowChord", pcs = case mpcs of
                                                         Just pcs -> [ pcs ]
                                                         Nothing -> [] }
  ToggleChord -> w0 { tag = "ToggleChord" }
  SetChordPeriod v -> w0 { tag = "SetChordPeriod", a = v }
  UnifyHeads -> w0 { tag = "UnifyHeads" }
  FanOffsets n -> w0 { tag = "FanOffsets", a = n }
  StaggerLengths n -> w0 { tag = "StaggerLengths", a = n }
  NudgeOffsets d -> w0 { tag = "NudgeOffsets", a = d }
  ToggleGen k -> w0 { tag = "ToggleGen", a = kindCode k }
  SetRate k v -> w0 { tag = "SetRate", a = kindCode k, b = v }
  SetAmt k v -> w0 { tag = "SetAmt", a = kindCode k, b = v }
  SetGenSpread m -> w0 { tag = "SetGenSpread", a = m }
  SetGenBias m -> w0 { tag = "SetGenBias", a = m }
  RollAllNotes -> w0 { tag = "RollAllNotes" }
  RollChords -> w0 { tag = "RollChords" }
  SeedMelody -> w0 { tag = "SeedMelody" }

-- | Reconstruct an `Input` from the wire record. `Nothing` on an unknown tag (or
-- | an unresolvable GenKind code) — `Reef.Protocol.decodeInput` lifts that into a
-- | decode error. Total over every tag `toWire` emits, so it round-trips.
fromWire :: WireInput -> Maybe Input
fromWire w = case w.tag of
  "ToggleSkip" -> Just (ToggleSkip w.a)
  "ToggleGate" -> Just (ToggleGate w.a)
  "ToggleGlide" -> Just (ToggleGlide w.a)
  "SetNote" -> Just (SetNote w.a w.b)
  "SetAllNotes" -> Just (SetAllNotes w.a)
  "SetNotes" -> Just (SetNotes w.ns)
  "SetCellDur" -> Just (SetCellDur w.a w.b)
  "SetCellRatchet" -> Just (SetCellRatchet w.a w.b)
  "SetCellVel" -> Just (SetCellVel w.a w.b)
  "SetHeadSpeedIx" -> Just (SetHeadSpeedIx w.a w.b)
  "SetHeadDir" -> Just (SetHeadDir w.a w.b)
  "SetHeadTransp" -> Just (SetHeadTransp w.a w.b)
  "SetHeadOffset" -> Just (SetHeadOffset w.a w.b)
  "SetHeadLen" -> Just (SetHeadLen w.a w.b)
  "SetHeadPulses" -> Just (SetHeadPulses w.a w.b)
  "SetHeadEuclidSteps" -> Just (SetHeadEuclidSteps w.a w.b)
  "ToggleHeadMute" -> Just (ToggleHeadMute w.a)
  "SetHeadMask" -> Just (SetHeadMask w.a)
  "CyclePattern" -> Just (CyclePattern w.a)
  "CycleRoot" -> Just (CycleRoot w.a)
  "CycleScaleType" -> Just (CycleScaleType w.a)
  "SetRandScale" -> Just (SetRandScale w.a)
  "ToggleScaleNote" -> Just (ToggleScaleNote w.a)
  "SetSpread" -> Just (SetSpread w.a)
  "ToggleDistribution" -> Just ToggleDistribution
  "SetRoot" -> Just (SetRoot w.a)
  "SetOctaveShift" -> Just (SetOctaveShift w.a)
  "SetDegShift" -> Just (SetDegShift w.a)
  "SetGatePct" -> Just (SetGatePct w.a)
  "SetPitchSet" -> map SetPitchSet w.ps
  "ClearPitchSet" -> Just ClearPitchSet
  "SetChordPicks" -> Just (SetChordPicks w.ns)
  "SetChordFeed" -> Just (SetChordFeed w.pcs)
  "FollowChord" -> Just (FollowChord (w.pcs !! 0))
  "ToggleChord" -> Just ToggleChord
  "SetChordPeriod" -> Just (SetChordPeriod w.a)
  "UnifyHeads" -> Just UnifyHeads
  "FanOffsets" -> Just (FanOffsets w.a)
  "StaggerLengths" -> Just (StaggerLengths w.a)
  "NudgeOffsets" -> Just (NudgeOffsets w.a)
  "ToggleGen" -> map ToggleGen (kindOf w.a)
  "SetRate" -> map (\k -> SetRate k w.b) (kindOf w.a)
  "SetAmt" -> map (\k -> SetAmt k w.b) (kindOf w.a)
  "SetGenSpread" -> Just (SetGenSpread w.a)
  "SetGenBias" -> Just (SetGenBias w.a)
  "RollAllNotes" -> Just RollAllNotes
  "RollChords" -> Just RollChords
  "SeedMelody" -> Just SeedMelody
  _ -> Nothing

-- ── the SimState handoff wire mapping ─────────────────────────────────────────
--
-- The lockstep HANDOFF (plan P4d): the frontend sends its WHOLE SimState once so
-- the rig's reef_voice picks up from exactly where the frontend is — same Odonus,
-- same gen config, same Marbles pad, SAME SEED (without which the generative
-- matrices would diverge). Like the Input protocol, the wire form is all
-- integers/strings/booleans + the (already-codable) Odonus: the two `Number`s in
-- SimState (spread/bias) travel as per-mille Ints and the integer-valued `seed`
-- travels as an Int, so no bare `Number` crosses the wire (JS and BEAM format
-- Numbers differently). `Reef.Protocol` wraps these into encode/decode strings.

-- | A gen source on the wire: `kind` as its `genKinds` index (see `kindCode`).
type WireGenSource = { kind :: Int, on :: Boolean, rate :: Int, amt :: Int }

-- | The flat wire form of a whole `SimState`.
type WireSim =
  { gen :: Array WireGenSource
  , spreadMille :: Int
  , biasMille :: Int
  , odo :: Odonus
  , seedInt :: Int
  }

toWireSim :: SimState -> WireSim
toWireSim s =
  { gen: map (\g -> { kind: kindCode g.kind, on: g.on, rate: g.rate, amt: g.amt }) s.gen
  , spreadMille: round (s.spread * 1000.0)
  , biasMille: round (s.bias * 1000.0)
  , odo: s.odo
  , seedInt: round s.seed
  }

-- | `Nothing` if any gen-kind code is unresolvable (a corrupt handoff);
-- | `Reef.Protocol.decodeSim` lifts that into a decode error.
fromWireSim :: WireSim -> Maybe SimState
fromWireSim w = do
  gen <- traverse
           (\g -> map (\k -> { kind: k, on: g.on, rate: g.rate, amt: g.amt }) (kindOf g.kind))
           w.gen
  pure
    { gen
    , spread: toNumber w.spreadMille / 1000.0
    , bias: toNumber w.biasMille / 1000.0
    , odo: w.odo
    , seed: toNumber w.seedInt
    }
