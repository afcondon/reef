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
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Reef.Scale (normaliseIvls)
import Data.Traversable (traverse)
import Reef.Gen (GenKind, GenSource, genKinds, rollAllNotes, seedMelody, setAmt, setOn, setRate, toggleGen)
import Reef.Marbles (Seed)
import Reef.Odonus
  ( Odonus
  , clearPitchSet, setOutScale, cyclePattern, setHeadPattern, cycleRoot, cycleScaleType, fanOffsets
  , nudgeOffsets, nudgeHeadPulses, nudgeHeadEuclidSteps
  , setAllNotes, setCellDur, setCellRatchet, setCellVel
  , setDegShift, setHarmony, setScalePattern, releaseScale, followChord, setGatePct, setHeadDir, setHeadEuclidSteps
  , setHeadLen, setHeadMask, setHeadOffset, setHeadPulses, setHeadSpeedIx, setHeadTransp
  , setNote, setNotes, setOctaveShift, setPitchSet, setRandScale, setRoot, setSpread
  , spreadOctaves, staggerLengths, toggleDistribution, toggleGate, toggleGlide, toggleHeadMute
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
  , frozen :: Boolean   -- generation paused (runGen is a no-op); config untouched
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
  | NudgeHeadPulses Int Int        -- head, signed delta (relative — accumulates under buffering)
  | NudgeHeadEuclidSteps Int Int   -- head, signed delta
  | ToggleHeadMute Int
  | SetHeadMask Int
  | CyclePattern Int
  | SetHeadPattern Int Int  -- head, library index (idempotent, for Reef.Move)
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
  -- harmony: a Tidal note pattern the output snaps to past the scale (Reef.Move's
  -- `harmony`, and Vetula's harmonic context). Replaced the chord overlay's feed,
  -- period clock and FollowChord on 2026-10-01; their wire tags now decode to nothing.
  | SetHarmony (Maybe String)
  -- a pattern of Tidal scale names the scale follows (Reef.Move's `scale`);
  -- Nothing returns to the authored scale. Shares the `txt` wire field.
  | SetScalePattern (Maybe String)
  -- a pattern of Tidal scale names the OUTPUT snaps to, past the offsets, on a
  -- root (pitch class): Reef.Move's `outscale`, the router's scale → odonus.out.
  -- Nothing returns the output to the scale. Wire: `txt` and `a`.
  | SetOutScale (Maybe String) Int
  -- what the rig sampled from those two patterns for this step: the chord (pitch
  -- classes; Nothing = none) and, when a scale pattern is set, the scale's steps
  -- (Nothing = leave the scale). The rig samples (it has Tidal) and broadcasts
  -- this tick-tagged, so the browser never reads a pattern (Engine.sampleInput).
  | SetSampled (Maybe (Array Int)) (Maybe (Array Int))
  -- Reichian phase macros
  | UnifyHeads
  | FanOffsets Int
  | StaggerLengths Int
  | SpreadOctaves Int
  | NudgeOffsets Int
  -- gen-source config (mutates the synced gen array)
  | ToggleGen GenKind
  | SetGenOn GenKind Boolean -- idempotent, for Reef.Move
  | SetRate GenKind Int
  | SetAmt GenKind Int
  -- Marbles pad (per-mille on the wire → Number in state)
  | SetGenSpread Int
  | SetGenBias Int
  | SetFrozen Boolean          -- pause / resume ALL generation (config preserved)
  -- one-shot rolls (thread the shared seed)
  | RollAllNotes
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
  NudgeHeadPulses h d -> onOdo (nudgeHeadPulses h d)
  NudgeHeadEuclidSteps h d -> onOdo (nudgeHeadEuclidSteps h d)
  ToggleHeadMute h -> onOdo (toggleHeadMute h)
  SetHeadMask m -> onOdo (setHeadMask m)
  CyclePattern h -> onOdo (cyclePattern h)
  SetHeadPattern h ix -> onOdo (setHeadPattern h ix)
  CycleRoot d -> onOdo (cycleRoot d)
  -- a hand on the scale takes it from a scale pattern (releaseScale)
  CycleScaleType d -> onOdo (cycleScaleType d <<< releaseScale)
  SetRandScale ix -> onOdo (setRandScale ix <<< releaseScale)
  ToggleScaleNote pc -> onOdo (toggleScaleNote pc <<< releaseScale)
  SetSpread k -> onOdo (setSpread k <<< releaseScale)
  ToggleDistribution -> onOdo toggleDistribution
  SetRoot pc -> onOdo (setRoot pc)
  SetOctaveShift n -> onOdo (setOctaveShift n)
  SetDegShift n -> onOdo (setDegShift n)
  SetGatePct n -> onOdo (setGatePct n)
  SetPitchSet ps -> onOdo (setPitchSet ps <<< releaseScale)
  ClearPitchSet -> onOdo clearPitchSet
  SetHarmony h -> onOdo (setHarmony h)
  SetScalePattern p -> onOdo (setScalePattern p)
  SetOutScale p root -> onOdo (setOutScale p root)
  SetSampled c sc -> onOdo \o -> (followChord c o) { scaleIvls = maybe o.scaleIvls normaliseIvls sc }
  UnifyHeads -> onOdo unifyHeads
  FanOffsets n -> onOdo (fanOffsets n)
  StaggerLengths n -> onOdo (staggerLengths n)
  SpreadOctaves n -> onOdo (spreadOctaves n)
  NudgeOffsets d -> onOdo (nudgeOffsets d)
  ToggleGen k -> onGen (toggleGen k)
  SetGenOn k b -> onGen (setOn k b)
  SetRate k v -> onGen (setRate k v)
  SetAmt k v -> onGen (setAmt k v)
  SetGenSpread m -> \s -> s { spread = toNumber m / 1000.0 }
  SetGenBias m -> \s -> s { bias = toNumber m / 1000.0 }
  SetFrozen b -> \s -> s { frozen = b }
  RollAllNotes -> \s ->
    let r = rollAllNotes s.spread s.bias s.odo s.seed
    in s { odo = r.odo, seed = r.seed }
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
  , ps :: Maybe PitchSet       -- SetPitchSet payload
  , txt :: Maybe String        -- SetHarmony / SetScalePattern payload; absent on older frames
  , chord :: Maybe (Array Int) -- SetSampled's chord; absent on older frames
  , ivls :: Maybe (Array Int)  -- SetSampled's scale steps; absent on older frames
  }

w0 :: WireInput
w0 = { tag: "", a: 0, b: 0, ns: [], ps: Nothing, txt: Nothing, chord: Nothing, ivls: Nothing }

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
  NudgeHeadPulses h d -> w0 { tag = "NudgeHeadPulses", a = h, b = d }
  NudgeHeadEuclidSteps h d -> w0 { tag = "NudgeHeadEuclidSteps", a = h, b = d }
  ToggleHeadMute h -> w0 { tag = "ToggleHeadMute", a = h }
  SetHeadMask m -> w0 { tag = "SetHeadMask", a = m }
  CyclePattern h -> w0 { tag = "CyclePattern", a = h }
  SetHeadPattern h ix -> w0 { tag = "SetHeadPattern", a = h, b = ix }
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
  SetHarmony h -> w0 { tag = "SetHarmony", txt = h }
  SetScalePattern p -> w0 { tag = "SetScalePattern", txt = p }
  SetOutScale p root -> w0 { tag = "SetOutScale", txt = p, a = root }
  SetSampled c sc -> w0 { tag = "SetSampled", chord = c, ivls = sc }
  UnifyHeads -> w0 { tag = "UnifyHeads" }
  FanOffsets n -> w0 { tag = "FanOffsets", a = n }
  StaggerLengths n -> w0 { tag = "StaggerLengths", a = n }
  SpreadOctaves n -> w0 { tag = "SpreadOctaves", a = n }
  NudgeOffsets d -> w0 { tag = "NudgeOffsets", a = d }
  ToggleGen k -> w0 { tag = "ToggleGen", a = kindCode k }
  SetGenOn k b -> w0 { tag = "SetGenOn", a = kindCode k, b = if b then 1 else 0 }
  SetRate k v -> w0 { tag = "SetRate", a = kindCode k, b = v }
  SetAmt k v -> w0 { tag = "SetAmt", a = kindCode k, b = v }
  SetGenSpread m -> w0 { tag = "SetGenSpread", a = m }
  SetGenBias m -> w0 { tag = "SetGenBias", a = m }
  SetFrozen b -> w0 { tag = "SetFrozen", a = if b then 1 else 0 }
  RollAllNotes -> w0 { tag = "RollAllNotes" }
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
  "NudgeHeadPulses" -> Just (NudgeHeadPulses w.a w.b)
  "NudgeHeadEuclidSteps" -> Just (NudgeHeadEuclidSteps w.a w.b)
  "ToggleHeadMute" -> Just (ToggleHeadMute w.a)
  "SetHeadMask" -> Just (SetHeadMask w.a)
  "CyclePattern" -> Just (CyclePattern w.a)
  "SetHeadPattern" -> Just (SetHeadPattern w.a w.b)
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
  "SetHarmony" -> Just (SetHarmony w.txt)
  "SetScalePattern" -> Just (SetScalePattern w.txt)
  "SetOutScale" -> Just (SetOutScale w.txt w.a)
  "SetSampled" -> Just (SetSampled w.chord w.ivls)
  "UnifyHeads" -> Just UnifyHeads
  "FanOffsets" -> Just (FanOffsets w.a)
  "StaggerLengths" -> Just (StaggerLengths w.a)
  "SpreadOctaves" -> Just (SpreadOctaves w.a)
  "NudgeOffsets" -> Just (NudgeOffsets w.a)
  "ToggleGen" -> map ToggleGen (kindOf w.a)
  "SetGenOn" -> map (\k -> SetGenOn k (w.b /= 0)) (kindOf w.a)
  "SetRate" -> map (\k -> SetRate k w.b) (kindOf w.a)
  "SetAmt" -> map (\k -> SetAmt k w.b) (kindOf w.a)
  "SetGenSpread" -> Just (SetGenSpread w.a)
  "SetGenBias" -> Just (SetGenBias w.a)
  "SetFrozen" -> Just (SetFrozen (w.a /= 0))
  "RollAllNotes" -> Just RollAllNotes
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
  , frozen :: Boolean
  }

toWireSim :: SimState -> WireSim
toWireSim s =
  { gen: map (\g -> { kind: kindCode g.kind, on: g.on, rate: g.rate, amt: g.amt }) s.gen
  , spreadMille: round (s.spread * 1000.0)
  , biasMille: round (s.bias * 1000.0)
  , odo: s.odo
  , seedInt: round s.seed
  , frozen: s.frozen
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
    , frozen: w.frozen
    }
