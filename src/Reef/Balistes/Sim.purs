-- | `Reef.Balistes.Sim` — the shared Balistes simulation: the engine step (tick)
-- | and the *render decision* (which MIDI note/velocity/timing/ratchet each fired
-- | Grids trigger becomes), as ONE source compiled to both the Triggerfish JS
-- | frontend and the purerl-tidal BEAM voice. This is the Balistes analogue of
-- | `Reef.Engine` + `Reef.Render` for Odonus.
-- |
-- | Phase 0 unified the pure Grids *engine* (`Reef.Balistes.Engine`). The lockstep
-- | co-simulation needs more: the frontend and the rig must also agree on the
-- | overlay decisions the module makes on emit — open-hat conversion, per-lane note
-- | mapping, the J-Dilla push, ratchet subdivision, accent velocity. If those lived
-- | in two hand-ports they could silently diverge (the exact trap Phase 0 removed
-- | from the RNG), so they live here and both runtimes call them. The frontend keeps
-- | the authoring overlay (snapshots, sequence) it doesn't share; `stepBal` and
-- | `renderStep` are row-polymorphic so its richer record flows through untouched.
module Reef.Balistes.Sim
  ( EngineRow
  , RenderRow
  , BalSim
  , BEvent
  , defaultBalSim
  , stepBal
  , renderStep
  , opensAt
  , noteOf
  , pushOf
  , ratchetAt
  , instNote
  , accentVel
  , baseVel
  , openGateMs
  , closedGateMs
  , clampI
  ) where

import Prelude

import Data.Array (replicate, (!!)) as A
import Data.Maybe (fromMaybe)
import Reef.Balistes.Engine (Trigger, evaluateStep, freshPerturbations, readDrumMap)

-- | The pure engine state the step function needs. Kept as an open row so the
-- | frontend's superset `Balistes` record (which adds snapshots/sequence) unifies.
type EngineRow r =
  ( x :: Int
  , y :: Int
  , densBd :: Int
  , densSd :: Int
  , densHh :: Int
  , randomness :: Int
  , step :: Int
  , perts :: Array Int
  , rng :: Int
  | r
  )

-- | The overlay fields `renderStep` reads to decide each hit. Open row, same reason.
type RenderRow r =
  ( x :: Int
  , y :: Int
  , notes :: Array Int
  , open :: Int
  , push :: Array Int
  , ratchet :: Array Int
  | r
  )

-- | The full serializable Balistes state pushed to the rig on handoff: the engine
-- | subset + the overlay `renderStep` reads. Snapshots/sequence stay frontend-only
-- | (song-structure authoring), so a running snapshot-sequence won't yet morph on
-- | the rig — a known limitation, not a divergence in the shared path.
type BalSim =
  { x :: Int
  , y :: Int
  , densBd :: Int
  , densSd :: Int
  , densHh :: Int
  , randomness :: Int
  , step :: Int
  , perts :: Array Int
  , rng :: Int
  , notes :: Array Int
  , open :: Int
  , push :: Array Int
  , ratchet :: Array Int
  }

-- | One resolved hit: the concrete MIDI event a fired trigger becomes, before the
-- | runtime schedules it (the frontend via Web MIDI, the BEAM via scheduleNoteAt).
-- | `pushMs` is the signed Dilla offset; `ratchet` is the subdivision count.
type BEvent =
  { note :: Int
  , velocity :: Int
  , pushMs :: Int
  , durMs :: Number
  , ratchet :: Int
  }

-- | A nonzero seed (xorshift fixed-points at 0) matching the frontend default.
initialSeed :: Int
initialSeed = 0x1A2B3C4D

-- | Central node, moderate density, no randomness — the firmware's neutral start,
-- | with the frontend's overlay defaults (a touch of open so the loudest hats
-- | breathe; flat push; GM drum notes; single hits).
defaultBalSim :: BalSim
defaultBalSim =
  let seeded = freshPerturbations 0 initialSeed
  in
    { x: 128
    , y: 128
    , densBd: 192
    , densSd: 150
    , densHh: 170
    , randomness: 0
    , step: 0
    , perts: seeded.perts
    , rng: seeded.rng
    , notes: [ 36, 38, 42, 46 ]
    , open: 70
    , push: [ 0, 0, 0, 0 ]
    -- one ratchet slot per (lane, step) over the 3 Grids lanes (96 = 3×32)
    , ratchet: A.replicate 96 1
    }

-- | Play the current step, then advance. Returns the triggers fired *this* step
-- | (the caller renders + schedules them at the tick's fire-time) and the advanced
-- | state. When the step wraps to 0 a fresh perturbation set is sampled for the new
-- | pattern — the firmware's once-per-pattern-start rule. Row-polymorphic so the
-- | frontend `Balistes` record passes through and the BEAM `BalSim` does too.
stepBal
  :: forall r
   . Record (EngineRow r)
  -> { bal :: Record (EngineRow r), fired :: Array Trigger }
stepBal b =
  let
    fired = evaluateStep b.step b.x b.y [ b.densBd, b.densSd, b.densHh ] b.perts
    nextStep = (b.step + 1) `mod` 32
    b' =
      if nextStep == 0 then
        let s = freshPerturbations b.randomness b.rng
        in b { step = nextStep, perts = s.perts, rng = s.rng }
      else
        b { step = nextStep }
  in
    { bal: b', fired }

-- ── render decision (shared by frontend Component + BEAM voice) ───────────────

accentVel :: Int
accentVel = 120

baseVel :: Int
baseVel = 78

openGateMs :: Number
openGateMs = 200.0

closedGateMs :: Number
closedGateMs = 30.0

clampI :: Int -> Int -> Int -> Int
clampI lo hi v = if v < lo then lo else if v > hi then hi else v

-- | Default GM-ish drum notes (BD=36, SD=38, HH=42, OH=46).
instNote :: Int -> Int
instNote inst = case inst of
  0 -> 36
  1 -> 38
  2 -> 42
  _ -> 46

-- | This state's MIDI note for a lane (0=BD,1=SD,2=HH,3=OH), GM default fallback.
noteOf :: forall r. Int -> { notes :: Array Int | r } -> Int
noteOf inst b = fromMaybe (instNote inst) (b.notes A.!! inst)

-- | Per-lane timing offset in ms (signed: − earlier, + later).
pushOf :: forall r. Int -> { push :: Array Int | r } -> Int
pushOf inst b = fromMaybe 0 (b.push A.!! inst)

-- | The ratchet count for a grid slot (inst, step), >= 1.
ratchetAt :: forall r. Int -> Int -> { ratchet :: Array Int | r } -> Int
ratchetAt inst step b = fromMaybe 1 (b.ratchet A.!! (inst * 32 + step))

-- | Does the HH voice fire OPEN at this step? The open boundary is `255 - open`,
-- | descending from above the landscape as the dial rises — the loudest/most-
-- | stressed hats convert to open first. Uses the deterministic level (the same
-- | landscape the heatmap paints), so visual + audio agree.
opensAt :: forall r. Int -> Record (RenderRow r) -> Boolean
opensAt step b = b.open > 0 && readDrumMap step 2 b.x b.y >= 255 - b.open

-- | Turn a step's fired triggers into concrete MIDI events. A firing HH that clears
-- | the OPEN boundary rings as an open hat (OH note, OH push slot, long gate) and
-- | chokes its closed self; everything else is a short blip. Ratchet roll + per-voice
-- | Dilla push are attached for the runtime to apply on schedule. THIS is the single
-- | decision both the frontend and the rig use — no second copy.
renderStep :: forall r. Record (RenderRow r) -> Int -> Array Trigger -> Array BEvent
renderStep b step fired = map ev fired
  where
  ev t =
    let
      opens = t.inst == 2 && opensAt step b
      note = if opens then noteOf 3 b else noteOf t.inst b
      durMs = if opens then openGateMs else closedGateMs
      pushLane = if opens then 3 else t.inst
    in
      { note
      , velocity: if t.accent then accentVel else baseVel
      , pushMs: pushOf pushLane b
      , durMs
      , ratchet: ratchetAt t.inst step b
      }
