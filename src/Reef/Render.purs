-- | `Reef.Render` — the deterministic MIDI-articulation layer, shared so the
-- | frontend monitor and the BEAM `reef_voice` render a fired note IDENTICALLY
-- | (lockstep fidelity, P4f). The generative MODEL (which head fires which pitch on
-- | which step) is `Reef.Engine.stepTick`; this is the next stage down — turning a
-- | `Fired` into the actual note events, with gate length and ratchet retriggers.
-- |
-- | Everything here is a pure function of the `Odonus` record (gate %, per-head
-- | speed, per-cell ratchet/glide) plus the step length — all of which already
-- | travel to the rig in the SimState handoff. So the rig can render with full
-- | fidelity from state it already has; `reef_voice` simply wasn't using it.
-- |
-- | Swing (a groove offset that lives in frontend State, not the model) and glide
-- | (a stateful held-note articulation) are handled by the caller — see the plan's
-- | P4f stages 2 and 3.
module Reef.Render
  ( Hit
  , gateMs
  , ratchetHits
  , renderHits
  , subOffsetMs
  ) where

import Prelude

import Data.Array (range, (!!))
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Reef.Odonus (Fired, Odonus, speedOf)

-- | One scheduled note for a fired cell: `offsetMs` from the step's onset, and how
-- | long it sounds. A plain gated note is a single hit at offset 0; a ratcheted
-- | cell is several evenly-spaced hits filling the gate window.
type Hit = { offsetMs :: Number, durMs :: Number }

-- | The gate window (ms) for a fired note: the step spacing, divided by the head's
-- | speed (a faster head plays shorter notes), scaled by the gate %. Matches the
-- | frontend's `gateMsFor` exactly (>100% gatePct = legato overlap).
gateMs :: Odonus -> Number -> Fired -> Number
gateMs odo stepMs f =
  let spd = maybe 1.0 speedOf (odo.heads !! f.headIdx)
  in stepMs / max 1.0 spd * (toNumber odo.gatePct / 100.0) * toNumber f.dur

-- | Subdivide the gate window into the cell's `ratchet` evenly-spaced retriggers,
-- | each sounding 85% of its slot (so they stay articulate). A single hit when
-- | ratchet ≤ 1 — or when the cell glides, since a slide is one sustained event
-- | (glide and ratchet don't mix; the frontend only ratchets non-glide cells).
ratchetHits :: Fired -> Number -> Array Hit
ratchetHits f gate =
  let rat = if f.ratchet < 1 then 1 else f.ratchet
  in if f.glide || rat <= 1 then [ { offsetMs: 0.0, durMs: gate } ]
     else
       let sub = gate / toNumber rat
       in map (\k -> { offsetMs: toNumber k * sub, durMs: sub * 0.85 }) (range 0 (rat - 1))

-- | Where inside its model step a fired note falls, in ms: 0 for a head at
-- | speed 1 or below, and between 0 and the step for the extra ticks of a
-- | faster head (`Reef.Odonus.headTicks`).
subOffsetMs :: Number -> Fired -> Number
subOffsetMs stepMs f =
  if f.offDen <= 0 then 0.0 else stepMs * toNumber f.offNum / toNumber f.offDen

-- | The full schedule for a fired note: its ratchet hits over the gate window,
-- | placed at the note's own tick inside the step. Both runtimes iterate these,
-- | scheduling each hit at the step's onset + `offsetMs`.
renderHits :: Odonus -> Number -> Fired -> Array Hit
renderHits odo stepMs f =
  map (\h -> h { offsetMs = h.offsetMs + subOffsetMs stepMs f }) (ratchetHits f (gateMs odo stepMs f))
