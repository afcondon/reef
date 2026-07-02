-- | `Reef.Balistes.Fixed` — the shared eval for Balistes' FIXED-RHYTHM patterns (the
-- | hand-written `lane × step` loops, as opposed to the generative Grids engine). The
-- | fixed-rhythm analogue of `Reef.Balistes.Sim`: one `renderFixed` decision compiled
-- | to both the Triggerfish frontend and the BEAM voice, so a pushed fixed rhythm
-- | plays byte-identically on the rig.
-- |
-- | A fixed pattern is a pure function of the ABSOLUTE step: `fixedStep = absStep mod
-- | steps` picks the column, `loop = absStep / steps` drives trig conditions, and a
-- | deterministic per-(step,lane) hash gates probability. No state, no seed — so both
-- | runtimes, reading the same Link step, agree with no handoff phase concern.
-- |
-- | `TrigCond` is flattened to two ints (`condX`/`condY`; condX = 0 means "always")
-- | so a `Cell` is a flat record simple-json can carry — the frontend's rich
-- | `Triggerfish.Balistes.Pattern` projects onto this for the wire + the shared eval.
module Reef.Balistes.Fixed
  ( Cell
  , FixedPattern
  , kitSize
  , emptyCell
  , cellAt
  , condFires
  , cellHash
  , probPass
  , laneGateMs
  , noteOf
  , usedLanes
  , renderFixed
  ) where

import Prelude

import Data.Array (any, filter, mapMaybe, range, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Reef.Balistes.Sim (BEvent)

-- | The shared 16-lane kit coordinate system (BD…SH); used only to bound lane
-- | iteration here (names/notes live frontend-side in Triggerfish.Balistes.Pattern).
kitSize :: Int
kitSize = 16

-- | One cell, wire-flat: velocity (0 = no hit), firing probability %, the trig
-- | condition as two ints (condX = 0 → always; else fire on pass condX of every
-- | condY, Elektron-style), and the ratchet subdivision count.
type Cell =
  { vel :: Int
  , prob :: Int
  , condX :: Int
  , condY :: Int
  , ratchet :: Int
  }

-- | A literal rhythm: a step count, a dense `kitSize × steps` grid of cells, and a
-- | per-lane MIDI note (length kitSize). Name/kit metadata stay frontend-side.
type FixedPattern =
  { steps :: Int
  , grid :: Array (Array Cell)
  , notes :: Array Int
  }

emptyCell :: Cell
emptyCell = { vel: 0, prob: 100, condX: 0, condY: 0, ratchet: 1 }

cellAt :: FixedPattern -> Int -> Int -> Cell
cellAt p lane step = fromMaybe emptyCell ((p.grid !! lane) >>= (_ !! step))

-- | Does this cell's condition fire on loop pass `loop` (0-based)? condX = 0 is the
-- | serialised `CAlways`; else the `CEvery condX condY` rule.
condFires :: Cell -> Int -> Boolean
condFires c loop =
  if c.condX == 0 then true
  else if c.condY <= 0 then true
  else (loop `mod` c.condY) == ((c.condX - 1) `mod` c.condY)

-- | Deterministic per-(absStep, lane, step) hash 0..99 — reproducible but varying
-- | pass-to-pass as the absolute step grows. CROSS-RUNTIME SAFE: kept WRAP-FREE, every
-- | intermediate < 2^31, so JS's 32-bit Int multiply and the BEAM's bignums agree.
-- | (The original frontend hash multiplied by ~7e7; those products overflow 2^31 and
-- | JS wraps to signed-32 while the BEAM does not — a genuine node↔BEAM divergence the
-- | conformance flagged. `idx mod 9973` bounds the growing index; the mix is arbitrary
-- | — this only gates probability, so any identical-on-both hash serves.)
cellHash :: Int -> Int -> Int -> Int
cellHash idx lane step =
  ((idx `mod` 9973) * 131 + lane * 37 + step * 17) `mod` 100

probPass :: Int -> Int -> Int -> Int -> Boolean
probPass prob idx lane step =
  prob >= 100 || (prob > 0 && cellHash idx lane step < prob)

-- | Gate length per lane: rings for open hat / rides / crash, a blip otherwise.
laneGateMs :: Int -> Number
laneGateMs lane = case lane of
  6 -> 180.0   -- OH
  10 -> 200.0  -- RD
  11 -> 200.0  -- RB
  12 -> 320.0  -- CR
  _ -> 55.0

-- | This pattern's MIDI note for a lane (GM-ish fallback; the projection always
-- | carries all kitSize notes, so the fallback is never hit in practice).
noteOf :: FixedPattern -> Int -> Int
noteOf p lane = fromMaybe (36 + lane) (p.notes !! lane)

-- | Which lanes carry any hit.
usedLanes :: FixedPattern -> Array Int
usedLanes p = filter (\l -> any (\c -> c.vel > 0) (fromMaybe [] (p.grid !! l))) (range 0 (kitSize - 1))

-- | Render one absolute step of a fixed rhythm to concrete MIDI events — the SINGLE
-- | decision both the frontend and the rig use. Fixed rhythms carry no Dilla push
-- | (pushMs = 0); gate + ratchet + note + velocity come straight from the cell.
renderFixed :: FixedPattern -> Int -> Array BEvent
renderFixed p absStep =
  let
    fixedStep = if p.steps <= 0 then 0 else absStep `mod` p.steps
    loop = if p.steps <= 0 then 0 else absStep / p.steps
  in
    mapMaybe (evalLane fixedStep loop) (range 0 (kitSize - 1))
  where
  evalLane fixedStep loop lane =
    let c = cellAt p lane fixedStep
    in
      if c.vel > 0 && condFires c loop && probPass c.prob absStep lane fixedStep then
        Just
          { note: noteOf p lane
          , velocity: c.vel
          , pushMs: 0
          , durMs: laneGateMs lane
          , ratchet: c.ratchet
          }
      else Nothing
