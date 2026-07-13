-- | `Reef.Balistes.Trig` — the shared eval for Balistes' POLYTRIG (the TIDAL tab):
-- | a rack of named jacks, each firing one MIDI note at the onsets of its resolved
-- | pattern. The POLYTRIG analogue of `Reef.Balistes.Fixed`: one `renderTrigStep`
-- | decision compiled to both the Triggerfish frontend and the BEAM voice, so a
-- | pushed rack plays byte-identically on the rig.
-- |
-- | A POLYTRIG rack is a pure function of the ABSOLUTE step, exactly like a fixed
-- | rhythm — no state, no seed. One Tidal cycle == `cycleSteps` grid steps; the
-- | onsets are the pattern's cycle-0 event fractions in [0,1). Both runtimes read
-- | the same Link step, so both agree with no handoff phase concern.
-- |
-- | The mini-notation lives frontend-side (reef has no Tidal parser): the frontend
-- | RESOLVES each jack to its note + merged onset fractions (its own source ∪ the
-- | route atoms addressed to its name, via `Triggerfish.Tidal.Lane`) and pushes the
-- | flat `TrigKit`. This module then only slices the resolved onsets into steps —
-- | the co-simulation-critical part both runtimes must agree on. Converting a
-- | fraction-within-step to wall time is tempo-dependent and stays runtime-side.
module Reef.Balistes.Trig
  ( TrigJack
  , TrigKit
  , TrigFire
  , trigVelocity
  , trigGateMs
  , renderTrigStep
  ) where

import Prelude

import Data.Array (filter)
import Data.Int (toNumber)

-- | A resolved POLYTRIG jack: a MIDI note + its onset fractions over one cycle
-- | (own source ∪ routed atoms addressed to its name), already computed by the
-- | frontend's `Tidal.Lane`. Wire-flat, so simple-json carries it with no codec.
type TrigJack =
  { note :: Int
  , onsets :: Array Number
  }

-- | The whole rack: an ordered list of resolved jacks.
type TrigKit = Array TrigJack

-- | One fire within a step: the jack's note and the fractional position in [0,1)
-- | of the onset within THIS step (the runtime multiplies it by the step's ms to
-- | schedule it). The velocity/gate are fixed for a trigger rack (`trigVelocity`
-- | / `trigGateMs`); a POLYTRIG jack is a bare trigger, not a velocity lane.
type TrigFire =
  { note :: Int
  , frac :: Number
  }

-- | Velocity every POLYTRIG jack fires at — a firm trigger. Shared so the frontend
-- | (Web MIDI) and the rig (scheduleNoteAt) send the same value.
trigVelocity :: Int
trigVelocity = 100

-- | Gate length (ms) for a POLYTRIG hit — a short blip; the downstream is a trigger
-- | input, so the note-off timing is immaterial past "short".
trigGateMs :: Number
trigGateMs = 40.0

-- | Render one absolute step of a resolved rack: every jack onset that falls in
-- | this step's window fires at its fractional sub-step time. `step = absStep mod
-- | cycleSteps` selects the window `[step/cycleSteps, (step+1)/cycleSteps)`, and
-- | `frac = onset * cycleSteps - step` is the position within it. This is the SINGLE
-- | slicing decision both runtimes use — reading the same Link step, they agree.
renderTrigStep :: TrigKit -> Int -> Int -> Array TrigFire
renderTrigStep kit absStep cycleSteps
  | cycleSteps <= 0 = []
  | otherwise = kit >>= firesOf
      where
      cs = toNumber cycleSteps
      step = absStep `mod` cycleSteps
      stepN = toNumber step
      lo = stepN / cs
      hi = (stepN + 1.0) / cs
      firesOf jack =
        map (\o -> { note: jack.note, frac: o * cs - stepN })
            (filter (\o -> o >= lo && o < hi) jack.onsets)
