-- | `Reef.Engine` — the whole-tick composite for the Odonus module, in ONE place.
-- |
-- | Advancing the sequencer one model step is three things in a fixed order:
-- |   1. `runGen`   — the randomisation matrix fires FIRST, so any mutated value is
-- |                   what the heads read this step;
-- |   2. `tickChord`— the chord-progression overlay advances on its own clock BEFORE
-- |                   the heads read, so the new chord is what this step quantises to;
-- |   3. `stepEmit` — the heads advance and report what fired.
-- |
-- | This composite was, until P4, inlined SEPARATELY in three places — the
-- | Triggerfish frontend's per-tick handler, `reef_voice` on the BEAM, and
-- | `Reef.Conformance` — and they had already drifted (the conformance omitted
-- | step 2). `stepTick` makes the whole tick a single shared definition, so the
-- | frontend co-simulation and the rig authority run bit-identical logic by
-- | construction, not by luck — the same guarantee `stepEmit` already gives for the
-- | heads alone, now extended to generation + chord clock
-- | (reef/docs/PLAN-lockstep-cosimulation.md, P4).
-- |
-- | Tick-tagged INPUTS are applied by the caller (via `Reef.Input.applyInput`)
-- | BEFORE `stepTick`, matching the frontend's order (a gesture mutates the model,
-- | then the step advances). Expression that is purely emission-side — swing,
-- | accent, gate length, legato, MIDI channel — stays with each runtime; only the
-- | shared deterministic MODEL lives here.
module Reef.Engine
  ( SimState
  , stepTick
  ) where

import Reef.Gen (GenInput, runGen)
import Reef.Odonus (Fired, stepEmit, tickChord)

-- | The whole synced state a tick advances: the Odonus record, the gen-source
-- | config, the Marbles pad (spread/bias), and the shared PRNG seed. Structurally
-- | identical to `Reef.Gen.GenInput` and `Reef.Input.SimState` (one row) — re-named
-- | here for a caller that thinks in terms of "the module's state", not "generator
-- | input".
type SimState = GenInput

-- | Advance the module one model step. `runGen` (mutate) → `tickChord` (advance the
-- | chord clock, if the overlay is on) → `stepEmit` (heads advance + report fired).
-- | Returns the next state and the notes that fired. Pure: same state → same next
-- | state + same fired on either runtime.
stepTick :: SimState -> { sim :: SimState, fired :: Array Fired }
stepTick s =
  let
    g = runGen { gen: s.gen, spread: s.spread, bias: s.bias, odo: s.odo, seed: s.seed }
    o1 = if g.odo.chord.on then tickChord g.odo else g.odo
    r = stepEmit o1
  in
    { sim: s { odo = r.odo, seed = g.seed }, fired: r.fired }
