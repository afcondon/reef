-- | `Reef.Engine` — the whole-tick composite for the Odonus module, in ONE place.
-- |
-- | Advancing the sequencer one model step is two things in a fixed order:
-- |   1. `runGen`   — the randomisation matrix fires FIRST, so any mutated value is
-- |                   what the heads read this step;
-- |   2. `stepEmit` — the heads advance and report what fired.
-- |
-- | (A third, `tickChord`, advanced a chord progression on its own clock; it
-- | retired 2026-10-01 for the harmony pattern, which the host samples into the
-- | model before the step: `followHarmony`, below.)
-- |
-- | This composite was, until P4, inlined SEPARATELY in three places — the
-- | Triggerfish frontend's per-tick handler, `reef_voice` on the BEAM, and
-- | `Reef.Conformance` — and they had already drifted (the conformance omitted
-- | step 2). `stepTick` makes the whole tick a single shared definition, so the
-- | frontend co-simulation and the rig authority run bit-identical logic by
-- | construction, not by luck — the same guarantee `stepEmit` already gives for the
-- | heads alone, now extended to generation
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
  , followHarmony
  , followScale
  , sampleInput
  , Patterns
  , patternsOf
  , samplePatterns
  , odoPatterns
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Reef.Gen (GenInput, runGen)
import Reef.Input (Input(..))
import Reef.Odonus (Fired, Odonus, stepEmit)
import Reef.Odonus (followHarmony, followScale) as Odonus

-- | The whole synced state a tick advances: the Odonus record, the gen-source
-- | config, the Marbles pad (spread/bias), and the shared PRNG seed. Structurally
-- | identical to `Reef.Gen.GenInput` and `Reef.Input.SimState` (one row) — re-named
-- | here for a caller that thinks in terms of "the module's state", not "generator
-- | input".
type SimState = GenInput

-- | Advance the module one model step. `runGen` (mutate) → `stepEmit` (heads
-- | advance + report fired).
-- | Returns the next state and the notes that fired. Pure: same state → same next
-- | state + same fired on either runtime.
stepTick :: SimState -> { sim :: SimState, fired :: Array Fired }
stepTick s =
  let
    g = runGen { gen: s.gen, spread: s.spread, bias: s.bias, odo: s.odo, seed: s.seed, frozen: s.frozen }
    r = stepEmit g.odo
  in
    { sim: s { odo = r.odo, seed = g.seed }, fired: r.fired }

-- | The host's half of harmony-as-a-pattern (`Reef.Odonus.followHarmony`):
-- | before `stepTick`, let the chord overlay follow the harmony pattern as the
-- | host's Tidal reader samples it at this step. Both hosts call it at the same
-- | place in the step, after the step's inputs, so lockstep holds.
followHarmony :: (String -> Array Int) -> SimState -> SimState
followHarmony sample s = s { odo = Odonus.followHarmony sample s.odo }

-- | **What the rig sends instead of following.** A host that can read Tidal (the
-- | BEAM) samples Odonus's two patterns for a coming step and hands both
-- | runtimes the result as one input, `SetSampled`, tick-tagged, so the browser
-- | never reads a pattern (docs/kb/plans/gpl-boundary-review.md). `harmony` and
-- | `scale` are the samplers for that step; an empty sample is no chord, and
-- | leaves the scale as it is. Applying the input is what `followHarmony` then
-- | `followScale` do.
sampleInput :: (String -> Array Int) -> (String -> Array Int) -> SimState -> Input
sampleInput harmony scale s = samplePatterns harmony scale (patternsOf s)

-- | The patterns a sample reads, and nothing else of the state: what the rig
-- | samples for a page that plays Odonus itself (Solo), which sends just
-- | these (`odonus-sample`) and gets back one `SetSampled` a step.
type Patterns =
  { harmony :: Maybe String
  , scale :: Maybe String
  , outScale :: Maybe { pattern :: String, root :: Int }
  , gridHarmony :: Maybe String
  , heads :: Maybe String
  , octave :: Maybe String
  }

patternsOf :: SimState -> Patterns
patternsOf s = odoPatterns s.odo

-- | The patterns an Odonus holds, for a host that has the model alone.
odoPatterns :: Odonus -> Patterns
odoPatterns o = { harmony: o.harmony, scale: o.scalePattern, outScale: o.outScale, gridHarmony: o.gridHarmony, heads: o.headsPattern, octave: o.octavePattern }

samplePatterns :: (String -> Array Int) -> (String -> Array Int) -> Patterns -> Input
samplePatterns harmony scale p = SetSampled chord sc grid heads octave
  where
  -- the heads to play, as numbers (`harmony` gives a note pattern's numbers
  -- as written); a rest is an empty list, every head silent
  heads = harmony <$> p.heads
  -- the octave, as a number pattern's numbers, the same way
  octave = harmony <$> p.octave
  -- the output's set: a scale on its own root (outScale), or a chord
  chord = case p.outScale of
    Just out -> map (\i -> (i + out.root) `mod` 12) <$> nonEmpty (scale out.pattern)
    Nothing -> p.harmony >>= \t -> nonEmpty (harmony t)
  sc = p.scale >>= \t -> nonEmpty (scale t)
  -- the grid's chord, as voiced: `harmony` is the host's sampler, which gives
  -- the notes as voiced (Tidal.Harmony.voicingSampler)
  grid = p.gridHarmony >>= \t -> nonEmpty (harmony t)
  nonEmpty xs = if Array.null xs then Nothing else Just xs

-- | The same for the scale pattern (`Reef.Odonus.followScale`): `sample`
-- | gives the steps of the scale named at this step. Before `followHarmony`,
-- | which snaps past the scale.
followScale :: (String -> Array Int) -> SimState -> SimState
followScale sample s = s { odo = Odonus.followScale sample s.odo }
