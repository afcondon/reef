-- | `Reef.Stellatus.Engine` — the shared generative core of the Stellatus
-- | circular sample re-sequencer, living in `reef` so the Triggerfish JS frontend
-- | and the purerl-tidal BEAM backend run ONE definition (the same discipline as
-- | Odonus/Balistes/Vetula; see reef/docs/PLAN-lockstep-cosimulation.md).
-- |
-- | The reframe: THE RING IS ONE CYCLE. A `Scene` is a ring of resolved `Slot`s
-- | (the frontend's text parser has already turned `place`/`slice` + `# verbs`
-- | into flat per-arc samples + params), plus stochastic GLITCH rules and a JUMP
-- | adjacency table. The playback model is grid-locked: one step of the WALK per
-- | clock tick, advancing arc-to-arc but LEAPING where the jump table + its
-- | probability fire; each step's speed is the arc's base `# speed` folded through
-- | any glitch rules that rolled true.
-- |
-- | Determinism: the walk is precomputed as a FIXED-LENGTH loop (`walkLen`),
-- | threaded through the Park–Miller `Reef.Marbles` PRNG — exact in Double, hence
-- | byte-identical on JS and the BEAM (proven by `Reef.Conformance.stellatusRun`).
-- | The BEAM voice computes `walk` once per pushed scene and indexes it by
-- | `step `mod` walkLen`; no per-tick seed threading, no float drift. Output is a
-- | SuperDirt `/dirt/play` param bag (`DirtEvent`) — reef's first non-MIDI voice.
module Reef.Stellatus.Engine
  ( Slot
  , GlitchRule
  , Target
  , JumpRow
  , JumpSpec
  , Scene
  , Emit
  , DirtEvent
  , walkLen
  , walk
  , dirtOf
  , events
  ) where

import Prelude

import Data.Array (elemIndex, find, foldl, length, range, uncons, (!!))
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Reef.Marbles (Seed, nextRand, seedFrom)

-- | One arc of the ring, fully resolved by the frontend: its ring placement
-- | (`onset`/`span`, for the visualizer's identity/colour) and its SuperDirt
-- | payload (`s`/`n` sample, `begin`/`end` window, base `speed`, `gain`). All
-- | primitives, so the whole `Scene` is wire-ready with no enum projection.
type Slot =
  { name :: String
  , onset :: Number
  , span :: Number
  , s :: String
  , n :: Int
  , begin :: Number
  , end :: Number
  , speed :: Number
  , gain :: Number
  }

-- | A stochastic per-hit warp. `kind` 0 = reverse (flip speed sign), 1 = speed
-- | multiply by `amount`. `prob` is the firing probability per fired step.
type GlitchRule = { prob :: Number, kind :: Int, amount :: Number }

type Target = { name :: String, weight :: Number }

type JumpRow = { from :: String, targets :: Array Target }

-- | Global jump probability + the name→weighted-targets adjacency table.
type JumpSpec = { prob :: Number, table :: Array JumpRow }

-- | The whole instrument, pushed to the BEAM in one shot.
type Scene =
  { slots :: Array Slot
  , glitch :: Array GlitchRule
  , jumps :: JumpSpec
  , seed :: Int
  }

-- | One loop position: which slot fires, the arc it jumped FROM (Nothing = plain
-- | advance; Just = leapt, for the visualizer's jump chord), and the final speed
-- | after glitch.
type Emit = { slot :: Int, from :: Maybe Int, speed :: Number }

-- | A SuperDirt `/dirt/play` param bag. `orbit`/`cps` are added by the voice.
type DirtEvent =
  { s :: String, n :: Int, begin :: Number, end :: Number, speed :: Number, gain :: Number }

-- | Loop length: four passes of the ring, clamped to a sane window. Fixed so the
-- | BEAM indexes by `step `mod` walkLen`.
walkLen :: Int -> Int
walkLen count = let x = count * 4 in if x < 12 then 12 else if x > 48 then 48 else x

advance :: Int -> Int -> Int
advance count cur = (cur + 1) `mod` count

-- | Weighted choice over a jump row's targets, driven by a uniform draw in [0,1).
pickTarget :: Number -> Array Target -> Maybe String
pickTarget h targets = go (h * sum (map _.weight targets)) 0.0 targets
  where
  go thr acc ts = case uncons ts of
    Just { head: t, tail: rest } ->
      if thr < acc + t.weight then Just t.name else go thr (acc + t.weight) rest
    Nothing -> Nothing

applyGlitch :: GlitchRule -> Number -> Number
applyGlitch rule sp = if rule.kind == 0 then negate sp else sp * rule.amount

-- | Precompute the walk loop: `walkLen` emits, seed threaded left-to-right. Draw
-- | order per step is FIXED (jump die → target pick if jumping → one glitch die
-- | per rule) so both runtimes reproduce the same sequence.
walk :: Scene -> Array Emit
walk scene =
  let count = length scene.slots
      names = map _.name scene.slots
      len = walkLen count
  in if count == 0 then []
     else (foldl (stepFold scene count names) { cur: 0, seed: seedFrom scene.seed, out: [] } (range 0 (len - 1))).out

stepFold
  :: Scene
  -> Int
  -> Array String
  -> { cur :: Int, seed :: Seed, out :: Array Emit }
  -> Int
  -> { cur :: Int, seed :: Seed, out :: Array Emit }
stepFold scene count names acc _i =
  let curName = fromMaybe "" (names !! acc.cur)
      j = nextRand acc.seed
      hop = case find (\r -> r.from == curName) scene.jumps.table of
        Just row | j.u < scene.jumps.prob ->
          let t = nextRand j.seed
          in case pickTarget t.u row.targets >>= \nm -> elemIndex nm names of
               Just tgt -> { arc: tgt, from: Just acc.cur, seed: t.seed }
               Nothing -> { arc: advance count acc.cur, from: Nothing, seed: t.seed }
        _ -> { arc: advance count acc.cur, from: Nothing, seed: j.seed }
      base = fromMaybe 1.0 (map _.speed (scene.slots !! hop.arc))
      g = foldl glitchFold { sp: base, seed: hop.seed } scene.glitch
      emit = { slot: hop.arc, from: hop.from, speed: g.sp }
  in { cur: hop.arc, seed: g.seed, out: acc.out <> [ emit ] }

glitchFold :: { sp :: Number, seed :: Seed } -> GlitchRule -> { sp :: Number, seed :: Seed }
glitchFold acc rule =
  let r = nextRand acc.seed
  in if r.u < rule.prob then { sp: applyGlitch rule acc.sp, seed: r.seed }
     else { sp: acc.sp, seed: r.seed }

-- | Resolve one loop emit to its `/dirt/play` bag. A missing slot yields an empty
-- | `s` (the voice skips it) so `events` stays index-aligned with `walk`.
dirtOf :: Scene -> Emit -> DirtEvent
dirtOf scene e = case scene.slots !! e.slot of
  Just sl -> { s: sl.s, n: sl.n, begin: sl.begin, end: sl.end, speed: e.speed, gain: sl.gain }
  Nothing -> { s: "", n: 0, begin: 0.0, end: 1.0, speed: 1.0, gain: 0.0 }

-- | The whole loop as `/dirt/play` bags — what the BEAM voice indexes per tick.
events :: Scene -> Array DirtEvent
events scene = map (dirtOf scene) (walk scene)
