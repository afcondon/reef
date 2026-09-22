-- | `Reef.Conspicillum.Cloud` — the cloud over one cycle: where grains fall,
-- | and what happens to each one.
-- |
-- | C2 answered *which sample*. This answers *when, and then what* — and it is
-- | the step where the instrument stops being a granular module.
-- |
-- | ## A cloud is a pattern, so a grain has an ordinal
-- |
-- | Arbhar's grain stream is one anonymous continuous process: you set density
-- | and spray and it rains, and there is no way to say anything about *one*
-- | grain, because no grain has a name. Here density is a list of onsets — the
-- | mini-notation the player typed, already resolved — so grain 7 is a thing
-- | that exists and can be spoken about. `Every 3 0` is `every 3` over grains,
-- | and it costs one Int.
-- |
-- | That single fact is the whole difference, and everything else here follows
-- | from it.
-- |
-- | ## Why onsets and not mini-notation
-- |
-- | The parser lives in purerl-tidal, which depends on reef and not the other
-- | way round, and pulling it down here to chase a string would be the tail
-- | wagging the dog. The house pattern is the one Stellatus set: the frontend
-- | resolves its text to a fully-determined scene and pushes it ONCE
-- | (`Triggerfish.Tidal.Lane.onsetsOf` already returns exactly these fractional
-- | times). Reef consumes the resolved form. A cloud is then a pure function of
-- | its scene, which is what lets the browser draw it and the BEAM sound it
-- | from the same definition.
-- |
-- | ## Why this is cycle-addressed, where Stellatus loops
-- |
-- | `Reef.Stellatus.Engine.walk` precomputes a finite walk and repeats it,
-- | threading the seed left to right. That is right for a ring of a dozen
-- | slots and wrong here, for two reasons.
-- |
-- | A cloud of hundreds of grains a cycle would need a very long precomputed
-- | array before the repeat stopped being audible — and a cloud that repeats
-- | exactly is heard as a loop in a way a drum pattern is not.
-- |
-- | More importantly, Conspicillum's browser side is a pure visualizer: it
-- | recomputes what the rig is sounding rather than being told. Recomputing
-- | means being able to ask for cycle 400 *directly*, without simulating the
-- | 399 before it. So the seed for a cycle is derived from the base seed and
-- | the cycle number rather than threaded — which is also Tidal's own model,
-- | where a pattern is a function from a time arc to the events in it.
module Reef.Conspicillum.Cloud
  ( When(..)
  , Op(..)
  , Rule
  , Spec
  , Emit
  , cycleSeed
  , applies
  , cycleOf
  ) where

import Prelude

import Data.Array (foldl, length, mapWithIndex, snoc)
import Data.Int.Bits (and)
import Data.Maybe (Maybe(..))
import Reef.Bits (xorshift32)
import Reef.Conspicillum.Corpus (Cloud, Corpus, Query, grainAt, pick)
import Reef.Marbles (Seed, nextRand, seedFrom)

-- ── when a rule applies ──────────────────────────────────────────────────────

-- | Two ways to select grains, and they are different in kind.
-- |
-- | `Every n k` is **positional and certain**: grain ordinals `k, k+n, k+2n…`,
-- | counted across cycles so the figure does not restart every bar. This is the
-- | one no hardware granulator has, because it needs a grain to have an
-- | identity.
-- |
-- | `Chance p` is **stochastic and seeded**: reproducible from the scene, which
-- | the Link-less granulators cannot offer either — their randomness is gone
-- | the moment it happens.
data When
  = Always
  | Every Int Int
  | Chance Number

derive instance eqWhen :: Eq When

-- | What a rule does to a grain. Multiplicative where multiplication is the
-- | musical operation (`speed`, `gain`, grain length), absolute where it is not
-- | (`pan`, `accelerate` — the latter being a ratio already).
data Op
  = OpSpeed Number
  | OpGain Number
  | OpLength Number
  | OpPan Number
  | OpAccelerate Number

type Rule = { when :: When, op :: Op }

-- | Everything about the cloud that is not the corpus or the query.
-- |
-- | `onsets` are fractional positions in [0,1) over one cycle — the resolved
-- | density pattern. An empty list is a silent cloud, which is a legitimate
-- | thing to ask for and must not be confused with a filter that admitted
-- | nothing (see `cycleOf`).
type Spec =
  { onsets :: Array Number
  , cloud :: Cloud
  , rules :: Array Rule
  , speed :: Number
  , gain :: Number
  , pan :: Number
  , accelerate :: Number
  }

-- | One grain, placed in the cycle and fully resolved: everything
-- | `/dirt/play` needs and nothing it does not.
type Emit =
  { at :: Number
  , n :: Int
  , begin :: Number
  , end :: Number
  , sustain :: Number
  , speed :: Number
  , gain :: Number
  , pan :: Number
  , accelerate :: Number
  }

-- ── the per-cycle seed ───────────────────────────────────────────────────────

-- | The seed for one cycle, from the base seed and the cycle number.
-- |
-- | Built from `Reef.Bits.xorshift32`, the one bit primitive this package owns
-- | FFI for because it is proven bit-identical on V8 and the BEAM.
-- |
-- | **Only the low byte of a xorshift state may be used as a VALUE.** The state
-- | itself is signed-32 under JS and explicitly-masked unsigned-32 on the BEAM;
-- | the bits are the same, so chaining one step into the next agrees, but the
-- | *number* does not — and handing the raw state to something like `seedFrom`,
-- | which reduces modulo a constant, gives two different answers. That is not a
-- | hypothetical: this function did exactly that, `conspicillumCloudRun`
-- | diverged on every line while the C2 selector golden stayed byte-identical,
-- | and the two goldens together said "same selector, different seed" precisely
-- | enough to find it. `Reef.Balistes.Engine.randByte` is the idiom — chain the
-- | state, use `and 0xFF`.
-- |
-- | So three chained steps give three sign-agnostic bytes, assembled into 24
-- | bits. That is ~16.7M distinct cycle seeds, which at one cycle a couple of
-- | seconds is longer than any session.
-- |
-- | **Both inputs are reduced below 10^6 before they are added**, and that is
-- | not fussiness either: PureScript's `Int` is 32-bit under JS and
-- | arbitrary-precision under purerl, so an addition that overflows is another
-- | silent cross-runtime divergence. Nothing here can overflow on either.
-- |
-- | This is a mixer, not a hash. Its job is only that adjacent cycles should
-- | not sound related.
cycleSeed :: Int -> Int -> Seed
cycleSeed base n =
  let
    s1 = xorshift32 (small n + small base)
    s2 = xorshift32 s1
    s3 = xorshift32 s2
  in
    seedFrom ((s1 `and` 0xFF) + (s2 `and` 0xFF) * 256 + (s3 `and` 0xFF) * 65536)
  where
  -- Non-negative and bounded, whatever the caller passed and whichever
  -- runtime's `mod` sign convention applies.
  small :: Int -> Int
  small x = let m = x `mod` 1000003 in if m < 0 then m + 1000003 else m

-- ── rule selection ───────────────────────────────────────────────────────────

-- | Does this rule fire for the grain with this absolute ordinal?
-- |
-- | `Chance` draws, so it advances the seed; `Always` and `Every` do not.
-- |
-- | **A `Chance` rule draws unconditionally, before anything is decided.** The
-- | tempting optimisation is to skip the draw when an earlier rule already
-- | settled the grain — and it would make the draws of every later rule depend
-- | on how the earlier ones happened to land, so the cycle would stop being
-- | reproducible from its scene. Fixed draw ORDER is the whole guarantee, and
-- | it is easy to lose by being clever here.
applies :: Rule -> Int -> Seed -> { fires :: Boolean, seed :: Seed }
applies rule ordinal s = case rule.when of
  Always -> { fires: true, seed: s }
  Every n k ->
    let period = if n < 1 then 1 else n
    in { fires: (ordinal - k) `mod` period == 0 && ordinal >= k, seed: s }
  Chance p ->
    let { u, seed } = nextRand s
    in { fires: u < p, seed }

applyOp :: Op -> Emit -> Emit
applyOp op e = case op of
  OpSpeed x -> e { speed = e.speed * x }
  OpGain x -> e { gain = e.gain * x }
  OpLength x -> e { sustain = e.sustain * x }
  OpPan x -> e { pan = x }
  OpAccelerate x -> e { accelerate = x }

-- ── the cycle ────────────────────────────────────────────────────────────────

-- | Every grain of one cycle, fully resolved.
-- |
-- | A pure function of (corpus, query, spec, base seed, cycle number) — so any
-- | cycle can be asked for directly, and the browser and the rig computing the
-- | same one get the same answer.
-- |
-- | A grain whose query admits nothing is **dropped**, not defaulted to sample
-- | zero. The cloud then has fewer grains than it has onsets, which is exactly
-- | the truth and is a thing the surface should be able to show: "your filter
-- | excluded everything" and "you asked for a sparse cloud" must not look the
-- | same. Note that the seed still advances for a dropped grain, so a query
-- | narrowing does not reshuffle the grains that do survive.
cycleOf :: Corpus -> Query -> Spec -> Int -> Int -> Array Emit
cycleOf corpus query spec base cyc =
  (foldl step { seed: cycleSeed base cyc, out: [] } indexed).out
  where
  count = length spec.onsets

  indexed :: Array { i :: Int, at :: Number }
  indexed = mapWithIndex (\i at -> { i, at }) spec.onsets

  -- The ordinal counts across cycles, so `Every 3 0` marks every third grain
  -- continuously rather than restarting each bar. At a few hundred grains a
  -- cycle this stays inside Int for something over two hundred days of
  -- continuous play, which is longer than the rig stays up.
  step acc o =
    let
      ordinal = cyc * count + o.i
      { chosen, seed: s1 } = pick query corpus acc.seed
    in case chosen of
      Nothing -> { seed: s1, out: acc.out }
      Just g ->
        let
          { grain, seed: s2 } = grainAt spec.cloud g s1
          base' =
            { at: o.at
            , n: grain.n
            , begin: grain.begin
            , end: grain.end
            , sustain: grain.sustain
            , speed: spec.speed
            , gain: spec.gain
            , pan: spec.pan
            , accelerate: spec.accelerate
            }
          r = foldl (applyRule ordinal) { e: base', seed: s2 } spec.rules
        in { seed: r.seed, out: snoc acc.out r.e }

  applyRule ordinal acc rule =
    let { fires, seed } = applies rule ordinal acc.seed
    in { e: if fires then applyOp rule.op acc.e else acc.e, seed }
