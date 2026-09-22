-- | `Reef.Conspicillum.Corpus` — choosing which grain to play, by querying a
-- | measured corpus rather than by scanning a buffer.
-- |
-- | This is the half of Conspicillum that has to be identical in the browser
-- | and on the BEAM, and so by the layer rule it lives here. The browser draws
-- | the cloud it expects; the BEAM emits the cloud that sounds. If those two
-- | disagree the picture is a lie, and `Reef.Conformance.conspicillumRun` is
-- | what holds them together.
-- |
-- | ## Why a query at all
-- |
-- | A hardware granulator scans a timeline: you have a position knob, and the
-- | grains come from wherever it points. That is the only addressing available
-- | to a module that never measured what it recorded.
-- |
-- | A Quadrat set did measure it, and it recorded WHY each sample exists. So a
-- | grain's source can be chosen by **what it is like** rather than by where it
-- | sits, and the corpus offers two axes of different kinds:
-- |
-- |   * **measured** — `peak`, `rms`, `zcr`, `tilt`, `decay`, and the sample's
-- |     own duration. What it turned out to be.
-- |   * **intentional** — `cell`, the sample's coordinate in the transect, and
-- |     the parameter levels that were set to make it (`means`). Where in the
-- |     instrument's parameter space it came from.
-- |
-- | The second is the one nothing else has. "Grains from the high-`harm` corner
-- | of the transect" is a question no sampler has been able to ask, because no
-- | sampler knew why its samples existed.
-- |
-- | ## Filter, then weight
-- |
-- | A clause is a hard cut: it decides what is *in the cloud at all*. Weighting
-- | is soft: among the survivors it decides what the cloud is *made mostly of*.
-- | Both are wanted and they are not the same gesture — "only the bright ones"
-- | and "mostly the bright ones" are different instruments.
-- |
-- | ## Determinism
-- |
-- | Every arithmetic operation in the select path is `+ - * /` and comparison,
-- | all correctly rounded by IEEE 754 and therefore identical on V8 and the
-- | BEAM. There is deliberately **no `pow`** here, unlike `Reef.Gen`'s Beta
-- | weights, whose last-ULP risk `Reef.Conformance.betaProbe` exists to watch.
-- | A cloud is reproducible from its seed, exactly, on both runtimes — which is
-- | the thing the Link-less granulators cannot offer at all.
module Reef.Conspicillum.Corpus
  ( Axis(..)
  , Cmp(..)
  , Toward(..)
  , Clause
  , Weighting
  , Query
  , Grainable
  , Corpus
  , Cloud
  , Grain
  , axisOf
  , survivors
  , weights
  , pick
  , grainAt
  , emptyQuery
  ) where

import Prelude

import Data.Array (filter, index, length, zipWith, (!!))
import Data.Foldable (foldl, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Reef.Conspicillum.Harmonic (Harmonic, fit)
import Reef.Marbles (Seed, nextRand)

-- ── the corpus ───────────────────────────────────────────────────────────────

-- | One sample of a Quadrat set, reduced to what choosing a grain needs.
-- |
-- | `index` is the identity, because it is the identity at the far end too: a
-- | set's directory IS a SuperDirt bank, its files sort, and the index is the
-- | `n` in `s "brass" # n 3`. The filename never has to cross the wire.
-- |
-- | `secs` is not decoration — a grain's `begin`/`end` window has to be as long
-- | in the source as `sustain` is in time, or the grain is transposed. See
-- | `grainAt`.
type Grainable =
  { index  :: Int
  , secs   :: Number
  -- measured: what the sample turned out to be
  , peak   :: Number
  , rms    :: Number
  , zcr    :: Number
  , tilt   :: Number   -- SPECTRAL tilt. Not SuperDirt's `tilt`, which is
                       -- envelope skew. The two meet inside this instrument.
  , decay  :: Number
  -- intentional: where it came from, and why
  , cell   :: Array Int
  , params :: Array { name :: String, level :: Number }
  , notes  :: Array Int   -- carried, unused here; the harmonia join is C5
  }

-- | A corpus is one Quadrat set. `name` is the SuperDirt `s`.
type Corpus = { name :: String, samples :: Array Grainable }

-- ── addressing an axis ───────────────────────────────────────────────────────

-- | Something a clause or a weighting can be about. The two families are kept
-- | visibly apart because they mean different things, not because they are
-- | handled differently.
data Axis
  = APeak | ARms | AZcr | ATilt | ADecay | ASecs
  | ACell Int        -- the k-th transect coordinate
  | AParam String    -- the level a named transect parameter was set to

derive instance eqAxis :: Eq Axis

-- | Read an axis off a sample. `Nothing` where the sample has no such axis —
-- | a `cell` shorter than the index asked for, or a parameter it never carried
-- | — and a sample that cannot answer is **excluded rather than defaulted**.
-- | Defaulting a missing measurement to zero would quietly place a sample at
-- | one end of every axis it happens to lack.
axisOf :: Axis -> Grainable -> Maybe Number
axisOf ax g = case ax of
  APeak -> Just g.peak
  ARms -> Just g.rms
  AZcr -> Just g.zcr
  ATilt -> Just g.tilt
  ADecay -> Just g.decay
  ASecs -> Just g.secs
  -- A transect coordinate is an Int, and comparing it as a Number is exact:
  -- cells are small and every Int here is representable.
  ACell k -> map toNumber (g.cell !! k)
  AParam nm -> map _.level (findParam nm g.params)

-- ── clauses ──────────────────────────────────────────────────────────────────

data Cmp = Lt | Lte | Gt | Gte

derive instance eqCmp :: Eq Cmp

-- | A hard cut: what is in the cloud at all.
type Clause = { axis :: Axis, cmp :: Cmp, value :: Number }

-- | Which end of an axis a weighting leans toward.
data Toward = High | Low

derive instance eqToward :: Eq Toward

-- | A soft lean: what the cloud is mostly made of.
-- |
-- | `strength` is 0..1 — 0 is uniform, 1 is fully proportional to the axis. The
-- | weight is `(1 - strength) + strength * t`, which is linear, bounded and
-- | monotone: no exponent, so nothing here can disagree across runtimes in the
-- | last bit. A sharper curve would be a nicer instrument and a worse
-- | guarantee; if it is ever wanted it belongs behind a conformance probe of
-- | its own, the way `pow` is.
type Weighting = { axis :: Axis, toward :: Toward, strength :: Number }

-- | `harmonic` is a THIRD axis, and orthogonal to the other two on purpose.
-- |
-- | A clause and a weighting both ask about a property OF the sample. A
-- | harmonic constraint asks about the relation between the sample and
-- | something outside it — the chord currently wanted — so it is not an `Axis`
-- | and forcing it to be one would have been the wrong shape. It carries its
-- | own hard cut and soft lean, mirroring the pairing the numeric axes use.
type Query =
  { clauses :: Array Clause
  , weighting :: Maybe Weighting
  , harmonic :: Maybe Harmonic
  }

emptyQuery :: Query
emptyQuery = { clauses: [], weighting: Nothing, harmonic: Nothing }

-- ── filtering ────────────────────────────────────────────────────────────────

-- | The samples a query admits. Every clause must hold, and a sample that
-- | cannot answer an axis fails it (see `axisOf`).
survivors :: Query -> Corpus -> Array Grainable
survivors q c = filter admits c.samples
  where
  admits g = foldl (\ok cl -> ok && holds cl g) true q.clauses && voices g
  -- A harmonic cut is a clause like any other: below `minFit` the sample is
  -- not in the cloud at all. Note this is where the 24 chord hits with no
  -- recorded notes drop out, since `fit` scores them zero rather than
  -- abstaining — see `Reef.Conspicillum.Harmonic`.
  voices g = case q.harmonic of
    Nothing -> true
    Just h -> fit h.target g.notes >= h.minFit

holds :: Clause -> Grainable -> Boolean
holds cl g = case axisOf cl.axis g of
  Nothing -> false
  Just v -> case cl.cmp of
    Lt -> v < cl.value
    Lte -> v <= cl.value
    Gt -> v > cl.value
    Gte -> v >= cl.value

-- ── weighting ────────────────────────────────────────────────────────────────

-- | A weight per survivor, in the same order.
-- |
-- | The axis is normalised across the survivors THEMSELVES rather than against
-- | any absolute scale, because these measurements have no absolute scale worth
-- | leaning on: `zcr` in Hz and `decay` in seconds share no range, and "the
-- | bright end" only ever means bright *relative to this corpus*. A consequence
-- | worth knowing at the surface: narrowing the filter re-normalises the lean,
-- | so the same weighting over a smaller survivor set is a sharper distinction,
-- | not a weaker one.
-- |
-- | Degenerate cases are uniform rather than arbitrary: no weighting, every
-- | value equal, or a sample that cannot answer the axis.
weights :: Query -> Array Grainable -> Array Number
weights q gs = zipWith (*) (axisWeights q gs) (harmonicWeights q gs)

-- | The harmonic lean, or all-ones when there is none.
-- |
-- | Multiplied into the axis lean rather than replacing it, so "mostly the
-- | bright ones AND mostly the ones that voice this chord" composes — which is
-- | the whole reason the two are separate concepts.
-- |
-- | Not normalised across survivors, unlike the axis lean: `fit` is already an
-- | absolute measure on [0,1] with a meaning ("how well does this voice the
-- | chord"), so re-scaling it against whatever else survived would destroy
-- | exactly the information it carries.
harmonicWeights :: Query -> Array Grainable -> Array Number
harmonicWeights q gs = case q.harmonic of
  Nothing -> map (const 1.0) gs
  Just h -> map (\g -> (1.0 - h.strength) + h.strength * fit h.target g.notes) gs

axisWeights :: Query -> Array Grainable -> Array Number
axisWeights q gs = case q.weighting of
  Nothing -> map (const 1.0) gs
  Just w ->
    let
      vals = map (axisOf w.axis) gs
      known = foldl (\acc v -> case v of
                        Just x -> case acc of
                          Nothing -> Just { lo: x, hi: x }
                          Just r -> Just { lo: min r.lo x, hi: max r.hi x }
                        Nothing -> acc) Nothing vals
    in case known of
      Nothing -> map (const 1.0) gs
      Just r ->
        let span = r.hi - r.lo
        in map (\mv -> case mv of
                  Nothing -> 1.0
                  Just x ->
                    let t0 = if span == 0.0 then 0.5 else (x - r.lo) / span
                        t = case w.toward of
                              High -> t0
                              Low -> 1.0 - t0
                    in (1.0 - w.strength) + w.strength * t) vals

-- ── choosing ─────────────────────────────────────────────────────────────────

-- | Draw one survivor, weighted, and advance the seed.
-- |
-- | `Nothing` when the query admits nothing at all — which is a real state and
-- | a silent one if it is defaulted. A cloud whose filter excludes everything
-- | should say so on the surface, not quietly play sample 0.
pick :: Query -> Corpus -> Seed -> { chosen :: Maybe Grainable, seed :: Seed }
pick q c s =
  let
    gs = survivors q c
    ws = weights q gs
    total = sum ws
    { u, seed } = nextRand s
  in
    if length gs == 0 || total <= 0.0 then { chosen: Nothing, seed }
    else { chosen: walk (u * total) 0.0 0 gs ws, seed }
  where
  -- Cumulative search. Linear in the survivors, which is the right complexity
  -- for a corpus of tens-to-hundreds and a rate the probe measured as free.
  walk :: Number -> Number -> Int -> Array Grainable -> Array Number -> Maybe Grainable
  walk target acc i gs ws = case index ws i of
    Nothing -> gs !! (length gs - 1)   -- float drift at the very top; take the last
    Just w ->
      let acc' = acc + w
      in if target < acc' then gs !! i else walk target acc' (i + 1) gs ws

-- ── the grain ────────────────────────────────────────────────────────────────

-- | How the cloud reads whatever sample it lands on.
-- |
-- | `position` is the scan point, 0..1; `spray` is the jitter around it, in the
-- | same units, so `spray 0.0` scans and `spray 1.0` is the whole sample.
type Cloud = { sustain :: Number, position :: Number, spray :: Number }

-- | What `/dirt/play` needs: which sample, and which window of it.
type Grain = { n :: Int, begin :: Number, end :: Number, sustain :: Number }

-- | Place one grain in a chosen sample, and advance the seed.
-- |
-- | **The window must be as long in the source as the grain is in time.**
-- | SuperDirt sweeps `begin`→`end` over `sustain`, so the effective rate is
-- | `(end - begin) * secs / sustain`: get the width wrong and every grain is
-- | transposed, consistently, in a way that sounds like a deliberate choice.
-- | That is why `Grainable` carries `secs` at all.
grainAt :: Cloud -> Grainable -> Seed -> { grain :: Grain, seed :: Seed }
grainAt cl g s =
  let
    { u, seed } = nextRand s
    w = clamp01 (if g.secs <= 0.0 then 1.0 else cl.sustain / g.secs)
    room = 1.0 - w
    jitter = (u - 0.5) * cl.spray
    begin = clampTo 0.0 room (cl.position * room + jitter)
  in
    { grain: { n: g.index, begin, end: begin + w, sustain: cl.sustain }, seed }

clamp01 :: Number -> Number
clamp01 = clampTo 0.0 1.0

clampTo :: Number -> Number -> Number -> Number
clampTo lo hi x = if x < lo then lo else if x > hi then hi else x

findParam
  :: String
  -> Array { name :: String, level :: Number }
  -> Maybe { name :: String, level :: Number }
findParam nm ps = foldl (\acc p -> case acc of
                            Just _ -> acc
                            Nothing -> if p.name == nm then Just p else Nothing) Nothing ps
