-- | The Marbles X-section as a pure note-value generator. Values are drawn
-- | from a Beta distribution whose **mean is BIAS** and whose **concentration
-- | falls as SPREAD rises** — reproducing the Mutable Instruments SPREAD×BIAS
-- | chart: at low spread a delta spike at the bias; widening to a bell; at
-- | high spread a broad near-uniform; at full spread the mass flees to the two
-- | rails (bimodal), weighted by bias. We evaluate the Beta pdf at discrete
-- | candidate notes and inverse-CDF sample it (no rejection), so it's pure and
-- | reproducible from a seed. DÉJÀ-VU lives at the call site as the per-cell
-- | regenerate-vs-hold probability (`mutateInts`'s `amount`).
module Reef.Marbles
  ( Seed
  , seedFrom
  , nextRand
  , nextInt
  , concentration
  , betaWeights
  , rollValue
  , mutateInts
  ) where

import Prelude

import Data.Array (range, length, (!!))
import Data.Foldable (foldl, sum)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Reef.Numeric (pow)

-- ── PRNG ─────────────────────────────────────────────────────────────────────
-- Park–Miller minimal standard. The seed is a Number holding an integer in
-- [1, 2147483646]; 16807·seed stays below 2^53, so the step is exact in Double.

type Seed = Number

modulus :: Number
modulus = 2147483647.0

-- | Map any Int to a valid non-zero seed.
seedFrom :: Int -> Seed
seedFrom n =
  let m = n `mod` 2147483646
  in toNumber (if m <= 0 then m + 2147483646 else m)

nextSeed :: Seed -> Seed
nextSeed s =
  let p = 16807.0 * s
  in p - modulus * toNumber (floor (p / modulus))

-- | A uniform draw in [0,1) and the advanced seed.
nextRand :: Seed -> { u :: Number, seed :: Seed }
nextRand s = let s' = nextSeed s in { u: s' / modulus, seed: s' }

-- | A uniform integer in [0, hi) and the advanced seed (hi should be ≥ 1).
nextInt :: Int -> Seed -> { n :: Int, seed :: Seed }
nextInt hi s =
  let { u, seed } = nextRand s
      n = floor (u * toNumber hi)
  in { n: if n < 0 then 0 else if n >= hi then hi - 1 else n, seed }

-- ── Beta distribution ────────────────────────────────────────────────────────

clamp01 :: Number -> Number
clamp01 = clamp 0.0 1.0

-- | Distribution concentration κ from SPREAD. κ huge ⇒ a delta at the bias;
-- | κ ≈ 2 ⇒ uniform; κ < 2 ⇒ U-shaped/bimodal. Geometric so the bell→uniform
-- | →bimodal transition feels even across the knob's travel.
concentration :: Number -> Number
concentration spread =
  let kmax = 1500.0
      kmin = 0.30
  in kmin * pow (kmax / kmin) (1.0 - clamp01 spread)

-- | The Beta(α,β) pdf evaluated at `n` bucket centres across (0,1), with
-- | α = bias·κ and β = (1−bias)·κ, normalised to a discrete distribution.
-- | Exposed so the X-Y pad can draw the live histogram behind the puck.
betaWeights :: Int -> Number -> Number -> Array Number
betaWeights n bias spread =
  let k = concentration spread
      a = max 1.0e-3 (clamp01 bias * k)
      b = max 1.0e-3 ((1.0 - clamp01 bias) * k)
      raw = map
        (\i -> let t = (toNumber i + 0.5) / toNumber n
               in pow t (a - 1.0) * pow (1.0 - t) (b - 1.0))
        (range 0 (n - 1))
      tot = sum raw
  in if tot <= 0.0 then raw else map (_ / tot) raw

-- | Inverse-CDF pick: walk the cumulative weights until they pass `u`.
pickIndex :: Array Number -> Number -> Int
pickIndex ws u = go 0.0 0
  where
  go acc i = case ws !! i of
    Nothing -> max 0 (length ws - 1)
    Just w -> let acc' = acc + w in if u <= acc' then i else go acc' (i + 1)

-- | Draw one note from the distribution over `candidates` (which should be the
-- | in-scale notes across the working range, ascending). Returns the chosen
-- | value and the advanced seed.
rollValue
  :: { bias :: Number, spread :: Number }
  -> Array Int -> Seed -> { value :: Int, seed :: Seed }
rollValue cfg candidates seed =
  let ws = betaWeights (length candidates) cfg.bias cfg.spread
      { u, seed: seed' } = nextRand seed
      ix = pickIndex ws u
  in { value: fromMaybe 60 (candidates !! ix), seed: seed' }

-- | The DÉJÀ-VU step: for each current value, with probability `amount` redraw
-- | it from the distribution, else hold it — `amount` 0 freezes the loop, 1
-- | regenerates every cell. Threads the seed left-to-right.
mutateInts
  :: { bias :: Number, spread :: Number, amount :: Number }
  -> Array Int -> Array Int -> Seed -> { values :: Array Int, seed :: Seed }
mutateInts cfg candidates current seed0 =
  foldl step { values: [], seed: seed0 } current
  where
  step acc v =
    let { u, seed: s1 } = nextRand acc.seed
    in if u < cfg.amount
       then let r = rollValue { bias: cfg.bias, spread: cfg.spread } candidates s1
            in { values: acc.values <> [ r.value ], seed: r.seed }
       else { values: acc.values <> [ v ], seed: s1 }
