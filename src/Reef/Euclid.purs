-- | **E(k, n), once** — the Euclidean rhythm every machine on the rig plays:
-- | Selene's Euclid banks on both the FH-2 and the ES-9, Odonus's heads, and
-- | the ring Triggerfish draws for either.
-- |
-- | The definition is the Expert Sleepers FH-2's, because the FH-2's firmware
-- | is the one copy we cannot change, and a Euclid dragged from an FHX-8GT jack
-- | to an ES-5 jack has to keep its sound. Measured 2026-10-20 against 18
-- | patterns (3/7 to 17/29, both rigs patched into the ES-9): it is Bjorklund's
-- | algorithm in Toussaint's recursive form, started on its first pulse.
-- |
-- | Every Euclidean rule makes the same set of gaps; they differ in where the
-- | cycle starts. The modular shortcut we used before, `(i * k) mod n < k`,
-- | agreed with the FH-2 on 3/8 and 11/32 and was turned by up to six steps
-- | elsewhere (5/32). Tidal's `euclid` (Drape's Bjorklund) is a third answer:
-- | it agrees with the FH-2 on 8 of those 18 and differs on 7/16, 9/16, 5/32
-- | and 13/32. So a Tidal `euclid 5 32` and a Selene `5 32` are the same
-- | rhythm, but they don't start on the same step.
-- |
-- | Rotation: step `i` plays the pattern's step `i - rotation`, so rotation 1
-- | starts the figure one step later. That is the FH-2's direction too
-- | (measured at r1, r3 and r9).
module Reef.Euclid
  ( pattern
  , turned
  , hit
  ) where

import Prelude

import Data.Array (drop, findIndex, fold, range, replicate, snoc, take, (!!))
import Data.Maybe (Maybe(..), fromMaybe)

-- | E(k, n): `k` pulses over `n` steps, step 0 a pulse. No pulses below 1, a
-- | pulse on every step from `n` up.
pattern :: Int -> Int -> Array Boolean
pattern k n
  | n <= 0 = []
  | k <= 0 = replicate n false
  | k >= n = replicate n true
  | otherwise = startOnPulse (build (divide k n))

-- | E(k, n) turned `rotation` steps later.
turned :: Int -> Int -> Int -> Array Boolean
turned k n rotation = map (\i -> hitIn bits n (i - rotation)) (indices n)
  where
  bits = pattern k n

-- | Is step `i` a pulse of E(k, n) turned `rotation` steps later? Steps wrap.
hit :: Int -> Int -> Int -> Int -> Boolean
hit k n rotation i = hitIn (pattern k n) n (i - rotation)

hitIn :: Array Boolean -> Int -> Int -> Boolean
hitIn bits n i = n > 0 && fromMaybe false (bits !! wrap i n)

-- non-negative before `mod`: purerl's Int `mod` truncates
wrap :: Int -> Int -> Int
wrap i n = ((i `mod` n) + n) `mod` n

indices :: Int -> Array Int
indices n = if n <= 0 then [] else range 0 (n - 1)

-- | Euclid's division of the rests by the pulses, as Toussaint writes it: the
-- | quotient at each level, the remainders, and the level it stopped at.
type Division = { counts :: Array Int, remainders :: Array Int, level :: Int }

divide :: Int -> Int -> Division
divide k n = go [] [ k ] (n - k) 0
  where
  go counts remainders divisor level =
    let
      r = at remainders level
      counts' = snoc counts (divisor / r)
      remainders' = snoc remainders (divisor `mod` r)
      next = level + 1
    in
      if at remainders' next <= 1 then { counts: snoc counts' r, remainders: remainders', level: next }
      else go counts' remainders' r next

build :: Division -> Array Boolean
build d = go d.level
  where
  -- level -1 is a rest, level -2 a pulse
  go level
    | level == -1 = [ false ]
    | level == -2 = [ true ]
    | otherwise =
        fold (replicate (at d.counts level) (go (level - 1)))
          <> (if at d.remainders level /= 0 then go (level - 2) else [])

startOnPulse :: Array Boolean -> Array Boolean
startOnPulse bits = case findIndex identity bits of
  Just i -> drop i bits <> take i bits
  Nothing -> bits

at :: Array Int -> Int -> Int
at xs i = fromMaybe 0 (xs !! i)
