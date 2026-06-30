-- | A `PitchSet` is the general quantisation target: an ascending list of
-- | LITERAL semitone offsets from a root, plus an optional tiling period. It is
-- | the thing an engine indexes into — `realize :: index -> MIDI` — and a static
-- | scale is just the degenerate, octave-periodic case.
-- |
-- | Two design commitments (see project_reef_quantisation_realize):
-- |
-- |   * Offsets are LITERAL, never folded to pitch-classes (mod 12). A 9th sitting
-- |     at offset 26 stays a high D; it never collapses into the base octave. This
-- |     is what lets an extended chord across three octaves be honoured rather than
-- |     amputated ("don't lop the ninth").
-- |
-- |   * `period` is SEPARATE from the offsets' extent: 12 for ordinary octaves, 19
-- |     for a tritave-ish world, `Nothing` for a one-shot finite set. Non-octave
-- |     repetition is just `period /= 12`.
-- |
-- | `equalIndex` is the flat (Instruo Dáil-style) mapping: an input range divides
-- | EQUALLY across all available slots — not hierarchically (period-then-degree).
module Reef.PitchSet
  ( PitchSet(..)
  , cardinality
  , realize
  , equalIndex
  , realizeEqual
  , scaleToPitchSet
  ) where

import Prelude

import Data.Array (length, (!!))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (clamp)
import Reef.Scale (Scale(..))

-- | `offsets` ascending, literal semitones from `root` (the MIDI note of offset 0);
-- | `period` = semitones until the whole list tiles (`Nothing` = finite, no tiling).
newtype PitchSet = PitchSet
  { offsets :: Array Int
  , root :: Int
  , period :: Maybe Int
  }

-- | Notes per period (= per octave for an octave-periodic set). The `N` that
-- | "octave = +N indices" refers to.
cardinality :: PitchSet -> Int
cardinality (PitchSet s) = length s.offsets

-- | Realize an integer index to a MIDI pitch. Periodic sets tile infinitely in
-- | both directions (so voice offsets never clip); finite sets clamp at the ends.
realize :: PitchSet -> Int -> Int
realize (PitchSet s) i =
  let n = length s.offsets in
  if n <= 0 then s.root
  else case s.period of
    Just p ->
      let oct = floorDiv i n
          pos = i - oct * n
      in s.root + p * oct + fromMaybe 0 (s.offsets !! pos)
    Nothing ->
      s.root + fromMaybe 0 (s.offsets !! clamp 0 (n - 1) i)

-- | Flat-equal mapping: an input `v` in `[0, inMax]` divides equally across
-- | `slots` indices. `slots` is typically `spanPeriods * cardinality`.
equalIndex :: Int -> Int -> Int -> Int
equalIndex slots inMax v =
  clamp 0 (slots - 1) (floor (toNumber v * toNumber slots / toNumber (inMax + 1)))

-- | The whole front mapping: input value -> index (flat-equal over
-- | `spanPeriods` periods) -> realized MIDI pitch.
realizeEqual :: PitchSet -> Int -> Int -> Int -> Int
realizeEqual ps spanPeriods inMax v =
  realize ps (equalIndex (spanPeriods * cardinality ps) inMax v)

-- | A `Reef.Scale` is the degenerate octave-periodic PitchSet — proving the
-- | generalisation: its intervals are the offsets, its period the tiling.
scaleToPitchSet :: Scale -> PitchSet
scaleToPitchSet (Scale s) =
  PitchSet { offsets: s.intervals, root: s.root, period: Just s.period }

-- | Floor division — explicit (not Int `div`) so negative indices behave
-- | identically on the JS and Erlang backends (downward voice offsets later).
floorDiv :: Int -> Int -> Int
floorDiv a b = floor (toNumber a / toNumber b)
