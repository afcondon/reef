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
  , realizeEqualShift
  , scaleToPitchSet
  , voicing
  , periodsIn
  , quantiseToVoicing
  , nearestIn
  ) where

import Prelude

import Data.Array (length, nub, sort, (!!))
import Data.Array as Array
import Data.Foldable (maximum, minimumBy)
import Data.Ord (abs, comparing)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Reef.Scale (Scale(..), quantiseToChordPCs)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

-- | `offsets` ascending, literal semitones from `root` (the MIDI note of offset 0);
-- | `period` = semitones until the whole list tiles (`Nothing` = finite, no tiling).
newtype PitchSet = PitchSet
  { offsets :: Array Int
  , root :: Int
  , period :: Maybe Int
  }

-- | Notes per period (= per octave for an octave-periodic set). The `N` that
-- | "octave = +N indices" refers to.
-- Wire codec (Reef.Protocol): a PitchSet travels on the record, so the frontend
-- can hand the BEAM an arbitrary harmonic world. Newtype over a plain record, so
-- the instances just (un)wrap and defer to simple-json's record instances.
instance writeForeignPitchSet :: WriteForeign PitchSet where
  writeImpl (PitchSet r) = writeImpl r

instance readForeignPitchSet :: ReadForeign PitchSet where
  readImpl f = map PitchSet (readImpl f)

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
realizeEqual ps spanPeriods inMax v = realizeEqualShift ps spanPeriods inMax 0 v

-- | `realizeEqual` with a scale-DEGREE transpose: `degShift` indices are added to
-- | the equal-mapped index before realizing, so the whole melody moves by that
-- | many set-degrees (a scalar transpose in the set's own space, not chromatic).
-- | `degShift = 0` is exactly `realizeEqual` — so it's a no-op at the default.
realizeEqualShift :: PitchSet -> Int -> Int -> Int -> Int -> Int
realizeEqualShift ps spanPeriods inMax degShift v =
  realize ps (equalIndex (spanPeriods * cardinality ps) inMax v + degShift)

-- | A `Reef.Scale` is the degenerate octave-periodic PitchSet — proving the
-- | generalisation: its intervals are the offsets, its period the tiling.
scaleToPitchSet :: Scale -> PitchSet
scaleToPitchSet (Scale s) =
  PitchSet { offsets: s.intervals, root: s.root, period: Just s.period }

-- | Floor division — explicit (not Int `div`) so negative indices behave
-- | identically on the JS and Erlang backends (downward voice offsets later).
floorDiv :: Int -> Int -> Int
floorDiv a b = floor (toNumber a / toNumber b)

-- | **A chord as voiced, as a set** (docs/kb/plans/harmony-routes-coherent.md):
-- | its notes (Tidal's note numbers, as `Tidal.Harmony.voicingAt` gives them)
-- | relative to the lowest, repeating over their own span rounded up to whole
-- | octaves. So C E G B D(14) repeats every 24: its D is always a ninth above a
-- | C, never a second, and between that D and the next C there is nothing,
-- | which is what makes a melody on it an arpeggio of the chord (AC: "all notes
-- | in it, none that aren't"). The root is placed as a scale's is, `48 + pc`
-- | of the lowest note: the shape is the chord's, the register Odonus's.
-- | `Nothing` for no notes.
voicing :: Array Int -> Maybe PitchSet
voicing notes = do
  let ns = sort (nub notes)
  lowest <- Array.head ns
  let
    offsets = map (_ - lowest) ns
    reach = fromMaybe 0 (maximum offsets)
  pure (PitchSet { offsets, root: 48 + mod12 lowest, period: Just (12 * (floorDiv reach 12 + 1)) })
  where
  mod12 n = ((n `mod` 12) + 12) `mod` 12

-- | How many of a set's periods `octaves` octaves hold, at least one: a cell's
-- | knob ranges over the same register whatever the set's period, so a
-- | two-octave chord gets half the periods a scale does.
periodsIn :: PitchSet -> Int -> Int
periodsIn (PitchSet s) octaves = max 1 (floorDiv (octaves * 12) (fromMaybe 12 s.period))

-- | Snap `note` to the nearest note of a voicing, repeated over its span. A
-- | voicing within an octave is a set of pitch classes, and snaps exactly as
-- | `quantiseToChordPCs` does (the output's behaviour before voicings); a
-- | wider one snaps to its own notes only, the nearer upward on a tie.
quantiseToVoicing :: Array Int -> Int -> Int
quantiseToVoicing notes note = case voicing notes of
  Nothing -> note
  Just (PitchSet s)
    | fromMaybe 12 s.period <= 12 -> quantiseToChordPCs (map (\n -> ((n `mod` 12) + 12) `mod` 12) notes) note
    | otherwise -> nearestIn (PitchSet s) note

-- | The note of a set nearest `note`, the higher on a tie: over its periods
-- | for a periodic set, its own notes for a finite one. Empty: `note`.
nearestIn :: PitchSet -> Int -> Int
nearestIn (PitchSet s) note =
  fromMaybe note (minimumBy (comparing (\c -> Tuple (abs (c - note)) (negate c))) cands)
  where
  cands = case s.period of
    Just p ->
      let k = floorDiv (note - s.root) p
      in do
        o <- [ k - 1, k, k + 1 ]
        off <- s.offsets
        pure (s.root + off + p * o)
    Nothing -> map (_ + s.root) s.offsets
