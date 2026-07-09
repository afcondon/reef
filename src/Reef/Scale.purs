-- | The pitch lens. Mirrors purerl-tidal's `Tidal.Scales`: a cell stores an
-- | integer; a `Scale` (root + intervals + period) plus a `Distribution`
-- | (Natural | Equal) turn that integer into a concrete MIDI note. Switching
-- | the scale re-renders every cell without touching a stored value — the
-- | bidirectional, text-consistent model the BEAM Odonus uses.
module Reef.Scale
  ( Scale(..)
  , Distribution(..)
  , ScaleType
  , scaleTypes
  , randomisableScales
  , mkScale
  , mkScaleFromIvls
  , normaliseIvls
  , quantiseToChordPCs
  , recogniseScale
  , spreadIvls
  , rootNames
  , rootName
  , rootSlug
  , scaleName
  , renderDegree
  , quantiseToScale
  , noteToDegreeIn
  , shiftDegrees
  , applyDistribution
  , pitchClassesOf
  ) where

import Prelude

import Data.Array (find, findIndex, index, length, nub, sort, take, (!!), (:))
import Data.Foldable (minimumBy)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Foreign (ForeignError(..), fail)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

-- | A concrete scale: `root` is the MIDI note of degree 1, `intervals` are
-- | ascending semitone offsets within one period, `period` is semitones until
-- | the pattern repeats (12 = octave). `name` is the wire identifier.
newtype Scale = Scale
  { root :: Int
  , intervals :: Array Int
  , period :: Int
  , name :: String
  }

-- | How a cell integer is interpreted. `Natural` = chromatic semitone snapped
-- | to the nearest scale tone; `Equal` = scale-degree index.
data Distribution = Natural | Equal

derive instance eqDistribution :: Eq Distribution

instance showDistribution :: Show Distribution where
  show Natural = "Natural"
  show Equal = "Equal"

-- | Wire representation for the reef protocol (see Reef.Protocol). Declared here,
-- | beside the type, to stay non-orphan. `Distribution` is the only non-primitive
-- | field in the Odonus record; a matched read/write pair keeps it symmetric.
instance writeForeignDistribution :: WriteForeign Distribution where
  writeImpl Natural = writeImpl "natural"
  writeImpl Equal = writeImpl "equal"

instance readForeignDistribution :: ReadForeign Distribution where
  readImpl f = readImpl f >>= case _ of
    "natural" -> pure Natural
    "equal" -> pure Equal
    other -> fail (ForeignError ("Reef.Scale: unknown Distribution " <> show (other :: String)))

-- | A named scale shape (intervals only — root supplied separately so the UI
-- | can pick root × type independently).
type ScaleType = { name :: String, intervals :: Array Int }

-- | The selectable scale shapes — the pool the pickers (Odonus KEY, later Vetula)
-- | draw from. Period is 12 (octave) for all of these. Grouped by family in the
-- | UI; intervals are semitone offsets from the root, computed by rotating each
-- | parent's step pattern (so the modes are exact). Names are stable identities
-- | (round-trip + recogniseScale + the pickers' option `value`); the display
-- | labels live in the view. Existing names kept for back-compat.
scaleTypes :: Array ScaleType
scaleTypes =
  -- Major (Ionian) modes
  [ { name: "major",         intervals: [ 0, 2, 4, 5, 7, 9, 11 ] }        -- Ionian
  , { name: "dorian",        intervals: [ 0, 2, 3, 5, 7, 9, 10 ] }
  , { name: "phrygian",      intervals: [ 0, 1, 3, 5, 7, 8, 10 ] }
  , { name: "lydian",        intervals: [ 0, 2, 4, 6, 7, 9, 11 ] }
  , { name: "mixolydian",    intervals: [ 0, 2, 4, 5, 7, 9, 10 ] }
  , { name: "minor",         intervals: [ 0, 2, 3, 5, 7, 8, 10 ] }        -- Aeolian
  , { name: "locrian",       intervals: [ 0, 1, 3, 5, 6, 8, 10 ] }
  -- Melodic minor modes
  , { name: "melodicMinor",     intervals: [ 0, 2, 3, 5, 7, 9, 11 ] }
  , { name: "dorianFlat2",      intervals: [ 0, 1, 3, 5, 7, 9, 10 ] }
  , { name: "lydianAugmented",  intervals: [ 0, 2, 4, 6, 8, 9, 11 ] }
  , { name: "lydianDominant",   intervals: [ 0, 2, 4, 6, 7, 9, 10 ] }     -- acoustic
  , { name: "melodicMajor",     intervals: [ 0, 2, 4, 5, 7, 8, 10 ] }     -- mixolydian b6
  , { name: "halfDiminished",   intervals: [ 0, 2, 3, 5, 6, 8, 10 ] }     -- locrian ♮2
  , { name: "altered",          intervals: [ 0, 1, 3, 4, 6, 8, 10 ] }     -- superlocrian
  -- Harmonic minor modes
  , { name: "harmonicMinor",     intervals: [ 0, 2, 3, 5, 7, 8, 11 ] }
  , { name: "locrianNat6",       intervals: [ 0, 1, 3, 5, 6, 9, 10 ] }
  , { name: "ionianAugmented",   intervals: [ 0, 2, 4, 5, 8, 9, 11 ] }
  , { name: "ukrainianDorian",   intervals: [ 0, 2, 3, 6, 7, 9, 10 ] }    -- dorian ♯4
  , { name: "phrygianDominant",  intervals: [ 0, 1, 4, 5, 7, 8, 10 ] }
  , { name: "lydianSharp2",      intervals: [ 0, 3, 4, 6, 7, 9, 11 ] }
  , { name: "alteredDiminished", intervals: [ 0, 1, 3, 4, 6, 8, 9 ] }
  -- Pentatonic
  , { name: "pentaMajor",   intervals: [ 0, 2, 4, 7, 9 ] }
  , { name: "pentaMinor",   intervals: [ 0, 3, 5, 7, 10 ] }
  -- Messiaen modes of limited transposition (mode 1 = whole tone)
  , { name: "wholetone",    intervals: [ 0, 2, 4, 6, 8, 10 ] }            -- Messiaen 1
  , { name: "messiaen2",    intervals: [ 0, 1, 3, 4, 6, 7, 9, 10 ] }      -- octatonic
  , { name: "messiaen3",    intervals: [ 0, 2, 3, 4, 6, 7, 8, 10, 11 ] }
  , { name: "messiaen4",    intervals: [ 0, 1, 2, 5, 6, 7, 8, 11 ] }
  , { name: "messiaen5",    intervals: [ 0, 1, 5, 6, 7, 11 ] }
  , { name: "messiaen6",    intervals: [ 0, 2, 4, 5, 6, 8, 10, 11 ] }
  , { name: "messiaen7",    intervals: [ 0, 1, 2, 3, 5, 6, 7, 8, 9, 11 ] }
  -- Carnatic (a curated few; the full set awaits the scale-library page)
  , { name: "mayamalavagowla",    intervals: [ 0, 1, 4, 5, 7, 8, 11 ] }   -- double harmonic
  , { name: "simhendramadhyamam", intervals: [ 0, 2, 3, 6, 7, 8, 11 ] }   -- Hungarian minor
  , { name: "shanmukhapriya",     intervals: [ 0, 2, 3, 6, 7, 8, 10 ] }
  -- Symmetric
  , { name: "chromatic",    intervals: [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ] }
  ]

-- | A larger body of musically-reasonable scales for the KEY·SCALE randomiser
-- | to draw from. Deliberately excludes the chromatic set (and any single
-- | atonal note-toggling) — random tonal centres, not random clusters. The
-- | seven diatonic modes, the minor variants, pentatonics, blues, and a handful
-- | of evocative-but-grounded exotics (phrygian-dominant, hungarian minor,
-- | hirajoshi, …).
randomisableScales :: Array (Array Int)
randomisableScales =
  [ [ 0, 2, 4, 5, 7, 9, 11 ]      -- major (ionian)
  , [ 0, 2, 3, 5, 7, 9, 10 ]      -- dorian
  , [ 0, 1, 3, 5, 7, 8, 10 ]      -- phrygian
  , [ 0, 2, 4, 6, 7, 9, 11 ]      -- lydian
  , [ 0, 2, 4, 5, 7, 9, 10 ]      -- mixolydian
  , [ 0, 2, 3, 5, 7, 8, 10 ]      -- minor (aeolian)
  , [ 0, 1, 3, 5, 6, 8, 10 ]      -- locrian
  , [ 0, 2, 3, 5, 7, 8, 11 ]      -- harmonic minor
  , [ 0, 2, 3, 5, 7, 9, 11 ]      -- melodic minor
  , [ 0, 2, 4, 5, 7, 8, 11 ]      -- harmonic major
  , [ 0, 2, 4, 6, 7, 9, 10 ]      -- lydian dominant
  , [ 0, 1, 4, 5, 7, 8, 10 ]      -- phrygian dominant
  , [ 0, 2, 3, 6, 7, 8, 11 ]      -- hungarian minor
  , [ 0, 2, 4, 7, 9 ]             -- major pentatonic
  , [ 0, 3, 5, 7, 10 ]            -- minor pentatonic
  , [ 0, 2, 5, 7, 10 ]            -- egyptian / suspended pentatonic
  , [ 0, 3, 5, 6, 7, 10 ]         -- blues
  , [ 0, 2, 3, 7, 8 ]             -- hirajoshi
  , [ 0, 1, 5, 7, 8 ]             -- in / japanese
  , [ 0, 2, 4, 6, 8, 10 ]         -- whole tone
  ]

rootNames :: Array String
rootNames = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

-- | Name a 0..11 pitch class.
rootName :: Int -> String
rootName pc = fromMaybe "?" (rootNames !! (((pc `mod` 12) + 12) `mod` 12))

-- | Lowercase wire slug for a root pitch class ("c", "cs", "d", …).
rootSlug :: Int -> String
rootSlug pc = case rootName pc of
  "C#" -> "cs"
  "D#" -> "ds"
  "F#" -> "fs"
  "G#" -> "gs"
  "A#" -> "as"
  "C" -> "c"
  "D" -> "d"
  "E" -> "e"
  "F" -> "f"
  "G" -> "g"
  "A" -> "a"
  "B" -> "b"
  o -> o

-- | Build a Scale from a root pitch-class (0..11, placed in the middle octave)
-- | and a scale type. The wire name is e.g. "c-minor".
mkScale :: Int -> ScaleType -> Scale
mkScale rootPc t = Scale
  { root: 60 + (((rootPc `mod` 12) + 12) `mod` 12)
  , intervals: t.intervals
  , period: 12
  , name: rootSlug rootPc <> "-" <> t.name
  }

-- | Normalise an interval set: each into 0..11, with the root (0) always
-- | present, sorted ascending and de-duplicated. The canonical scale form.
normaliseIvls :: Array Int -> Array Int
normaliseIvls ivls = nub (sort (0 : map (\i -> (((i `mod` 12) + 12) `mod` 12)) ivls))

-- | Build a Scale directly from a root and an arbitrary interval set — the
-- | pitch-class-mask model. The name is auto-recognised (e.g. "c-dorian", or
-- | "5-note" for a custom set).
mkScaleFromIvls :: Int -> Array Int -> Scale
mkScaleFromIvls rootPc ivls =
  let
    rpc = (((rootPc `mod` 12) + 12) `mod` 12)
    n = normaliseIvls ivls
  in
    Scale { root: 60 + rpc, intervals: n, period: 12
          , name: rootSlug rpc <> "-" <> recogniseScale n }

-- | Name an interval set by matching it against the known scale shapes;
-- | falls back to "<count>-note" for an unrecognised custom set.
recogniseScale :: Array Int -> String
recogniseScale ivls =
  let n = normaliseIvls ivls
  in case find (\t -> normaliseIvls t.intervals == n) scaleTypes of
    Just t -> t.name
    Nothing -> show (length n) <> "-note"

-- | Pitch classes ordered by consonance from the root — the order in which a
-- | Marbles-style "spread" dial grows the scale: root, fifth, fourth, major
-- | third, sixth, second, … out to the tritone.
consonanceOrder :: Array Int
consonanceOrder = [ 0, 7, 5, 4, 9, 2, 11, 3, 8, 10, 1, 6 ]

-- | The scale of the first `k` consonance-ordered notes (1..12). spread 1 =
-- | root only; 2 = root+fifth; … 12 = full chromatic.
spreadIvls :: Int -> Array Int
spreadIvls k = normaliseIvls (take (clamp 1 12 k) consonanceOrder)

-- | The wire identifier for a scale ("c-minor", "a-dorian"…).
scaleName :: Scale -> String
scaleName (Scale s) = s.name

-- | floor division that rounds toward negative infinity (Int `div` truncates).
floorDiv :: Int -> Int -> Int
floorDiv a b = floor (toNumber a / toNumber b)

-- | Render a 1-based degree to a MIDI note, wrapping octaves through `period`.
-- | Degree 1 = root, 8 = root + one period, 0 / negatives go below.
renderDegree :: Scale -> Int -> Int
renderDegree (Scale s) degree =
  let
    n = length s.intervals
    idx0 = degree - 1
    period = if idx0 >= 0 then idx0 / n else -((-idx0 - 1) / n + 1)
    step = idx0 - period * n
    offset = fromMaybe 0 (index s.intervals step)
  in
    s.root + offset + s.period * period

-- | Snap any MIDI note to the nearest in-scale note (a few octaves of
-- | candidates around the input cover the range).
quantiseToScale :: Scale -> Int -> Int
quantiseToScale (Scale s) note =
  let
    baseOct = floorDiv (note - s.root) s.period
    cands = do
      o <- [ baseOct - 1, baseOct, baseOct + 1 ]
      iv <- s.intervals
      pure (s.root + iv + s.period * o)
  in
    fromMaybe note (minimumBy (comparing \c -> abs (c - note)) cands)

-- | Multi-octave chord quantiser (Instruō Dial-style): snap `note` to the
-- | nearest pitch in ANY octave whose pitch-class is one of `pcs` (a chord's
-- | tones, 0..11). Spreads voicings naturally across octaves — each cell lands
-- | on whichever chord tone is closest, so notes fan out over the range. Empty
-- | `pcs` passes the note through unchanged.
quantiseToChordPCs :: Array Int -> Int -> Int
quantiseToChordPCs pcs note =
  let cands = map (nearestWithPc note) pcs
  in fromMaybe note (minimumBy (comparing \c -> abs (c - note)) cands)

-- | The note nearest to `note` whose pitch class is `pc` (0..11).
nearestWithPc :: Int -> Int -> Int
nearestWithPc note pc =
  let r = (((pc - note) `mod` 12) + 12) `mod` 12
  in if r <= 6 then note + r else note + r - 12

-- | The 1-based degree of an in-scale note (snaps first if off-scale).
noteToDegreeIn :: Scale -> Int -> Int
noteToDegreeIn scale@(Scale s) note =
  let
    snapped = quantiseToScale scale note
    oct = floorDiv (snapped - s.root) s.period
    within = snapped - s.root - oct * s.period
    stepIx = fromMaybe 0 (findIndex (_ == within) s.intervals)
  in
    oct * length s.intervals + stepIx + 1

-- | Shift a note by `off` scale degrees (the scale-aware transpose: "+4" lands
-- | on the scale's fifth, not a chromatic interval).
shiftDegrees :: Scale -> Int -> Int -> Int
shiftDegrees scale off note = renderDegree scale (noteToDegreeIn scale note + off)

-- | Interpret a cell integer through the active distribution. `Equal` maps the
-- | chromatic distance from the root onto scale degrees (cell = root → degree
-- | 1), so each grid step climbs the scale; `Natural` snaps chromatically.
applyDistribution :: Distribution -> Scale -> Int -> Int
applyDistribution Natural scale n = quantiseToScale scale n
applyDistribution Equal scale@(Scale s) n = renderDegree scale (n - s.root + 1)

-- | The 0..11 pitch classes that belong to the scale (for the quantizer ring).
pitchClassesOf :: Scale -> Array Int
pitchClassesOf (Scale s) = map (\iv -> (((iv + s.root) `mod` 12) + 12) `mod` 12) s.intervals
