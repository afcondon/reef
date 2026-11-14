-- | Judging a calibration sweep: combining the readings of one step, naming
-- | the base, saying what is wrong with a table, and moving a table by whole
-- | octaves to agree with what a person heard.
-- |
-- | Every rule here was DeepStar's, and each was learned on the rig; the
-- | reasons are in docs/kb/reference/vco-calibration-lessons.md. DeepStar is
-- | retired, so this is held to what DeepStar gave on fixed inputs, recorded
-- | before it went: conformance/vectors/calibration-judgement.json, replayed
-- | by conformance/calibration-judgement.mjs.
module Reef.Calibration.Judge
  ( Combined
  , combine
  , Snapped
  , snap
  , Base
  , baseOf
  , noteName
  , Problem(..)
  , problems
  , problemKind
  , problemText
  , Heard(..)
  , heard
  , centsBetween
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (minimum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (abs, floor, log, pow)
import Data.Tuple (Tuple(..))
import Reef.Calibration.Stored (Stored, StoredPoint)

log2 :: Number -> Number
log2 x = log x / log 2.0

-- | The interval from one frequency to another, in cents. Positive when the
-- | second is sharp of the first.
centsBetween :: Number -> Number -> Number
centsBetween from to = 1200.0 * log2 (to / from)

-- | To the nearest whole number, a half going away from zero.
roundAway :: Number -> Number
roundAway x = if x < 0.0 then negate (floor (negate x + 0.5)) else floor (x + 0.5)

roundTo :: Number -> Number -> Number
roundTo places x = roundAway (x * places) / places

-- ── The readings of one step ───────────────────────────────────────────────

type Combined =
  { median :: Number
  -- | Highest reading to lowest. Answers "was there an outlier", which is
  -- | worth knowing and is NOT a verdict on the point.
  , spreadCents :: Number
  -- | The median to its nearest other reading. Answers "is the median
  -- | corroborated", which is the question that matters.
  , agreementCents :: Number
  }

-- | The median of a step's readings, not their mean: a bad reading is a
-- | different answer, not noise. The two measures come apart exactly when
-- | the median does its job. On ESX slot 1 at 3.50 V one reading of three
-- | landed 1.86 octaves out, a spread of 2,236 cents, while the median sat
-- | 2 cents from where its neighbours predicted.
combine :: Array Number -> Combined
combine readings =
  let
    sorted = Array.sort readings
    count = Array.length sorted
    at index = fromMaybe 0.0 (Array.index sorted index)
    middle = count / 2
    odd = count `mod` 2 == 1
    median = if count == 0 then 0.0 else if odd then at middle else (at (middle - 1) + at middle) / 2.0
    -- Every reading but the median's own slot, when there is one.
    others = Array.mapMaybe identity (Array.mapWithIndex (\index reading -> if (odd && index == middle) || reading <= 0.0 then Nothing else Just reading) sorted)
  in
    if count < 2 || at 0 <= 0.0 || median <= 0.0 then { median, spreadCents: 0.0, agreementCents: 0.0 }
    else
      { median
      , spreadCents: abs (centsBetween (at 0) (at (count - 1)))
      , agreementCents: fromMaybe 0.0 (minimum (map (\reading -> abs (centsBetween median reading)) others))
      }

-- ── Folding a reading onto the octave expected ─────────────────────────────

type Snapped = { folded :: Number, octaves :: Int, ok :: Boolean }

-- | Fold a reading onto the nearest octave of the pitch expected at this
-- | step, and say whether the result is close enough to believe.
-- |
-- | A pitch detector asked about an inharmonic signal does not fail; it
-- | answers. The fold is legitimate only because it moves whole octaves: it
-- | can correct a detector that is out by a factor of two or four, and it
-- | cannot invent a tracking result. A reading near no octave (a fifth
-- | partial) is refused, not dragged into line.
snap :: Number -> Number -> Number -> Snapped
snap hz expect toleranceCents =
  if hz <= 0.0 || expect <= 0.0 then { folded: hz, octaves: 0, ok: false }
  else
    let
      octaves = roundAway (log2 (hz / expect))
      folded = hz * pow 2.0 (negate octaves)
    in
      { folded, octaves: Int.round octaves, ok: abs (centsBetween expect folded) <= toleranceCents }

-- ── The base ───────────────────────────────────────────────────────────────

type Base = { midi :: Int, note :: String, offsetCents :: Number }

-- | Scientific pitch notation; middle C is C4, note 60.
noteName :: Int -> String
noteName midi =
  fromMaybe "?" (Array.index [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ] (((midi `mod` 12) + 12) `mod` 12))
    <> show (midi / 12 - 1)

-- | The note nearest a pitch, and how far the pitch sits from it. The base
-- | of a sweep is detected this way, not dialled: a hand-set base was never
-- | load-bearing, because the realiser works from absolute points.
baseOf :: Number -> Base
baseOf hz =
  let
    midi = Int.round (roundAway (69.0 + 12.0 * log2 (hz / 440.0)))
  in
    { midi, note: noteName midi, offsetCents: centsBetween (440.0 * pow 2.0 ((Int.toNumber midi - 69.0) / 12.0)) hz }

-- ── What is wrong with a table ─────────────────────────────────────────────

data Problem
  -- | Cannot interpolate. How many points there are.
  = TooFewPoints Int
  -- | Pitch does not rise with voltage all the way: a flat or folded sweep,
  -- | most likely a dead audio stream. The span found.
  | NotMonotonic { loHz :: Number, hiHz :: Number }
  -- | The readings behind a point do not corroborate its median.
  | UnreliablePoint { volts :: Number, reads :: Int, agreementCents :: Number, spreadCents :: Number }

derive instance Eq Problem

problemKind :: Problem -> String
problemKind = case _ of
  TooFewPoints _ -> "too-few-points"
  NotMonotonic _ -> "not-monotonic"
  UnreliablePoint _ -> "unreliable-point"

-- | For a person.
problemText :: Problem -> String
problemText = case _ of
  TooFewPoints count -> "only " <> show count <> " point" <> (if count == 1 then "" else "s") <> ": nothing to interpolate between"
  NotMonotonic { loHz, hiHz } ->
    "the pitch does not rise with the voltage all the way (" <> show loHz <> " to " <> show hiHz <> " Hz): a flat or folded sweep, most likely no sound reaching the input"
  UnreliablePoint point ->
    "the point at " <> show point.volts <> " V is unreliable: " <> show point.reads <> " readings, and the middle one agrees with none of the others within "
      <> show (if point.agreementCents > 0.0 then point.agreementCents else point.spreadCents)
      <> " cents"

-- | Everything suspect about a table, in the order a person would want it:
-- | too few points, then whether pitch rises, then each point in turn. A
-- | table with any problem is not kept.
-- |
-- | A point is judged on whether its median is corroborated, not on whether
-- | any reading was wild: rejecting good data because the outlier rejection
-- | worked is the wrong way round. A table from before agreement was
-- | recorded falls back to its spread.
problems :: Array StoredPoint -> Array Problem
problems points =
  (if Array.length points < 2 then [ TooFewPoints (Array.length points) ] else [])
    <> (if rises then [] else [ NotMonotonic span ])
    <> Array.mapMaybe unreliable points
  where
  byVolts = Array.sortWith _.volts points
  rises = Array.all (\(Tuple lower upper) -> upper.hz > lower.hz) (Array.zip byVolts (Array.drop 1 byVolts))
  span =
    { loHz: fromMaybe 0.0 (Array.head byVolts <#> _.hz)
    , hiHz: fromMaybe 0.0 (Array.last byVolts <#> _.hz)
    }
  unreliable point =
    let
      reads = fromMaybe 0 point.reads
      agreementCents = fromMaybe 0.0 point.agreementCents
      spreadCents = fromMaybe 0.0 point.spreadCents
    in
      if (reads > 2 && agreementCents > 5.0) || (not (reads > 2 && agreementCents > 5.0) && reads > 0 && agreementCents == 0.0 && spreadCents > 10.0) then
        Just (UnreliablePoint { volts: point.volts, reads, agreementCents, spreadCents })
      else Nothing

-- ── What a person heard ────────────────────────────────────────────────────

data Heard
  -- | The table, moved by this many whole octaves (none, if they agreed).
  = Moved Int Stored
  -- | Nothing to compare: no heard pitch, no measured base, or no points.
  | NothingToCompare
  -- | The heard pitch is not near a whole number of octaves from the
  -- | measured base, so this is not an octave error: the two are following
  -- | different signals. How many octaves apart they are.
  | NotAnOctaveError Number

-- | Move every pitch in a table by whole octaves so that its base agrees
-- | with a pitch a person heard.
-- |
-- | A detector and an ear can both be right about one signal and disagree:
-- | autocorrelation finds the true period, the ear follows the loudest
-- | partial. For a calibration table the ear wins, because the table exists
-- | to place the pitch someone will hear. But the ear supplies only the
-- | octave: the move is rounded to a whole one, so an estimate 20 cents out
-- | costs nothing and the detector keeps supplying the precision.
heard :: Stored -> Number -> Heard
heard table heardHz = case table.refHzAt0V of
  Just refHz | heardHz > 0.0 && refHz > 0.0 && not (Array.null table.points) ->
    let
      apart = log2 (heardHz / refHz)
      octaves = roundAway apart
    in
      if abs (apart - octaves) > 0.25 then NotAnOctaveError apart
      else if octaves == 0.0 then Moved 0 table
      else
        let
          scale = pow 2.0 octaves
          moved = table.points <#> \point -> point { hz = roundTo 1000.0 (point.hz * scale) }
          newRef = roundTo 100.0 (fromMaybe 0.0 (Array.head moved <#> _.hz))
          base = baseOf newRef
        in
          Moved (Int.round octaves) table
            { points = moved
            , refHzAt0V = Just newRef
            , baseNote = Just base.note
            , baseOffsetCents = Just (roundTo 100.0 base.offsetCents)
            , octaveOffset = Just (Int.round octaves)
            }
  _ -> NothingToCompare
