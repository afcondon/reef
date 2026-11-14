-- | A calibration table as it is kept in Amphora: content of kind
-- | `calibration-table`, curated into the collection `vco-calibrations`, its
-- | payload the JSON described here.
-- |
-- | `Reef.Calibration.Table` is what the realiser needs: a label and
-- | (volts, hz) points. A stored table carries those and the record of how it
-- | was measured, because a table is true only for the signal path and the
-- | front-panel state it was measured on. The field names are the ones
-- | DeepStar wrote from 2026-07 on, so every table already in the store reads.
-- |
-- | The fields are described in the alphabetical order of their keys, so that
-- | what `write` produces has its keys sorted at every level: the payload's
-- | text is what Amphora hashes, and two writers of one table should agree on
-- | its name.
module Reef.Calibration.Stored
  ( Stored
  , StoredPoint
  , kind
  , collection
  , stored
  , read
  , write
  , table
  , voltsPerOctave
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Number (log)
import Hawser.Message (field, optional, recordOf)
import Hawser.Value (Value, arrayOf, fromText, nullable, number, string, toText, whole)
import Reef.Calibration (Table)

-- | The `kind` every calibration payload is saved under.
kind :: String
kind = "calibration-table"

-- | The collection the realisers read.
collection :: String
collection = "vco-calibrations"

-- | One measured step: the voltage applied and the pitch that came out, with
-- | how well the readings behind it agreed.
type StoredPoint =
  { volts :: Number
  , hz :: Number
  -- | Deviation from a perfect volt per octave, from the first point.
  -- | Diagnostic; the realiser never reads it.
  , errCents :: Maybe Number
  -- | How many readings were usable.
  , reads :: Maybe Int
  -- | Highest reading to lowest. Says whether an outlier occurred.
  , spreadCents :: Maybe Number
  -- | The median to its nearest other reading. Says whether the median
  -- | survived the outlier, which is the verdict on the point.
  , agreementCents :: Maybe Number
  -- | Octaves the reading was folded by to reach `hz`.
  , octaveShift :: Maybe Int
  }

type Stored =
  { label :: String
  , points :: Array StoredPoint
  , bus :: Int
  , channel :: Int
  -- | The module's front-panel state when it was measured. A table without
  -- | it cannot be reproduced.
  , coarseSetting :: Maybe String
  , baseNote :: Maybe String
  , baseOffsetCents :: Maybe Number
  , refHzAt0V :: Maybe Number
  , module :: Maybe String
  , device :: Maybe String
  , measuredAt :: Maybe String
  , probe :: Maybe String
  -- | The output family: `es9-cv`, `esx-cv`, `fh2-midi`.
  , stimulus :: Maybe String
  , fullScaleVolts :: Maybe Number
  , resolutionCents :: Maybe Number
  , role :: Maybe String
  , sampleRate :: Maybe Int
  , usableHz :: Maybe (Array Int)
  , octaveSnapCents :: Maybe Number
  -- | Octaves every pitch was moved to agree with what a person heard.
  , octaveOffset :: Maybe Int
  -- | Another table these points were copied from, not swept.
  , derivedFrom :: Maybe String
  }

point :: Value StoredPoint
point = recordOf "one measured step" $
  (\agreementCents errCents hz octaveShift reads spreadCents volts -> { agreementCents, errCents, hz, octaveShift, reads, spreadCents, volts })
    <$> optional "agreement_cents" _.agreementCents number "cents from the median reading to its nearest other reading"
    <*> optional "err_cents" _.errCents number "cents from a perfect volt per octave, from the first point"
    <*> field "hz" _.hz number "the pitch that came out"
    <*> optional "octave_shift" _.octaveShift whole "octaves the reading was folded by"
    <*> optional "reads" _.reads whole "how many readings were usable"
    <*> optional "spread_cents" _.spreadCents number "cents from the highest reading to the lowest"
    <*> field "volts" _.volts number "the voltage applied"

-- | The payload. The `kind` inside it is always `calibration-table`; it is
-- | read and written, and is not a field of `Stored`.
stored :: Value Stored
stored = recordOf "a calibration table" $
  ( \baseNote baseOffsetCents bus channel coarseSetting derivedFrom device fullScaleVolts _ label measuredAt mod octaveOffset octaveSnapCents points probe refHzAt0V resolutionCents role sampleRate stimulus usableHz ->
      { baseNote, baseOffsetCents, bus, channel, coarseSetting, derivedFrom, device, fullScaleVolts, label, measuredAt, module: mod, octaveOffset, octaveSnapCents, points, probe, refHzAt0V, resolutionCents, role, sampleRate, stimulus, usableHz }
  )
    <$> optional "base_note" _.baseNote string "the note nearest the first point's pitch"
    <*> optional "base_offset_cents" _.baseOffsetCents number "how far the first point sat from that note"
    <*> field "bus" _.bus whole "the output it was driven from"
    <*> field "channel" _.channel whole "the input it was heard on"
    <*> field "coarse_setting" _.coarseSetting (nullable string) "the module's front-panel state; null if nobody recorded it"
    <*> optional "derived_from" _.derivedFrom string "another table these points were copied from"
    <*> optional "device" _.device string "the audio device it was heard through"
    <*> optional "full_scale_volts" _.fullScaleVolts number "the output's measured swing"
    <*> field "kind" (const kind) string "always `calibration-table`"
    <*> field "label" _.label string "the signal path this table is for; realisers look it up by this"
    <*> optional "measured_at" _.measuredAt string "when"
    <*> optional "module" _.module string "the module at the far end"
    <*> optional "octave_offset" _.octaveOffset whole "octaves every pitch was moved to agree with what was heard"
    <*> optional "octave_snap_cents" _.octaveSnapCents number "the tolerance octave folding ran with"
    <*> field "points" _.points (arrayOf point) "the measured steps"
    <*> optional "probe" _.probe string "what measured the pitch"
    <*> optional "ref_hz_at_0v" _.refHzAt0V number "the pitch at the first point"
    <*> optional "resolution_cents" _.resolutionCents number "the finest pitch step the output can place"
    <*> optional "role" _.role string "what the output is fit to carry"
    <*> optional "sample_rate" _.sampleRate whole "the capture's sample rate"
    <*> optional "stimulus" _.stimulus string "the output family: es9-cv, esx-cv or fh2-midi"
    <*> optional "usable_hz" _.usableHz (arrayOf whole) "the band the table is trusted over"

-- | A payload as text. `Nothing` if it is not a calibration table as
-- | described: a field of another type, or one that is not described.
read :: String -> Maybe Stored
read = fromText stored

-- | The payload's text, keys sorted at every level.
write :: Stored -> String
write = toText stored

-- | What the realiser needs of it.
table :: Stored -> Table
table kept = { label: kept.label, points: kept.points <#> \measured -> { volts: measured.volts, hz: measured.hz } }

-- | Volts per octave from the first point to the last, by voltage: 1.0 for
-- | an oscillator that tracks perfectly. Zero for fewer than two points or
-- | no change in pitch.
voltsPerOctave :: Stored -> Number
voltsPerOctave kept =
  let
    sorted = Array.sortWith _.volts kept.points
  in
    case Array.head sorted, Array.last sorted of
      Just first, Just last | first.hz > 0.0 && last.hz > 0.0 && last.hz /= first.hz ->
        (last.volts - first.volts) / (log (last.hz / first.hz) / log 2.0)
      _, _ -> 0.0
