-- | The cases that hold every copy of the V/oct realiser to one answer.
-- |
-- | `Reef.Calibration.realise` is the reference: it is what the rig plays
-- | through, in the browser and on the BEAM. DeepStar has its own copy (Go,
-- | for verification) and so will whatever replaces it. Each replays
-- | `conformance/vectors/calibration.json`, which is generated from here:
-- | a table and a pitch in, volts out.
-- |
-- | The cases are written by hand. The volts are computed by the reference,
-- | and `conformance/calibration_reference.py`, written apart from it,
-- | must reach the same volts.
-- |
-- | Agreement is to a nanovolt, not to the bit: the copies call their own
-- | runtimes' logarithm, and IEEE 754 does not promise those agree in the
-- | last place.
module Test.CalibrationVectors (vectors, tables, cases) where

import Prelude

import Data.Array (filter, reverse)
import Data.String (joinWith)
import Reef.Calibration (Table, noteToHz, realise)

-- | The real July sweep of Saich voice 1, 21 points over 65.3-2086.4 Hz.
saich1 :: Table
saich1 =
  { label: "saich-1"
  , points:
      [ { volts: 0.0, hz: 65.349 }
      , { volts: 0.25, hz: 77.824 }
      , { volts: 0.5, hz: 92.691 }
      , { volts: 0.75, hz: 110.422 }
      , { volts: 1.0, hz: 131.544 }
      , { volts: 1.25, hz: 156.683 }
      , { volts: 1.5, hz: 186.544 }
      , { volts: 1.75, hz: 222.187 }
      , { volts: 2.0, hz: 264.683 }
      , { volts: 2.25, hz: 315.144 }
      , { volts: 2.5, hz: 375.168 }
      , { volts: 2.75, hz: 446.017 }
      , { volts: 3.0, hz: 530.847 }
      , { volts: 3.25, hz: 631.324 }
      , { volts: 3.5, hz: 750.071 }
      , { volts: 3.75, hz: 891.095 }
      , { volts: 4.0, hz: 1058.725 }
      , { volts: 4.25, hz: 1256.16 }
      , { volts: 4.5, hz: 1489.997 }
      , { volts: 4.75, hz: 1766.379 }
      , { volts: 5.0, hz: 2086.409 }
      ]
  }

tables :: Array Table
tables =
  [ saich1
  , { label: "saich-1-reversed", points: reverse saich1.points }
  , { label: "ideal", points: [ { volts: -1.0, hz: 55.0 }, { volts: 0.0, hz: 110.0 }, { volts: 1.0, hz: 220.0 }, { volts: 2.0, hz: 440.0 } ] }
  , { label: "two-points", points: [ { volts: 0.0, hz: 100.0 }, { volts: 4.0, hz: 1600.0 } ] }
  , { label: "one-point", points: [ { volts: 1.5, hz: 300.0 } ] }
  -- Hand-edited tables can do this; a measured, monotonic one cannot.
  , { label: "same-pitch-twice", points: [ { volts: 0.0, hz: 100.0 }, { volts: 1.0, hz: 200.0 }, { volts: 2.0, hz: 200.0 }, { volts: 3.0, hz: 400.0 } ] }
  ]

type Case = { name :: String, table :: String, hz :: Number, why :: String }

cases :: Array Case
cases =
  [ at "saich-below" "saich-1" 20.0 "below the sweep: the first voltage, not an extrapolation"
  , at "saich-first" "saich-1" 65.349 "exactly the first point"
  , at "saich-just-inside" "saich-1" 65.4 "just inside"
  , at "saich-c3" "saich-1" 130.8 "between two points, where log and linear interpolation part"
  , at "saich-on-a-point" "saich-1" 264.683 "exactly a measured point"
  , at "saich-c5" "saich-1" 523.25 "an octave and more up"
  , at "saich-high" "saich-1" 2000.0 "in the last bracket"
  , at "saich-last" "saich-1" 2086.409 "exactly the last point"
  , at "saich-above" "saich-1" 8000.0 "above the sweep: the last voltage"
  , note "saich-note-36" "saich-1" 36.0 "a MIDI note, C2"
  , note "saich-note-40" "saich-1" 40.0 "E2"
  , note "saich-note-55" "saich-1" 55.0 "G3"
  , note "saich-note-60" "saich-1" 60.0 "C4"
  , note "saich-note-72" "saich-1" 72.0 "C5"
  , note "saich-note-fractional" "saich-1" 60.5 "a quarter-tone: fractional notes are allowed"
  , at "reversed-c5" "saich-1-reversed" 523.25 "the points are sorted by volts on entry"
  , at "reversed-below" "saich-1-reversed" 20.0 "and the ends are the ends after sorting"
  , at "ideal-a2" "ideal" 110.0 "a perfect volt per octave: 110 Hz at 0 V"
  , at "ideal-a4" "ideal" 440.0 "440 Hz at 2 V"
  , at "ideal-tritone" "ideal" 155.56349186104046 "half an octave above 110 Hz is half a volt"
  , at "ideal-negative" "ideal" 77.78174593052023 "and below 0 V"
  , at "two-points-geometric-middle" "two-points" 400.0 "the middle in pitch of 100 and 1600 Hz is 400 Hz: 2 V"
  , at "two-points-quarter" "two-points" 200.0 "one octave of four: 1 V"
  , at "one-point-below" "one-point" 100.0 "one point: its voltage, whatever is asked"
  , at "one-point-above" "one-point" 1000.0 "the same"
  , at "same-pitch-first-bracket" "same-pitch-twice" 150.0 "before the repeated pitch"
  , at "same-pitch-on-it" "same-pitch-twice" 200.0 "on it: the first bracket that holds it gives its upper voltage"
  , at "same-pitch-after" "same-pitch-twice" 300.0 "after it"
  ]
  where
  at name table hz why = { name, table, hz, why }
  note name table midi why = { name, table, hz: noteToHz midi, why }

quoted :: String -> String
quoted text = "\"" <> text <> "\""

-- | The vector file. Written out by hand, with no JSON library: a number is
-- | as `show` gives it, which reads back exactly.
vectors :: String
vectors = joinWith "\n"
  [ "{"
  , "  \"calibration-vectors\": 1,"
  , "  \"generatedBy\": \"Test.CalibrationVectors in reef; do not edit by hand, run `node conformance/calibration.mjs write`\","
  , "  \"toleranceVolts\": 1e-9,"
  , "  \"tables\": {"
  , joinWith ",\n" (tables <#> \table -> "    " <> quoted table.label <> ": [" <> joinWith ", " (table.points <#> \point -> "{\"volts\": " <> show point.volts <> ", \"hz\": " <> show point.hz <> "}") <> "]")
  , "  },"
  , "  \"cases\": ["
  , joinWith ",\n"
      ( cases <#> \testCase -> "    {\"name\": " <> quoted testCase.name <> ", \"table\": " <> quoted testCase.table <> ", \"hz\": " <> show testCase.hz
          <> ", \"volts\": "
          <> show (voltsOf testCase)
          <> ", \"why\": "
          <> quoted testCase.why
          <> "}"
      )
  , "  ]"
  , "}"
  ]
  where
  voltsOf testCase = case filter (\table -> table.label == testCase.table) tables of
    [ table ] -> realise table testCase.hz
    -- No such table: not a number, which is not JSON, so writing fails.
    _ -> 0.0 / 0.0
