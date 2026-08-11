-- | Tests for the V/oct realiser.
-- |
-- | The values below are NOT computed here — they are what DeepStar's Go
-- | realiser returned for the real stored `saich-1` table (GET /realise on
-- | :3027, 2026-08-11). That makes this a cross-implementation check rather
-- | than a restatement: the browser must place a note exactly where the rig
-- | will, and `es9_cv:realise/2`, `internal/rig/calib.go` and this module are
-- | three renderings of one arithmetic that have to agree.
module Test.Calibration (calibrationTests) where

import Prelude

import Data.Array (reverse)

import Effect (Effect)
import Reef.Calibration (Table, covers, realise, realiseNote, span)
import Test.Assert (assertEqual', assertTrue')

-- | The real July sweep of Saich voice 1, 21 points over 65.3-2086.4 Hz.
saich1 :: Table
saich1 =
  { label: "saich-1"
  , points:
      [ { volts: 0.0, hz: 65.349 },
  { volts: 0.25, hz: 77.824 },
  { volts: 0.5, hz: 92.691 },
  { volts: 0.75, hz: 110.422 },
  { volts: 1.0, hz: 131.544 },
  { volts: 1.25, hz: 156.683 },
  { volts: 1.5, hz: 186.544 },
  { volts: 1.75, hz: 222.187 },
  { volts: 2.0, hz: 264.683 },
  { volts: 2.25, hz: 315.144 },
  { volts: 2.5, hz: 375.168 },
  { volts: 2.75, hz: 446.017 },
  { volts: 3.0, hz: 530.847 },
  { volts: 3.25, hz: 631.324 },
  { volts: 3.5, hz: 750.071 },
  { volts: 3.75, hz: 891.095 },
  { volts: 4.0, hz: 1058.725 },
  { volts: 4.25, hz: 1256.16 },
  { volts: 4.5, hz: 1489.997 },
  { volts: 4.75, hz: 1766.379 },
  { volts: 5.0, hz: 2086.409 }
      ]
  }

-- Agreement to a microvolt: the two implementations should differ only by
-- floating-point ordering, not by method.
close :: String -> Number -> Number -> Effect Unit
close what actual expected =
  assertTrue' (what <> ": got " <> show actual <> ", want " <> show expected)
    (abs (actual - expected) < 1.0e-6)
  where
  abs n = if n < 0.0 then -n else n

calibrationTests :: Effect Unit
calibrationTests = do
  -- MIDI notes, against the Go realiser's answers for the same table.
  close "C2 (60 -> 36)" (realiseNote saich1 36.0) 0.0012561588464089
  close "E2" (realiseNote saich1 40.0) 0.3318251319373611
  close "G3" (realiseNote saich1 55.0) 1.5706823686091758
  close "C4" (realiseNote saich1 60.0) 1.9834033644439326
  close "C5" (realiseNote saich1 72.0) 2.9793065286581233

  -- Raw Hz, including a value deliberately BETWEEN two measured points, which
  -- is where a linear-in-Hz interpolation would diverge from log-in-Hz.
  close "65.4 Hz" (realise saich1 65.4) 0.001116322575614265
  close "130.8 Hz" (realise saich1 130.8) 0.9918986854402799
  close "523.25 Hz" (realise saich1 523.25) 2.9793034262485936

  -- Clamping, not extrapolation. A table says what was measured; inventing a
  -- curve past the last reading would be quietly wrong where a note that stops
  -- rising is diagnosable.
  let s = span saich1
  close "below the sweep clamps to the first voltage" (realise saich1 20.0) 0.0
  close "above the sweep clamps to the last voltage" (realise saich1 8000.0) 5.0
  assertEqual' "span is the measured range"
    { actual: { lo: s.lo, hi: s.hi }, expected: { lo: 65.349, hi: 2086.409 } }
  assertTrue' "a pitch inside the sweep is covered" (covers saich1 500.0)
  assertTrue' "a pitch above it is not" (not (covers saich1 8000.0))

  -- An unsorted table must give the same answer: the Erlang sorts on entry and
  -- so must this, or a hand-written table would realise differently.
  let shuffled = saich1 { points = reverse saich1.points }
  close "sorting is done on entry" (realise shuffled 523.25) 2.9793034262485936
