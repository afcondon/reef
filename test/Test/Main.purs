-- | Reef test entry. Pins the Odonus `stepEmit` conformance golden (the same
-- | output the BEAM produces — see conformance/cross-runtime.sh). Asserts via
-- | Test.Assert so a drift in the engine fails CI under node; the cross-runtime
-- | script proves the Erlang side matches the same golden.
module Test.Main (main) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Reef.Conformance (run)
import Reef.Odonus (defaultOdonus)
import Reef.PitchSetGolden (tableRender)
import Reef.Protocol (decodeOdonus, encodeOdonus)
import Test.Assert (assertEqual')

main :: Effect Unit
main = do
  assertEqual' "Odonus stepEmit conformance (defaultOdonus, scale source, 32 steps)"
    { actual: run, expected: golden }
  log "Reef Odonus conformance golden: OK"
  -- Protocol round-trip: encode -> decode -> re-encode must reproduce the
  -- original JSON, proving the wire codec is faithful for the full Odonus
  -- record. (Odonus has no Show, so we compare the canonical JSON form.)
  let json = encodeOdonus defaultOdonus
  case decodeOdonus json of
    Left errs ->
      assertEqual' "Protocol decode of encoded defaultOdonus must succeed"
        { actual: "decode error: " <> show errs, expected: "Right _" }
    Right odo ->
      assertEqual' "Protocol round-trip (decode . encode is faithful)"
        { actual: encodeOdonus odo, expected: json }
  log "Reef.Protocol round-trip: OK"
  -- The frozen quantisation table (project_reef_quantisation_realize). Pins the
  -- agreed PitchSet examples: literal offsets, flat-equal mapping, finite vs
  -- periodic, octave vs non-octave (period 19). Row 3 is the "don't lop the 9th"
  -- proof — index 4 realizes to D4=62, the high ninth, not a folded base-octave D.
  assertEqual' "PitchSet quantisation table golden"
    { actual: tableRender, expected: pitchSetGolden }
  log "Reef.PitchSet quantisation table golden: OK"

-- | The frozen render of `Reef.Conformance.run`. Head 0 walks the default scale
-- | quantisation (one 16-cell bar, repeated). Captured 2026-06-30; identical
-- | under node and the BEAM.
golden :: String
golden = """  1 | h0 p62 d1 r1 v100
  2 | h0 p62 d1 r1 v100
  3 | h0 p63 d1 r1 v100
  4 | h0 p65 d1 r1 v100
  5 | h0 p65 d1 r1 v100
  6 | h0 p67 d1 r1 v100
  7 | h0 p67 d1 r1 v100
  8 | h0 p68 d1 r1 v100
  9 | h0 p70 d1 r1 v100
 10 | h0 p70 d1 r1 v100
 11 | h0 p72 d1 r1 v100
 12 | h0 p72 d1 r1 v100
 13 | h0 p74 d1 r1 v100
 14 | h0 p74 d1 r1 v100
 15 | h0 p75 d1 r1 v100
 16 | h0 p60 d1 r1 v100
 17 | h0 p62 d1 r1 v100
 18 | h0 p62 d1 r1 v100
 19 | h0 p63 d1 r1 v100
 20 | h0 p65 d1 r1 v100
 21 | h0 p65 d1 r1 v100
 22 | h0 p67 d1 r1 v100
 23 | h0 p67 d1 r1 v100
 24 | h0 p68 d1 r1 v100
 25 | h0 p70 d1 r1 v100
 26 | h0 p70 d1 r1 v100
 27 | h0 p72 d1 r1 v100
 28 | h0 p72 d1 r1 v100
 29 | h0 p74 d1 r1 v100
 30 | h0 p74 d1 r1 v100
 31 | h0 p75 d1 r1 v100
 32 | h0 p60 d1 r1 v100"""

-- | The frozen quantisation table — agreed with AC 2026-06-30. Captured from
-- | Reef.PitchSetGolden.tableRender; identical under node and the BEAM.
pitchSetGolden :: String
pitchSetGolden = """1 | C pentatonic | 1 octave | period 12 | span 1
  i0  0-19  -> 48 C3
  i1  20-39  -> 50 D3
  i2  40-59  -> 52 E3
  i3  60-79  -> 55 G3
  i4  80-99  -> 57 A3

2 | C pentatonic | 3 octaves | period 12 | span 3 (flat-equal over 15)
  i0  0-6  -> 36 C2
  i1  7-13  -> 38 D2
  i2  14-19  -> 40 E2
  i3  20-26  -> 43 G2
  i4  27-33  -> 45 A2
  i5  34-39  -> 48 C3
  i6  40-46  -> 50 D3
  i7  47-53  -> 52 E3
  i8  54-59  -> 55 G3
  i9  60-66  -> 57 A3
  i10  67-73  -> 60 C4
  i11  74-79  -> 62 D4
  i12  80-86  -> 64 E4
  i13  87-93  -> 67 G4
  i14  94-99  -> 69 A4

3 | extended chord | finite (don't lop the 9th)
  i0  0-12  -> 36 C2
  i1  13-24  -> 43 G2
  i2  25-37  -> 52 E3
  i3  38-49  -> 59 B3
  i4  50-62  -> 62 D4
  i5  63-74  -> 66 F#4
  i6  75-87  -> 69 A4
  i7  88-99  -> 72 C5

4 | exotic | period 19 | span 2 (non-octave repetition)
  i0  0-6  -> 36 C2
  i1  7-12  -> 39 D#2
  i2  13-18  -> 41 F2
  i3  19-24  -> 43 G2
  i4  25-31  -> 46 A#2
  i5  32-37  -> 48 C3
  i6  38-43  -> 51 D#3
  i7  44-49  -> 53 F3
  i8  50-56  -> 55 G3
  i9  57-62  -> 58 A#3
  i10  63-68  -> 60 C4
  i11  69-74  -> 62 D4
  i12  75-81  -> 65 F4
  i13  82-87  -> 67 G4
  i14  88-93  -> 70 A#4
  i15  94-99  -> 72 C5"""
