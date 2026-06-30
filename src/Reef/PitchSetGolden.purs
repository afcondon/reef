-- | The frozen quantisation table (project_reef_quantisation_realize). Renders a
-- | handful of `PitchSet`s as their input-band -> MIDI mapping, so the agreed
-- | examples become a pinned, byte-identical-across-backends golden. Pure (no
-- | Effect) so the cross-runtime script can render it under erl too.
module Reef.PitchSetGolden (tableRender) where

import Prelude

import Data.Array ((..), (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Reef.PitchSet (PitchSet(..), cardinality, realize)

type Row = { name :: String, ps :: PitchSet, span :: Int }

rows :: Array Row
rows =
  [ { name: "1 | C pentatonic | 1 octave | period 12 | span 1"
    , ps: PitchSet { offsets: [ 0, 2, 4, 7, 9 ], root: 48, period: Just 12 }
    , span: 1
    }
  , { name: "2 | C pentatonic | 3 octaves | period 12 | span 3 (flat-equal over 15)"
    , ps: PitchSet { offsets: [ 0, 2, 4, 7, 9 ], root: 36, period: Just 12 }
    , span: 3
    }
  , { name: "3 | extended chord | finite (don't lop the 9th)"
    , ps: PitchSet { offsets: [ 0, 7, 16, 23, 26, 30, 33, 36 ], root: 36, period: Nothing }
    , span: 1
    }
  , { name: "4 | exotic | period 19 | span 2 (non-octave repetition)"
    , ps: PitchSet { offsets: [ 0, 3, 5, 7, 10, 12, 15, 17 ], root: 36, period: Just 19 }
    , span: 2
    }
  ]

-- | Each row: its header, then one line per slot — the input band that lands on
-- | that slot and the MIDI pitch (+ name) it realizes to.
tableRender :: String
tableRender = joinWith "\n\n" (map renderRow rows)

renderRow :: Row -> String
renderRow r =
  let slots = r.span * cardinality r.ps
  in r.name <> "\n" <> joinWith "\n" (map (renderBand r.ps slots) (0 .. (slots - 1)))

renderBand :: PitchSet -> Int -> Int -> String
renderBand ps slots i =
  let lo = ceilDiv (i * 100) slots
      hi = ceilDiv ((i + 1) * 100) slots - 1
      midi = realize ps i
  in "  i" <> show i <> "  " <> show lo <> "-" <> show hi
       <> "  -> " <> show midi <> " " <> midiName midi

ceilDiv :: Int -> Int -> Int
ceilDiv a b = (a + b - 1) `div` b

-- | MIDI note name, scientific pitch (middle C = C4 = 60).
midiName :: Int -> String
midiName n =
  let names = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]
  in fromMaybe "?" (names !! (n `mod` 12)) <> show ((n `div` 12) - 1)
