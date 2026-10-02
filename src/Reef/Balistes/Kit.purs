-- | **The drum kit's lanes**, shared by everything that plays drums: Balistes'
-- | engines (Grids, Rhythm), the routing table (`Reef.Routing`, whose `notes`
-- | are these lanes' notes), and a Tidal line on the rig (`drums $ s "bd sn"`),
-- | which names a lane per event.
-- |
-- | The canonical 16-lane kit, ordered kit-classically top to bottom (kick,
-- | snares and claps, hats, toms, cymbals, percussion). Notes are GM
-- | percussion, so imported MIDI lands on the right lane by number. Samples are
-- | swapped freely downstream (Ableton, SuperDirt, the modular): these notes
-- | are only the wire.
module Reef.Balistes.Kit
  ( KitLane
  , canonKit
  , kitSize
  , laneOfName
  ) where

import Prelude

import Data.Array (findIndex, length)
import Data.Maybe (Maybe(..))
import Data.String (toLower)

-- | One row of the shared kit: a clear name plus the GM note it sends.
type KitLane = { name :: String, note :: Int }

canonKit :: Array KitLane
canonKit =
  [ { name: "BD", note: 36 } --  0  bass drum
  , { name: "SD", note: 38 } --  1  snare
  , { name: "CP", note: 39 } --  2  hand clap
  , { name: "RS", note: 37 } --  3  rim / side stick
  , { name: "CH", note: 42 } --  4  closed hat
  , { name: "PH", note: 44 } --  5  pedal hat
  , { name: "OH", note: 46 } --  6  open hat
  , { name: "LT", note: 41 } --  7  low tom
  , { name: "MT", note: 47 } --  8  mid tom
  , { name: "HT", note: 50 } --  9  high tom
  , { name: "RD", note: 51 } -- 10  ride
  , { name: "RB", note: 53 } -- 11  ride bell
  , { name: "CR", note: 49 } -- 12  crash
  , { name: "CW", note: 56 } -- 13  cowbell
  , { name: "TB", note: 54 } -- 14  tambourine
  , { name: "SH", note: 70 } -- 15  shaker / maracas
  ]

kitSize :: Int
kitSize = length canonKit

-- | The lane a name addresses, case-insensitively: the kit's own names (`bd`,
-- | `sd`, `ch`) and the Dirt-Samples names a Tidal line is likely to use for
-- | the same drums (`sn`, `hh`, `rim`, `cb`).
laneOfName :: String -> Maybe Int
laneOfName raw = case findIndex (\k -> toLower k.name == name) canonKit of
  Just i -> Just i
  Nothing -> alias name
  where
  name = toLower raw
  alias = case _ of
    "kick" -> Just 0
    "sn" -> Just 1
    "snare" -> Just 1
    "clap" -> Just 2
    "rim" -> Just 3
    "hh" -> Just 4
    "hat" -> Just 4
    "ride" -> Just 10
    "crash" -> Just 12
    "cb" -> Just 13
    "cowbell" -> Just 13
    "tamb" -> Just 14
    "shaker" -> Just 15
    _ -> Nothing
