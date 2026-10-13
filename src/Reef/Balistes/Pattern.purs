-- | Reef.Balistes.Pattern — a Balistes fixed rhythm, as the page edits it and
-- | the rig reads it off the stage (moved from Triggerfish.Balistes.Pattern
-- | 2026-10-13, with its text form, `Reef.Balistes.Lepidoptera`).
-- |
-- | A Balistes panel plays a *Pattern*. Grids (the X/Y morph engine in
-- | `Model`) is one — special, generative, mutatable. The other kind is a
-- | **fixed rhythm**: a literal `lane × step` velocity grid, the kind you get
-- | from transcribing a starter beat (the ~30 from *The Secrets of Dance Music
-- | Production*). It doesn't morph; it's a labelled loop you recall instantly
-- | and tweak.
-- |
-- | Every fixed rhythm is laid against ONE canonical 16-lane kit (`canonKit`),
-- | so patterns stack, swap samples, and sequence against the same rows. Import
-- | maps each source MIDI note onto a lane by GM number (`laneFromNote`); a
-- | pattern that only uses six voices leaves the other ten lanes empty (they
-- | fold away in the compact view).
module Reef.Balistes.Pattern
  ( KitLane
  , canonKit
  , kitSize
  , laneName
  , laneNote
  , laneFromNote
  , laneIndexOf
  , FixedPattern
  , Cell
  , TrigCond(..)
  , emptyCell
  , hitCell
  , cellAt
  , cellTweaked
  , condFires
  , condLabel
  , condPresets
  , cycleCond
  , modifyCell
  , velAt
  , firesAt
  , usedLanes
  , setVelAt
  , emptyGrid
  , buildGrid
  , emptyPattern
  , toWire
  ) where

import Prelude

import Data.Array (any, filter, findIndex, length, modifyAt, range, replicate, updateAt, (!!))
import Data.Foldable (foldl)
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Reef.Balistes.Fixed as Fixed
import Reef.Balistes.Kit as Kit

-- | One row of the shared kit. The kit itself lives in reef
-- | (`Reef.Balistes.Kit`), since the rig names its lanes too.
type KitLane = Kit.KitLane

canonKit :: Array KitLane
canonKit = Kit.canonKit

kitSize :: Int
kitSize = Kit.kitSize

laneName :: Int -> String
laneName i = fromMaybe "?" (map _.name (canonKit !! i))

laneNote :: Int -> Int
laneNote = Kit.laneNote

-- | The kit lane a GM note belongs to (Nothing if the note isn't in the kit —
-- | the importer then asks the user where it goes).
laneFromNote :: Int -> Maybe Int
laneFromNote n = findIndex (\k -> k.note == n) canonKit

-- | The kit lane index for a name (`"BD"` → 0). Used parsing the eDSL form back.
laneIndexOf :: String -> Maybe Int
laneIndexOf nm = findIndex (\k -> k.name == nm) canonKit

-- | A trig condition — when (on which loop pass) a hit fires. `CAlways` always;
-- | `CEvery x y` fires only on pass `x` of every `y` (Elektron-style 1:4 etc).
data TrigCond = CAlways | CEvery Int Int

derive instance eqTrigCond :: Eq TrigCond

-- | One cell: a velocity (0 = no hit) plus the per-cell overlay the NOTE
-- | inspector edits — firing probability (%), trig condition, and ratchet
-- | (subdivide the hit into n retriggers). The overlay is meaningful only when
-- | `vel > 0`.
type Cell =
  { vel :: Int        -- 0 = no hit, else 1..127
  , prob :: Int       -- 0..100 % chance to fire on a given pass
  , cond :: TrigCond  -- which passes fire
  , ratchet :: Int    -- 1..8 retriggers
  }

-- | A silent cell — the blank, and what clearing a cell returns to.
emptyCell :: Cell
emptyCell = { vel: 0, prob: 100, cond: CAlways, ratchet: 1 }

-- | A plain hit at velocity v with default overlay.
hitCell :: Int -> Cell
hitCell v = emptyCell { vel = v }

-- | Has this cell's overlay been tweaked off its defaults? (Velocity is shown
-- | by intensity already, so the grid's tweak-dot flags only prob/cond/ratchet.)
cellTweaked :: Cell -> Boolean
cellTweaked c = c.prob /= 100 || c.cond /= CAlways || c.ratchet /= 1

-- | Does this condition fire on loop pass `loop` (0-based)?
condFires :: TrigCond -> Int -> Boolean
condFires CAlways _ = true
condFires (CEvery x y) loop = if y <= 0 then true else (loop `mod` y) == ((x - 1) `mod` y)

condLabel :: TrigCond -> String
condLabel CAlways = "—"
condLabel (CEvery x y) = show x <> ":" <> show y

-- | The trig-condition presets the inspector cycles through.
condPresets :: Array TrigCond
condPresets =
  [ CAlways
  , CEvery 1 2, CEvery 2 2
  , CEvery 1 3
  , CEvery 1 4, CEvery 2 4, CEvery 3 4, CEvery 4 4
  ]

-- | Step to the next preset condition (wraps).
cycleCond :: TrigCond -> TrigCond
cycleCond c =
  let i = fromMaybe 0 (findIndex (_ == c) condPresets)
  in fromMaybe CAlways (condPresets !! ((i + 1) `mod` length condPresets))

-- | A literal rhythm: a name, a step count (16 or 32) and a dense
-- | `kitSize × steps` grid of cells, lane index aligned with `canonKit`. It
-- | names no notes: a lane is a drum, and the note that reaches an instrument
-- | is set where the hit leaves, in the routing (2026-10-13).
type FixedPattern =
  { name :: String
  , steps :: Int
  , grid :: Array (Array Cell)
  }

cellAt :: FixedPattern -> Int -> Int -> Cell
cellAt p lane step = fromMaybe emptyCell ((p.grid !! lane) >>= (_ !! step))

-- | Apply a function to one cell.
modifyCell :: Int -> Int -> (Cell -> Cell) -> FixedPattern -> FixedPattern
modifyCell lane step f p =
  p { grid = fromMaybe p.grid (modifyAt lane setStep p.grid) }
  where
  setStep row = fromMaybe row (modifyAt step f row)

velAt :: FixedPattern -> Int -> Int -> Int
velAt p lane step = (cellAt p lane step).vel

firesAt :: FixedPattern -> Int -> Int -> Boolean
firesAt p lane step = velAt p lane step > 0

-- | Set a cell's velocity (0 clears the whole cell — a silent cell carries no
-- | overlay). A positive velocity keeps the cell's prob/cond/ratchet.
setVelAt :: Int -> Int -> Int -> FixedPattern -> FixedPattern
setVelAt lane step v =
  modifyCell lane step \c -> if v <= 0 then emptyCell else c { vel = clampVel v }
  where
  clampVel x = if x < 0 then 0 else if x > 127 then 127 else x

-- | Which lanes carry any hit — the rows worth showing in the compact view.
usedLanes :: FixedPattern -> Array Int
usedLanes p = filter (\l -> any (\c -> c.vel > 0) (fromMaybe [] (p.grid !! l))) (range 0 (kitSize - 1))

-- | An all-silent `kitSize × steps` grid — the blank canvas.
emptyGrid :: Int -> Array (Array Cell)
emptyGrid steps = replicate kitSize (replicate steps emptyCell)

-- | Build a grid from sparse `(lane, velocityRow)` pairs over a blank canvas.
buildGrid :: Int -> Array (Tuple Int (Array Int)) -> Array (Array Cell)
buildGrid steps rows = foldl place (emptyGrid steps) rows
  where
  place g (Tuple lane row) = fromMaybe g (updateAt lane (map hitCell row) g)

-- | A fresh, all-silent named pattern (the "+ NEW" template).
emptyPattern :: String -> Int -> FixedPattern
emptyPattern name steps =
  { name, steps, grid: emptyGrid steps }

-- | The rhythm as the shared eval plays it (`Reef.Balistes.Fixed`): its trig
-- | condition flattened to two ints, its name dropped.
toWire :: FixedPattern -> Fixed.FixedPattern
toWire p =
  { steps: p.steps
  , grid: map (map cellOf) p.grid
  }
  where
  cellOf c =
    let cond = case c.cond of
                 CAlways -> { x: 0, y: 0 }
                 CEvery x y -> { x, y }
    in { vel: c.vel, prob: c.prob, ratchet: c.ratchet, condX: cond.x, condY: cond.y }
