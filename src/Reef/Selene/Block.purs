-- | **Blocks**: named virtual modules, each a whole bank of eight from a few
-- | musical parameters (docs/kb/plans/selene-in-tidal.md, AC 2026-10-05: "that
-- | part of modular more programmable, and cheaper in modules and hp").
-- |
-- |     selene $ ochd es9main # rate 0.03 # spread 40
-- |     selene $ divider es9gt0 # by "1 2 3 4 6 8 12 16"
-- |     selene $ toussaint es9gt1
-- |     selene $ chord es98cv0 # root D3 # quality min9
-- |
-- | A block takes the place of the kind. Its own parameters set every slot;
-- | any other `#` terms on the line then apply as plain parameters of the kind
-- | it makes (`ochd es9main # tri 0.5`). A bank made by a block is plain
-- | settings afterwards: it prints back as an `lfo …` line.
-- |
-- | Every block is a definition here, not code in a daemon, so a new one costs
-- | no hardware. Pure and compiled to both columns; any randomness is seeded
-- | and kept in small integers, so the BEAM and JS draw the same.
module Reef.Selene.Block
  ( Block
  , blocks
  , blockNamed
  , expand
  ) where

import Prelude

import Data.Array (filter, find, foldl, length, range, (!!))
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String as Str
import Data.Tuple (Tuple(..))
import Reef.Numeric (pow)
import Reef.Selene.Model as M
import Reef.Selene.Rack as Rack

-- | A block: its name, the kind of bank it makes, what it is (for the
-- | cheatsheet), and its parameters with their defaults, as a line writes them.
type Block =
  { name :: String
  , kind :: M.GenKind
  , what :: String
  , params :: Array { name :: String, default :: String, what :: String }
  }

blocks :: Array Block
blocks =
  [ { name: "ochd", kind: M.KLfo
    , what: "eight free LFOs, slow to fast at uneven ratios so they never lock (after Instruō's ochd)"
    , params:
        [ p "rate" "0.03" "the slowest, in Hz"
        , p "spread" "40" "the fastest is this many times the slowest"
        , p "shape" "tri" "sin, tri, saw or sqr"
        , p "depth" "0.8" "how far each swings"
        ]
    }
  , { name: "quadrature", kind: M.KLfo
    , what: "two sets of four LFOs a quarter-cycle apart (0°, 90°, 180°, 270°)"
    , params:
        [ p "rate" "0.25" "the first four, in Hz"
        , p "ratio" "1.5" "the second four run this much faster"
        , p "shape" "sin" "sin, tri, saw or sqr"
        , p "depth" "0.8" "how far each swings"
        ]
    }
  , { name: "drift", kind: M.KLfo
    , what: "eight slow, smooth random voltages, each at its own pace"
    , params:
        [ p "rate" "0.05" "the slowest, in Hz"
        , p "depth" "0.6" "how far they wander"
        ]
    }
  , { name: "divider", kind: M.KEuclid
    , what: "a clock divider: a pulse every n steps of the master clock, for each n"
    , params:
        [ p "by" "1 2 3 4 5 6 7 8" "the divisions, one per output"
        , p "rate" "1" "the master clock, in steps per beat (1 quarters, 4 sixteenths)"
        ]
    }
  , { name: "toussaint", kind: M.KEuclid
    , what: "eight traditional rhythms that are euclidean (Toussaint, 2005): tresillo, cinquillo, ruchenitza, khafif-e-ramal, the West African bell, Venda clapping, bossa nova, a Central African bell"
    , params: []
    }
  , { name: "polymeter", kind: M.KEuclid
    , what: "the same hits over 5 to 12 steps, phasing against each other"
    , params:
        [ p "hits" "3" "hits in every cycle"
        , p "rate" "4" "steps per beat"
        ]
    }
  , { name: "scatter", kind: M.KEuclid
    , what: "eight euclidean rhythms drawn at random, kept musical (a quarter to a half of the steps hit); another seed, another set"
    , params:
        [ p "seed" "1" "which set"
        , p "rate" "4" "steps per beat"
        ]
    }
  , { name: "chord", kind: M.KNote
    , what: "a chord's notes climbing the octaves across eight outputs"
    , params:
        [ p "root" "C3" "the lowest note"
        , p "quality" "maj7" "maj, min, maj7, min7, dom7, min9, sus2, sus4, dim, aug"
        ]
    }
  ]
  where
  p name default what = { name, default, what }

blockNamed :: String -> Maybe Block
blockNamed n = find (\b -> b.name == n) blocks

-- | A block's bank from the line's terms: its own parameters (the rest are
-- | handed back, to apply as plain parameters).
expand
  :: Block
  -> Array { param :: String, values :: Array String }
  -> Either String { bank :: M.GenBank, rest :: Array { param :: String, values :: Array String } }
expand b terms = do
  let
    own t = find (\q -> q.name == t.param) b.params /= Nothing
    rest = filter (not <<< own) terms
    vals name = fromMaybe [] (map _.values (find (\t -> t.param == name) terms))
      # \vs -> if length vs == 0 then words (fromMaybe "" (map _.default (find (\q -> q.name == name) b.params))) else vs
    one name = fromMaybe "" (vals name !! 0)
    num name = note (b.name <> ": " <> name <> " wants a number, not " <> one name) (Number.fromString (one name))
    int name = note (b.name <> ": " <> name <> " wants a whole number, not " <> one name) (Int.fromString (one name))
  bank <- case b.name of
    "ochd" -> do
      rate <- num "rate"
      spread <- num "spread"
      depth <- num "depth"
      shape <- shapeOf b.name (one "shape")
      let
        -- geometric from slowest to fastest, nudged off the exact ratios so
        -- no two outputs lock to each other
        nudge = [ 1.0, 1.031, 0.973, 1.047, 0.962, 1.021, 0.984, 1.0 ]
        r i = rate * pow spread (Int.toNumber i / 7.0) * fromMaybe 1.0 (nudge !! i)
      pure (M.GLfo (map (\i -> lfo (r i) 0.0 shape depth) slots))
    "quadrature" -> do
      rate <- num "rate"
      ratio <- num "ratio"
      depth <- num "depth"
      shape <- shapeOf b.name (one "shape")
      pure (M.GLfo (map (\i -> lfo (if i < 4 then rate else rate * ratio) (Int.toNumber (i `mod` 4) * 0.25) shape depth) slots))
    "drift" -> do
      rate <- num "rate"
      depth <- num "depth"
      let pace = [ 1.0, 1.37, 1.71, 2.13, 2.62, 3.29, 4.07, 5.11 ]
      pure (M.GLfo (map (\i -> (lfo (rate * fromMaybe 1.0 (pace !! i)) (Int.toNumber i * 0.125) Sin 0.0) { rnd = depth }) slots))
    "divider" -> do
      rate <- int "rate"
      by <- traverse' (\v -> note ("divider: by wants whole numbers, not " <> v) (Int.fromString v)) (vals "by")
      let n i = clamp 1 32 (fromMaybe 1 (by !! (i `mod` length by)))
      pure (M.GEuclid (map (\i -> { beats: 1, steps: n i, rate: max 1 rate, accentRate: 0 }) slots))
    "toussaint" ->
      let
        rhythms = [ Tuple 3 8, Tuple 5 8, Tuple 4 7, Tuple 2 5, Tuple 7 12, Tuple 5 12, Tuple 5 16, Tuple 9 16 ]
        -- each on its natural grid: twelves in triplet eighths, sixteens in
        -- sixteenths, the rest in eighths
        grid steps = if steps `mod` 3 == 0 then 3 else if steps == 16 then 4 else 2
        slot (Tuple hits steps) = { beats: hits, steps, rate: grid steps, accentRate: 0 }
      in pure (M.GEuclid (map (\i -> slot (fromMaybe (Tuple 3 8) (rhythms !! i))) slots))
    "polymeter" -> do
      hits <- int "hits"
      rate <- int "rate"
      pure (M.GEuclid (map (\i -> { beats: clamp 1 (5 + i) hits, steps: 5 + i, rate: max 1 rate, accentRate: 0 }) slots))
    "scatter" -> do
      seed <- int "seed"
      rate <- int "rate"
      let
        choices = [ 5, 7, 8, 9, 11, 12, 13, 16 ]
        drawn = (foldl (\acc _ ->
          let
            s1 = next acc.s
            s2 = next s1
            -- the generator's low bits cycle quickly: draw from its high ones
            steps = fromMaybe 8 (choices !! ((s1 / 256) `mod` length choices))
            lo = max 1 (steps / 4)
            hi = max lo (steps / 2)
            hits = lo + (s2 / 256) `mod` (hi - lo + 1)
          in { s: s2, out: acc.out <> [ { beats: hits, steps, rate: max 1 rate, accentRate: 0 } ] }) { s: start seed, out: [] } slots).out
      pure (M.GEuclid drawn)
    "chord" -> do
      root <- note ("chord: root wants a note, like C3 or 48, not " <> one "root") (Rack.noteToken (one "root"))
      shape <- note ("chord: no quality " <> one "quality" <> " (maj min maj7 min7 dom7 min9 sus2 sus4 dim aug)") (quality (one "quality"))
      let
        k = length shape
        noteAt i = root + fromMaybe 0 (shape !! (i `mod` k)) + 12 * (i / k)
      pure (M.GNote (map (\i -> { note: clamp 0 127 (noteAt i) }) slots))
    other -> Left ("no block " <> other)
  pure { bank, rest }
  where
  slots = range 0 (M.slotCount - 1)

data Shape = Sin | Tri | Saw | Sqr

shapeOf :: String -> String -> Either String Shape
shapeOf block = case _ of
  "sin" -> Right Sin
  "tri" -> Right Tri
  "saw" -> Right Saw
  "sqr" -> Right Sqr
  s -> Left (block <> ": shape is sin, tri, saw or sqr, not " <> s)

-- | One LFO slot of a shape and depth, its rate kept to the rack's three
-- | places so it prints back as it is.
lfo :: Number -> Number -> Shape -> Number -> M.ModSlot
lfo rate phase shape depth =
  { rate: places rate, phase: places phase, level: 0.0
  , sin: if isSin then depth else 0.0
  , tri: if isTri then depth else 0.0
  , saw: if isSaw then depth else 0.0
  , sqr: if isSqr then depth else 0.0
  , rnd: 0.0, nse: 0.0
  }
  where
  isSin = case shape of
    Sin -> true
    _ -> false
  isTri = case shape of
    Tri -> true
    _ -> false
  isSaw = case shape of
    Saw -> true
    _ -> false
  isSqr = case shape of
    Sqr -> true
    _ -> false

places :: Number -> Number
places x = Int.toNumber (Int.round (x * 1000.0)) / 1000.0

-- | Intervals above the root, a chord's shape.
quality :: String -> Maybe (Array Int)
quality = case _ of
  "maj" -> Just [ 0, 4, 7 ]
  "min" -> Just [ 0, 3, 7 ]
  "maj7" -> Just [ 0, 4, 7, 11 ]
  "min7" -> Just [ 0, 3, 7, 10 ]
  "dom7" -> Just [ 0, 4, 7, 10 ]
  "min9" -> Just [ 0, 3, 7, 10, 14 ]
  "sus2" -> Just [ 0, 2, 7 ]
  "sus4" -> Just [ 0, 5, 7 ]
  "dim" -> Just [ 0, 3, 6 ]
  "aug" -> Just [ 0, 4, 8 ]
  _ -> Nothing

-- | A small generator, kept in integers well under 2^31 so JS (32-bit Int)
-- | and the BEAM (unbounded) draw the same: the ZX81's, 75s + 74 mod 65537.
next :: Int -> Int
next s = (75 * s + 74) `mod` 65537

start :: Int -> Int
start seed = next ((seed `mod` 65537 + 65537) `mod` 65537)

traverse' :: forall a b. (a -> Either String b) -> Array a -> Either String (Array b)
traverse' f = foldl (\acc x -> acc >>= \xs -> (\y -> xs <> [ y ]) <$> f x) (Right [])

words :: String -> Array String
words = filter (_ /= "") <<< Str.split (Str.Pattern " ") <<< Str.trim
