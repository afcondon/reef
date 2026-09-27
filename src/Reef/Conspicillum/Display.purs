-- | **What the module draws, as pure data.** No rendering here: positions,
-- | spans, lanes and colours, which a Halogen view, an SVG printer or a test
-- | can each turn into marks.
-- |
-- | The picture has three scales, one per question (see the design doc's
-- | prototype 2):
-- |
-- | - **the material** (the strip): where in the sound, left to right, as one
-- |   box per segment, where a segment is a bar of a tape, a hit of a virtual
-- |   tape, or a sample of a corpus;
-- | - **time** (the ring): when in the bar, as a wedge per grain while it
-- |   sounds;
-- | - **the cloud**: each grain as it fires, by pan and speed.
-- |
-- | And one key between them: **colour means where in the material.** A wedge
-- | on the ring is the colour of the part of the tape it reads.
module Reef.Conspicillum.Display
  ( SampleFacts
  , TapeFacts
  , Material(..)
  , materialOf
  , materialName
  , Segment
  , segments
  , Location
  , locate
  , Colour
  , colourAt
  , colourCss
  , Wedge
  , wedges
  , lanes
  ) where

import Prelude

import Data.Array (elemIndex, filter, findIndex, foldl, head, index, length, mapWithIndex, snoc, sortBy, updateAt, (..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Reef.Conspicillum.Cloud (Emit)
import Reef.Conspicillum.Decimal (fixed)
import Reef.Conspicillum.Notation (Line)
import Reef.Numeric (ln)

-- ── the material ─────────────────────────────────────────────────────────────

-- | What a scene's set holds, as far as drawing it goes.
type SampleFacts = { index :: Int, seconds :: Number }

-- | What a projected tape says about itself (`project-tape.py`'s `tape.json`).
type TapeFacts = { bars :: Int, bpm :: Number }

data Material
  -- | A virtual tape: bar k is the sample named k-th here.
  = HitsAsBars (Array Int)
  -- | One sample read as a tape of this many bars.
  | WholeTape { sample :: SampleFacts, bars :: Int, bpm :: Maybe Number }
  -- | One sample, not followed: a read head over a single sound.
  | OneSample SampleFacts
  -- | A corpus of samples, laid end to end.
  | ManySamples (Array SampleFacts)

-- | What the scene in this line plays from, given its set.
materialOf :: { line :: Line, samples :: Array SampleFacts, tape :: Maybe TapeFacts } -> Material
materialOf { line, samples, tape } =
  let
    spec = line.spec
    chosen = case line.n of
      Just k -> filter (\s -> s.index == k) samples
      Nothing -> samples
    followed = spec.cloud.follow /= 0.0
    bars = if spec.tape.bars > 1 then spec.tape.bars else maybe 1 _.bars tape
  in
    if line.n == Nothing && spec.tape.samples /= [] then HitsAsBars spec.tape.samples
    else case chosen of
      [ one ]
        | followed || tape /= Nothing -> WholeTape { sample: one, bars, bpm: map _.bpm tape }
        | otherwise -> OneSample one
      _ -> ManySamples chosen

materialName :: Material -> String
materialName = case _ of
  HitsAsBars _ -> "hits, one a bar"
  WholeTape _ -> "the tape"
  OneSample s -> "sample " <> show s.index
  ManySamples ss -> show (length ss) <> " samples"

-- ── segments along the strip ─────────────────────────────────────────────────

-- | A stretch of the strip, `from` and `to` in [0, 1] left to right.
type Segment = { label :: String, from :: Number, to :: Number }

-- | The strip's boxes. A corpus's samples are as wide as the log of their
-- | length, as Quadrat draws them, so a hit is visibly shorter than a chord;
-- | bars and hits-as-bars are all one width.
segments :: Material -> Array Segment
segments material = case material of
  HitsAsBars keys -> spread (map (\k -> { label: show k, weight: 1.0 }) keys)
  WholeTape t -> spread (map (\b -> { label: if t.bars > 1 then show b else "", weight: 1.0 }) (1 .. max 1 t.bars))
  OneSample _ -> spread [ { label: "", weight: 1.0 } ]
  ManySamples ss -> spread (map (\s -> { label: show s.index, weight: ln (max 0.05 s.seconds / 0.025) }) ss)
  where
  spread parts =
    let
      total = foldl (\a p -> a + p.weight) 0.0 parts
      step acc p = { at: acc.at + p.weight, out: snoc acc.out { label: p.label, from: acc.at / total, to: (acc.at + p.weight) / total } }
    in
      (foldl step { at: 0.0, out: [] } parts).out

-- | Where a grain reads: which segment, how far into it, and so how far
-- | along the whole strip.
type Location = { segment :: Int, within :: Number, along :: Number }

-- | `sample` is the grain's `n`, `reading` its `begin` or `end`: a fraction of
-- | the whole tape for a tape, of the one sample otherwise.
locate :: Material -> Int -> Number -> Location
locate material sample reading =
  let
    x = clampUnit reading
    { segment, within } = case material of
      HitsAsBars keys -> { segment: fromMaybe 0 (elemIndex sample keys), within: x }
      WholeTape t ->
        let
          bars = max 1 t.bars
          y = min (toNumber bars - 1.0e-9) (x * toNumber bars)
          k = floor y
        in
          { segment: k, within: y - toNumber k }
      OneSample _ -> { segment: 0, within: x }
      ManySamples ss -> { segment: fromMaybe 0 (findIndex (\s -> s.index == sample) ss), within: x }
    seg = fromMaybe { label: "", from: 0.0, to: 1.0 } (index (segments material) segment)
  in
    { segment, within, along: seg.from + within * (seg.to - seg.from) }

-- ── colour: where in the material ────────────────────────────────────────────

-- | HSL, with hue in degrees and the rest in percent, so that any renderer
-- | can take it.
type Colour = { hue :: Number, saturation :: Number, lightness :: Number }

-- | One segment: the hue sweeps along it, so a tape played straight is a
-- | rainbow round the ring, a jump is a break in the gradient, a repeat is
-- | one colour twice, and reverse is the gradient running backwards. Several:
-- | a hue per segment and lightness for where in it, Quadrat's rainbow row.
colourAt :: Material -> Location -> Colour
colourAt material at =
  let count = length (segments material)
  in
    if count <= 1 then { hue: at.within * 300.0, saturation: 62.0, lightness: 52.0 }
    else
      { hue: toNumber at.segment * 360.0 / toNumber count
      , saturation: 55.0
      , lightness: 70.0 - at.within * 28.0
      }

colourCss :: Colour -> String
colourCss c = "hsl(" <> fixed 0 c.hue <> " " <> fixed 0 c.saturation <> "% " <> fixed 0 c.lightness <> "%)"

-- ── the ring ─────────────────────────────────────────────────────────────────

-- | A grain on the ring: from its onset for as long as it sounds, but no
-- | further than the next onset, so a dense cloud tiles the ring rather than
-- | piling up. `from` and `to` are fractions of the cycle.
type Wedge = { from :: Number, to :: Number, emit :: Emit }

-- | The wedges of one cycle's grains, in onset order. `orbit` is the scene's
-- | own chain's: the engine may add copies of a grain on the send orbits,
-- | which are the same grain and are not drawn twice.
wedges :: { cycleSeconds :: Number, orbit :: Int } -> Array Emit -> Array Wedge
wedges { cycleSeconds, orbit } emits =
  let
    ours = sortBy (comparing _.at) (filter (\e -> e.chain.orbit == orbit) emits)
    next e = maybe 1.0 _.at (head (filter (\o -> o.at > e.at + 1.0e-9) ours))
    lengthOf e = max 0.004 (e.sustain / cycleSeconds)
  in
    map (\e -> { from: e.at, to: min (e.at + lengthOf e) (next e), emit: e }) ours

-- ── lanes under the strip ────────────────────────────────────────────────────

-- | Pack spans into lanes so none hides another: each span goes in the first
-- | lane whose last span has ended, taking spans left to right. Spans that
-- | only touch share a lane, so a tape played straight needs one. A span is
-- | at least `minimum` wide, as it will be drawn. The result is one lane per
-- | span, in the order given.
lanes :: Number -> Array { from :: Number, to :: Number } -> Array Int
lanes minimum spans =
  let
    order = sortBy (\(Tuple i a) (Tuple j b) -> comparing _.from a b <> compare i j) (mapWithIndex Tuple spans)
    place acc (Tuple i s) =
      let
        lane = fromMaybe (length acc.ends) (findIndex (\end -> end <= s.from + 1.0e-9) acc.ends)
        end = max s.to (s.from + minimum)
        ends = if lane == length acc.ends then snoc acc.ends end else fromMaybe acc.ends (updateAt lane end acc.ends)
      in
        { ends, assigned: snoc acc.assigned (Tuple i lane) }
    assigned = (foldl place { ends: [], assigned: [] } order).assigned
  in
    -- Back into the order given. Not `0 .. (length spans - 1)`: for no spans
    -- that range counts down, to two of them.
    map snd (sortBy (comparing fst) assigned)

clampUnit :: Number -> Number
clampUnit = max 0.0 <<< min 1.0
