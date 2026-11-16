-- | **A melodic line, generated.** The material of the machine that reads one
-- | long line where Odonus reads a grid (AC, 2026-11-15): long enough for a
-- | fugue, and carrying its own rhythm, since a subject is known by its
-- | rhythm as much as by its notes.
-- |
-- | Sixteen independent draws make a texture; a line of sixty-four needs
-- | memory. So a melody here is **a motif and a plan**, and the notes are
-- | derived (`flatten`), as a clip is stored whole and flattened late:
-- |
-- |   * the **motif** is one bar: a slower head and a quicker tail, walked
-- |     mostly by step, a leap answered by a step back;
-- |   * a **contrast** bar of running notes, for relief;
-- |   * the **plan** is a bar-by-bar list of what is done with them: stated,
-- |     moved up or down the scale (a sequence), turned upside down, its tail
-- |     broken off and repeated, and every fourth bar a cadence, on the fifth
-- |     mid-way and on the tonic at the end.
-- |
-- | One change to the motif changes every bar made from it, which is what the
-- | grid gave Odonus (one cell, four heads) a level up.
-- |
-- | Degrees are steps of a seven-note scale from the tonic (0; 4 is the
-- | fifth, 7 the octave, negatives below). Lengths are in steps, sixteen to
-- | the bar. Integer arithmetic and the shared PRNG throughout, with no
-- | division of a negative (purerl truncates where JS floors), so both
-- | runtimes draw the same line from the same seed.
module Reef.Melody
  ( Note
  , Op(..)
  , Section
  , Melody
  , barSteps
  , generate
  , flatten
  , lineSteps
  , transpose
  , planText
  , lineText
  ) where

import Prelude

import Data.Array (drop, filter, foldl, length, mapWithIndex, null, range, snoc, take, (!!))
import Data.Foldable (sum)
import Data.Maybe (fromMaybe, maybe)
import Data.String (joinWith)
import Reef.Marbles (Seed, nextInt, seedFrom)

-- | One note of a line, or a rest of that length (its degree is then unused).
type Note = { degree :: Int, dur :: Int, rest :: Boolean }

-- | What a bar of the plan does with the material.
data Op
  = Statement      -- the motif
  | Inversion      -- the motif upside down, about its first note
  | Fragment Int   -- the motif's tail, twice, the second this many steps away
  | Contrast       -- the running bar
  | HalfCadence    -- a run to the fifth, held
  | Cadence        -- a run to the tonic, held

derive instance eqOp :: Eq Op

-- | A bar of the plan: what it does, and how far up the scale it sits.
type Section = { op :: Op, shift :: Int }

type Melody = { motif :: Array Note, contrast :: Array Note, plan :: Array Section }

barSteps :: Int
barSteps = 16

-- ── drawing ──────────────────────────────────────────────────────────────────

type Drawn a = { val :: a, seed :: Seed }

-- | One of a list, each as likely; the first when the list is empty.
pick :: forall a. a -> Array a -> Seed -> Drawn a
pick fallback xs seed =
  let r = nextInt (max 1 (length xs)) seed
  in { val: fromMaybe fallback (xs !! r.n), seed: r.seed }

-- | True `k` times in `n`.
chance :: Int -> Int -> Seed -> Drawn Boolean
chance k n seed = let r = nextInt n seed in { val: r.n < k, seed: r.seed }

-- ── rhythm: half a bar at a time, so the tail breaks off cleanly ─────────────

-- | Half-bars of eight steps, as lengths; a negative length is a rest.
slowHalves :: Array (Array Int)
slowHalves = [ [ 8 ], [ 6, 2 ], [ 4, 4 ], [ 4, 2, 2 ], [ 3, 1, 4 ] ]

mediumHalves :: Array (Array Int)
mediumHalves = [ [ 2, 2, 4 ], [ 2, 2, 2, 2 ], [ 4, 2, 1, 1 ], [ 3, 1, 2, 2 ], [ 2, 1, 1, 4 ], [ -2, 2, 2, 2 ] ]

fastHalves :: Array (Array Int)
fastHalves =
  [ [ 1, 1, 2, 2, 2 ], [ 2, 1, 1, 2, 2 ], [ 1, 1, 1, 1, 2, 2 ], [ 2, 2, 1, 1, 1, 1 ]
  , [ 1, 1, 1, 1, 1, 1, 1, 1 ], [ 2, 1, 1, 2, 1, 1 ], [ -1, 1, 1, 1, 2, 2 ] ]

-- ── pitch: a walk with momentum, a leap answered by a step back ──────────────

type Walk = { notes :: Array Note, at :: Int, dir :: Int, leapt :: Boolean, seed :: Seed }

-- | Walk the lengths from `start`, staying within about a fifth of `centre`.
-- | The first note is `start` itself when `fromStart`, else a move on from it.
walk :: Boolean -> Int -> Int -> Int -> Array Int -> Seed -> Walk
walk fromStart centre start dir0 durs seed0 =
  foldl go { notes: [], at: start, dir: dir0, leapt: false, seed: seed0 } (mapWithIndex (\i d -> { i, d }) durs)
  where
  go w { i, d }
    | d < 0 = w { notes = snoc w.notes { degree: w.at, dur: negate d, rest: true } }
    | i == 0 && fromStart = w { notes = snoc w.notes { degree: start, dur: d, rest: false } }
    | otherwise =
        let
          -- keep going the same way six times in ten; turn at the edge of the range
          turn = chance 4 10 w.seed
          far = w.at - centre
          dir
            | far >= 4 = -1
            | far <= -4 = 1
            | w.leapt = negate w.dir
            | turn.val = negate w.dir
            | otherwise = w.dir
          -- of 100: a repeat 8, a step 58, a third 20, a fourth 9, a fifth 5
          size = nextInt 100 turn.seed
          by
            | w.leapt = 1
            | size.n < 8 = 0
            | size.n < 66 = 1
            | size.n < 86 = 2
            | size.n < 95 = 3
            | otherwise = 4
          at = w.at + dir * by
        in
          { notes: snoc w.notes { degree: at, dur: d, rest: false }, at, dir, leapt: by >= 3, seed: size.seed }

-- ── the melody ───────────────────────────────────────────────────────────────

-- | A melody of `bars` bars (in fours, 4 to 32) from a seed.
generate :: Int -> Seed -> { melody :: Melody, seed :: Seed }
generate bars0 seed0 =
  let
    periods = max 1 (min 8 ((bars0 + 3) / 4))
    -- the motif: tonic seven times in ten, else the fifth; a slower head, a quicker tail
    -- Park–Miller is linear in its seed, so seeds 1, 2, 3 … draw in step
    -- (one's repeat is the next one's repeat): square a draw to part them.
    -- 30000² fits an Int on both runtimes
    mixA = nextInt 30000 seed0
    mixB = nextInt 30000 mixA.seed
    mixed = seedFrom (mixA.n * mixA.n + mixB.n + 1)
    -- and a small seed's first draws are small, so run it in
    warmed = foldl (\sd _ -> (nextInt 2 sd).seed) mixed (range 1 3)
    onFifth = chance 3 10 warmed
    start = if onFifth.val then 4 else 0
    up = chance 6 10 onFifth.seed
    headR = pick [ 4, 4 ] (slowHalves <> mediumHalves) up.seed
    tailR = pick [ 2, 2, 2, 2 ] (filter (_ /= headR.val) (mediumHalves <> fastHalves)) headR.seed
    headW = walk true start start (if up.val then 1 else -1) headR.val tailR.seed
    tailW = walk false start headW.at headW.dir tailR.val headW.seed
    motif = headW.notes <> tailW.notes
    -- the contrast: running notes, from where the motif began, the other way
    c1 = pick [ 1, 1, 1, 1, 1, 1, 1, 1 ] (filter (_ /= tailR.val) fastHalves) tailW.seed
    c2 = pick [ 2, 2, 1, 1, 1, 1 ] fastHalves c1.seed
    contrastW = walk true start start (if up.val then -1 else 1) (c1.val <> c2.val) c2.seed
    -- the plan: how high each period sits (an arch), and its four bars
    climb = nextInt 2 contrastW.seed
    planned = foldl (period periods (2 + climb.n)) { plan: [], seed: climb.seed } (range 0 (periods - 1))
  in
    { melody: { motif, contrast: contrastW.notes, plan: planned.plan }, seed: planned.seed }

-- | One period of four bars: a statement, a sequence of it or the contrast,
-- | a development, and a cadence. Periods rise to the middle of the line and
-- | come back, and within one the bars move a step at a time the same way.
period :: Int -> Int -> { plan :: Array Section, seed :: Seed } -> Int -> { plan :: Array Section, seed :: Seed }
period periods climb acc p =
  let
    final = p == periods - 1
    rising = p * 2 < periods
    dir = if rising then 1 else -1
    -- no higher than a fifth, so the whole line keeps to about two octaves
    base = min 4 (climb * min p (periods - 1 - p))
    -- after the first period, the motif may come back upside down
    inv = chance (if p == 0 then 0 else 4) 10 acc.seed
    op0 = if inv.val then Inversion else Statement
    wide = chance 3 10 inv.seed
    stepBy = dir * (if wide.val then 2 else 1)
    second = nextInt 10 wide.seed
    bar1 = if second.n < 7 then { op: op0, shift: base + stepBy } else { op: Contrast, shift: base }
    third = nextInt 10 second.seed
    bar2
      | third.n < 6 = { op: Fragment dir, shift: base + 2 * stepBy }
      | third.n < 8 = { op: Contrast, shift: base + stepBy }
      | otherwise = { op: op0, shift: base + 2 * stepBy }
    bar3 = { op: if final then Cadence else HalfCadence, shift: 0 }
  in
    { plan: acc.plan <> [ { op: op0, shift: base }, bar1, bar2, bar3 ], seed: third.seed }

-- ── from the plan to the notes ───────────────────────────────────────────────

transpose :: Int -> Array Note -> Array Note
transpose by = map \n -> n { degree = n.degree + by }

-- | The notes the plan makes of the motif, in order.
flatten :: Melody -> Array Note
flatten m = (foldl bar { notes: [], last: firstDegree m.motif } m.plan).notes
  where
  bar acc s =
    let
      ns = case s.op of
        Statement -> transpose s.shift m.motif
        Inversion -> transpose s.shift (invert m.motif)
        Contrast -> transpose s.shift m.contrast
        Fragment by -> let t = tailOf m.motif in transpose s.shift t <> transpose (s.shift + by) t
        HalfCadence -> cadence (headRhythm m.motif) (nearest 4 acc.last) acc.last
        Cadence -> cadence (headRhythm m.motif) (nearest 0 acc.last) acc.last
    in
      { notes: acc.notes <> ns, last: lastDegree acc.last ns }

firstDegree :: Array Note -> Int
firstDegree ns = maybe 0 _.degree ((filter (not <<< _.rest) ns) !! 0)

lastDegree :: Int -> Array Note -> Int
lastDegree fallback ns = let sounding = filter (not <<< _.rest) ns in maybe fallback _.degree (sounding !! (length sounding - 1))

-- | Upside down, about the first note.
invert :: Array Note -> Array Note
invert ns = let axis = firstDegree ns in map (\n -> n { degree = 2 * axis - n.degree }) ns

-- | The notes of the second half of a bar. A motif is made a half-bar at a
-- | time, so a note never straddles the middle.
tailOf :: Array Note -> Array Note
tailOf ns = (foldl go { at: 0, out: [] } ns).out
  where
  go acc n = { at: acc.at + n.dur, out: if acc.at >= barSteps / 2 then snoc acc.out n else acc.out }

-- | The lengths of the first half of a bar, a rest as a negative length.
headRhythm :: Array Note -> Array Int
headRhythm ns = (foldl go { at: 0, out: [] } ns).out
  where
  go acc n = { at: acc.at + n.dur, out: if acc.at < barSteps / 2 then snoc acc.out (if n.rest then negate n.dur else n.dur) else acc.out }

-- | The octave of `degree` (0..6) nearest to `from`.
nearest :: Int -> Int -> Int
nearest degree from =
  let
    -- floor (from - degree + 3) / 7, without dividing a negative
    d = from - degree + 3
    octave = if d >= 0 then d / 7 else negate ((6 - d) / 7)
  in
    degree + 7 * octave

-- | A cadence bar: a run by step into the target over the motif's own head
-- | rhythm (four quavers if that is one long note), then the target, held
-- | half a bar. The run comes from the side the line was on.
cadence :: Array Int -> Int -> Int -> Array Note
cadence rhythm0 target from =
  let
    rhythm = if length (filter (_ > 0) rhythm0) < 2 then [ 2, 2, 2, 2 ] else rhythm0
    side = if from >= target then 1 else -1
    sounding = length (filter (_ > 0) rhythm)
    run = (foldl step { k: 0, out: [] } rhythm).out
    step acc d
      | d < 0 = acc { out = snoc acc.out { degree: target, dur: negate d, rest: true } }
      | otherwise = { k: acc.k + 1, out: snoc acc.out { degree: target + side * (sounding - acc.k), dur: d, rest: false } }
  in
    run <> [ { degree: target, dur: barSteps / 2, rest: false } ]

-- | A line's length in steps.
lineSteps :: Array Note -> Int
lineSteps = sum <<< map _.dur

-- ── as text ──────────────────────────────────────────────────────────────────

-- | The plan, a bar a word, a bar-line every four: `A A+1 frag+ hc | …`.
planText :: Melody -> String
planText m = joinWith " | " (map (joinWith " " <<< map word) (fours m.plan))
  where
  fours xs = if null xs then [] else [ take 4 xs ] <> fours (drop 4 xs)
  at s = if s.shift == 0 then "" else if s.shift > 0 then "+" <> show s.shift else show s.shift
  word s = case s.op of
    Statement -> "A" <> at s
    Inversion -> "inv" <> at s
    Contrast -> "B" <> at s
    Fragment by -> "frag" <> (if by > 0 then "↑" else "↓") <> at s
    HalfCadence -> "hc"
    Cadence -> "cad"

-- | A line as Tidal mini-notation: degrees, `@n` a length in steps, `~` a
-- | rest; a bar a bracket. Over `lineSteps / 16` cycles it is a bar a
-- | cycle (`slow`), and `scale` gives the degrees their notes.
lineText :: Array Note -> String
lineText ns = joinWith " " (map (\b -> "[" <> joinWith " " (map word b) <> "]") (bars ns))
  where
  word n = (if n.rest then "~" else show n.degree) <> (if n.dur == 1 then "" else "@" <> show n.dur)
  bars xs = (foldl go { at: 0, cur: [], out: [] } xs) # \acc -> if null acc.cur then acc.out else snoc acc.out acc.cur
  go acc n =
    let at = acc.at + n.dur
    in if at `mod` barSteps == 0 then { at, cur: [], out: snoc acc.out (snoc acc.cur n) } else acc { at = at, cur = snoc acc.cur n }
