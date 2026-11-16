-- | **Melichthys: Odonus's heads over one long line** (AC, 2026-11-15). The
-- | line (`Reef.Melody`) takes the grid's place as the heads' material; the
-- | heads are Odonus's own, on their duration clock, so each holds a note for
-- | its length and a rest is a rest of its own length. Nothing here steps or
-- | renders: that is `Reef.Odonus`, unchanged, reading more than sixteen
-- | cells.
-- |
-- | A cell stores a knob, not a degree, so the line's degrees are laid on the
-- | knob exactly (the inverse of the pipeline's equal mapping, as Triggerfish's
-- | forms do it), over as many octaves as the line spans. The degrees are a
-- | seven-note scale's: under a scale of another size the line keeps its
-- | shape and loses its cadences.
module Reef.Melichthys
  ( load
  , enter
  , draw
  , setEntry
  , restart
  , fresh
  , lineOf
  , waiting
  , Sounded
  , sounded
  , waveAt
  , setWave
  , flatWaves
  , baseVelocity
  , weightsOf
  , setGround
  , setEvery
  , setPull
  , chordsOf
  , flatOf
  , padOf
  ) where

import Prelude

import Data.Array (filter, foldl, length, mapWithIndex, modifyAt, nub, range, replicate, snoc, sort, updateAt, (!!))
import Data.Foldable (maximum, minimum)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Reef.Marbles (seedFrom)
import Reef.Melody (Note, barSteps, flatten, generate, lineSteps)
import Reef.Odonus (Cell, Head, Line, Odonus, Wave, defaultOdonus, floorDivI, knobMax, rateTable, unitRateIx)

-- | A note of the line as the heads will play it: the melody's note, moved
-- | and given its loudness by the line's waves.
type Sounded = { degree :: Int, dur :: Int, rest :: Boolean, vel :: Int }

-- | A note's velocity where no wave reaches it.
baseVelocity :: Int
baseVelocity = 84

-- | Three waves that do nothing: the line as the melody has it, every note
-- | at one loudness.
flatWaves :: Array Wave
flatWaves = replicate 3 { shape: 0, cycles: 1, phase: 0, dyn: 0, bend: 0 }

-- | Wave `i` of a line drawn from `seed`, at step `at` of `total`: -127..127.
-- | Whole numbers throughout, so the browser and the BEAM agree to the unit:
-- | the sine is Bhaskara's quotient (within a fifth of a percent of a sine),
-- | and the random levels are one small hash per cycle of the wave.
waveAt :: Int -> Int -> Wave -> Int -> Int -> Int
waveAt seed i w at total =
  let
    turns = (at * w.cycles * 1024) / max 1 total + w.phase * 64
    p = turns `mod` 1024
    half f = if p < 512 then f p else negate (f (p - 512))
    arch x = (2032 * x * (512 - x)) / (1310720 - 4 * x * (512 - x))
    tent x = ((if x < 256 then x else 512 - x) * 127) / 256
    ramp = (p * 254) / 1023 - 127
    own = (((if seed < 0 then negate seed else seed) `mod` 9973 + 1) * 7919) `mod` 10007
    level = ((own + 1) * ((turns / 1024) * 37 + 11 + 5 * i)) `mod` 10007 `mod` 255 - 127
  in
    case w.shape of
      0 -> half arch
      1 -> half tent
      2 -> ramp
      3 -> negate ramp
      4 -> if p < 512 then 127 else -127
      _ -> level

-- | The line as its waves leave it. One wave sent whole to the dynamics moves
-- | a note forty either side of `baseVelocity`; sent whole to the pitch, an
-- | octave of degrees either side of where the melody put it. The sends add.
-- | A rest stays a rest, and a note keeps its length: the waves never touch
-- | the rhythm.
sounded :: Line -> Array Sounded
sounded l =
  let
    notes = lineOf l.seed l.bars
    total = lineSteps notes
    sumOf send at = foldl (+) 0 (mapWithIndex (\i w -> send w * waveAt l.seed i w at total) l.waves)
    step acc n =
      { at: acc.at + n.dur
      , out: snoc acc.out
          { degree: n.degree + floorDivI (7 * sumOf _.bend acc.at + 6350) 12700
          , dur: n.dur, rest: n.rest
          , vel: clamp 1 127 (baseVelocity + floorDivI (40 * sumOf _.dyn acc.at) 12700) }
      }
  in
    (foldl step { at: 0, out: [] } notes).out

-- | How much each note of a line carries, 0..100, by cell: a note on the bar
-- | line more than one on the third beat, that more than the other beats and
-- | those more than what falls between; a long note more than a short one;
-- | and a note that ends a phrase (a rest follows, or the line ends) more
-- | again. A rest carries nothing. The notes a ground pulls first.
weightsOf :: Array Sounded -> Array Int
weightsOf line = (foldl step { at: 0, i: 0, out: [] } line).out
  where
  step acc n =
    let
      place = acc.at `mod` 16
      beat
        | place == 0 = 50
        | place == 8 = 35
        | place `mod` 4 == 0 = 20
        | place `mod` 2 == 0 = 5
        | otherwise = 0
      long
        | n.dur >= 8 = 35
        | n.dur >= 4 = 25
        | n.dur >= 2 = 10
        | otherwise = 0
      ends = if maybe true _.rest (line !! (acc.i + 1)) then 15 else 0
    in
      { at: acc.at + n.dur, i: acc.i + 1, out: snoc acc.out (if n.rest then 0 else min 100 (beat + long + ends)) }

-- | Give the line a ground to play over (chords as pitch classes, bass
-- | first), or with none take it away and leave the scale alone.
setGround :: Array (Array Int) -> Odonus -> Odonus
setGround chords o = case o.line of
  Nothing -> o
  Just l ->
    let ground = filter (\c -> length c > 0) (map (map (\n -> ((n `mod` 12) + 12) `mod` 12)) chords)
    in (if length ground == 0 then o { chord = Nothing } else o) { line = Just l { ground = ground } }

-- | A ground on the wire and in a patch: each chord's pitch classes, then -1.
flatOf :: Array (Array Int) -> Array Int
flatOf = foldl (\acc c -> acc <> c <> [ -1 ]) []

chordsOf :: Array Int -> Array (Array Int)
chordsOf flat = (foldl step { now: [], out: [] } flat).out
  where
  step acc n = if n < 0 then { now: [], out: snoc acc.out acc.now } else acc { now = snoc acc.now n }

-- | How many bars each chord of the ground lasts.
setEvery :: Int -> Odonus -> Odonus
setEvery v o = o { line = o.line <#> _ { every = clamp 1 16 v } }

-- | How much of the line the ground pulls to its chords: none at 0, the
-- | notes that carry most first, every note at 100.
setPull :: Int -> Odonus -> Odonus
setPull v o = o { line = o.line <#> _ { pull = clamp 0 100 v } }

-- | The cells of a line, with the register they need: the octaves the line
-- | spans, and how many of them lie under the tonic.
lay :: Array Sounded -> { cells :: Array Cell, span :: Int, below :: Int }
lay line =
  let
    degrees = map _.degree (filter (not <<< _.rest) line)
    lo = fromMaybe 0 (minimum degrees)
    hi = fromMaybe 0 (maximum degrees)
    -- whole octaves under the tonic, so degree 0 stays the tonic
    below = if lo >= 0 then 0 else (6 - lo) / 7
    span = (hi + 7 * below) / 7 + 1
    slots = 7 * span
    knob d = min knobMax (((d + 7 * below) * (knobMax + 1) + slots - 1) / slots)
    cell n = { note: knob n.degree, skip: false, gate: not n.rest, glide: false, dur: n.dur, ratchet: 1, vel: n.vel }
  in
    { cells: map cell line, span, below }

-- | Put a line in the heads' hands: its notes as the cells, the register set
-- | so degree 0 is the tonic above middle C's octave start (C4 in C), and
-- | every head reading the whole line straight through on its notes' lengths,
-- | all starting together (`enter` staggers them).
load :: Array Note -> Odonus -> Odonus
load line = loadSounded (map (\n -> { degree: n.degree, dur: n.dur, rest: n.rest, vel: 100 }) line)

loadSounded :: Array Sounded -> Odonus -> Odonus
loadSounded line o =
  let
    laid = lay line
    n = length line
    reading h = h { clock = 1, patternIx = 0, len = n, offset = 0, pulses = 16, esteps = 16, rotation = 0, restart = false }
  in
    -- the clock at four to the beat (the top of Odonus's rates), so a head at
    -- ×1 moves in the line's own steps, sixteen to the bar
    enter [] (o { cells = laid.cells, span = laid.span, octaveShift = 1 - laid.below, clockIx = 8, heads = map reading o.heads })

-- | Turn one control of wave `i` (`param`: 0 shape, 1 cycles, 2 phase, 3 dyn,
-- | 4 bend) and lay the line again as it now comes out. The heads stay where
-- | they are, so it is heard on the notes still to come; the octave the page
-- | was set to stays too, whatever the bend does to the line's depth.
setWave :: Int -> Int -> Int -> Odonus -> Odonus
setWave i param v o = case o.line of
  Nothing -> o
  Just l ->
    let
      turn w = case param of
        0 -> w { shape = clamp 0 5 v }
        1 -> w { cycles = clamp 1 32 v }
        2 -> w { phase = clamp 0 15 v }
        3 -> w { dyn = clamp (-100) 100 v }
        _ -> w { bend = clamp (-100) 100 v }
      next = l { waves = fromMaybe l.waves (modifyAt i turn l.waves) }
      was = lay (sounded l)
      now = lay (sounded next)
    in
      o { line = Just next, cells = now.cells, span = now.span, octaveShift = o.octaveShift + was.below - now.below }

-- | Start the heads again, each after its own wait: head `i` comes in
-- | `delays !! i` ticks of its own clock from now (none given: at once), on
-- | the line's first note, or its last if it reads backward. From there they
-- | keep that distance for as long as their speeds agree: a canon.
enter :: Array Int -> Odonus -> Odonus
enter delays o = o { heads = mapWithIndex start o.heads }
  where
  start i h =
    let
      len = max 1 (min (max 16 (length o.cells)) h.len)
      -- stand one step before the first note sounded, in the head's direction
      before = case h.direction of
        1 -> 0
        2 -> 2 * len - 1
        _ -> len - 1
    in
      h { seqPos = before, cursor = 0, accumulator = 0, pendStep = 1, etick = 0
        , hold = max 0 (fromMaybe 0 (delays !! i)) + 1 }

-- | The line of a draw, as its notes: what `draw` lays into the cells, for a
-- | page to show with its lengths and rests as the melody has them.
lineOf :: Int -> Int -> Array Note
lineOf seed bars = flatten (generate bars (seedFrom seed)).melody

-- | Draw a new line and start the heads on it. Each keeps the entry it had,
-- | and the waves stay as they were set: the first draw spaces the heads a
-- | quarter of the line apart and gives the line a swell to its middle with
-- | a smaller one every four bars.
draw :: Int -> Int -> Odonus -> Odonus
draw seed bars0 o =
  let
    line = lineOf seed bars0
    bars = lineSteps line / barSteps
    entries = case o.line of
      Just l -> map (clamp 0 (2 * bars)) l.entries
      Nothing -> map (\i -> i * bars / 2) (range 0 3)
    waves = case o.line of
      Just l -> l.waves
      Nothing ->
        [ { shape: 0, cycles: 1, phase: 12, dyn: 40, bend: 0 }
        , { shape: 1, cycles: max 1 (bars / 4), phase: 12, dyn: 25, bend: 0 }
        , { shape: 0, cycles: 2, phase: 0, dyn: 0, bend: 0 } ]
    kept = case o.line of
      Just l -> { ground: l.ground, every: l.every, pull: l.pull }
      Nothing -> { ground: [], every: 2, pull: 50 }
    shaped = sounded { seed, bars, entries, waves, ground: [], every: 1, pull: 0, tick: 0, weights: [] }
    drawn = { seed, bars, entries, waves, ground: kept.ground, every: kept.every, pull: kept.pull, tick: 0, weights: weightsOf shaped }
  in
    restart ((loadSounded shaped o) { line = Just drawn })

-- | When head `h` comes in, in half bars after a restart (none past the
-- | line's own length). It takes effect at the next `restart`.
setEntry :: Int -> Int -> Odonus -> Odonus
setEntry h v o = o { line = o.line <#> \l -> l { entries = fromMaybe l.entries (updateAt h (clamp 0 (2 * l.bars) v) l.entries) } }

-- | Every head back to the start of the line, each coming in at its entry,
-- | and the ground back to its first chord.
-- | A wait is counted in the head's own ticks, so half a bar is eight of them
-- | times its speed (whole at every speed but the thirds, which round down).
restart :: Odonus -> Odonus
restart o = enter (mapWithIndex wait o.heads) (o { line = o.line <#> _ { tick = 0 } })
  where
  entries = fromMaybe [] (map _.entries o.line)
  wait i h =
    let r = fromMaybe { num: 1, den: 1 } (rateTable !! h.speedIx)
    in fromMaybe 0 (entries !! i) * (barSteps / 2) * r.num / r.den

-- | Melichthys as it opens: four heads forward at the line's own speed, the
-- | answer a fifth up, then an octave and a fourth below, a quarter of a
-- | sixteen-bar line apart.
fresh :: Odonus
fresh = draw 5 16 (defaultOdonus { heads = mapWithIndex voice defaultOdonus.heads })
  where
  voice i h = h { speedIx = unitRateIx, direction = 0, mute = false, transp = 0, spreadOct = 0, invert = false
                , spreadDeg = fromMaybe 0 ([ 0, 4, -7, -3 ] !! i) }

-- | Whether a head has yet to come in since the last restart: it still stands
-- | where `enter` put it, one step before its first note, having read none.
waiting :: Odonus -> Head -> Boolean
waiting o h =
  let
    len = max 1 (min (max 16 (length o.cells)) h.len)
    before = case h.direction of
      1 -> 0
      2 -> 2 * len - 1
      _ -> len - 1
  in
    len > 1 && h.cursor == 0 && h.seqPos == before && h.direction /= 1

-- | A chord of the ground as a held pad: its first pitch class as a bass
-- | note in the octave from C2, and every pitch class once in the octave
-- | from F3. The upper notes never leave that octave, so from chord to chord
-- | each moves as little as it can.
padOf :: Array Int -> Array Int
padOf chord = case chord !! 0 of
  Nothing -> []
  Just root -> [ 36 + root `mod` 12 ] <> sort (nub (map (\pc -> 53 + (pc - 5 + 120) `mod` 12) chord))
