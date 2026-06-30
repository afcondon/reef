-- | Triggerfish's Odonus model. A 4×4 grid of 16 cells (note + skip/gate/glide);
-- | each of four playheads walks the grid along its OWN access **pattern** (a
-- | René-style ordering of the 16 cells), at its own speed/direction, with a
-- | per-head transposition. Per-head pattern × speed × direction × interval is a
-- | richer Fugue Machine than either René (global pattern) or Fugue Machine
-- | (linear only). Patterns are drawn as small-multiple thumbnails by the grid.
module Reef.Odonus
  ( Cell
  , Head
  , Odonus
  , ChordSeq
  , numChordTable
  , chordNameAt
  , currentChordPCs
  , setChordPicks
  , setChordFeed
  , followChord
  , tickChord
  , toggleChord
  , setChordPeriod
  , chordPeriodMin
  , chordPeriodMax
  , Pattern
  , patternLibrary
  , orderOf
  , defaultOdonus
  , replicate16
  , step
  , Fired
  , stepEmit
  , cursorsOf
  , toggleSkip
  , toggleGate
  , toggleGlide
  , setNote
  , setAllNotes
  , setNotes
  , setCellDur
  , speedTable
  , speedOf
  , setHeadSpeedIx
  , setHeadDir
  , setHeadTransp
  , setHeadOffset
  , setHeadLen
  , setHeadPulses
  , setHeadEuclidSteps
  , setCellRatchet
  , setCellVel
  , euclidHit
  , harmonyPCs
  , toggleHeadMute
  , headMask
  , setHeadMask
  , cyclePattern
  , unifyHeads
  , fanOffsets
  , staggerLengths
  , nudgeOffsets
  , scaleOf
  , renderCell
  , cycleRoot
  , cycleScaleType
  , numRandScales
  , setRandScale
  , toggleDistribution
  , scaleTypeName
  , setRoot
  , setOctaveShift
  , setDegShift
  , setGatePct
  , toggleScaleNote
  , setSpread
  , recallScene
  ) where

import Prelude

import Data.Array (catMaybes, elem, filter, findIndex, mapWithIndex, null, replicate, modifyAt, length, zipWith, (!!), (:))
import Data.Foldable (foldl)
import Data.Int (floor, toNumber)
import Data.Int.Bits (and, shl, shr)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Reef.Scale (Scale, Distribution(..), applyDistribution, mkScaleFromIvls, normaliseIvls, pitchClassesOf, quantiseToChordPCs, quantiseToScale, randomisableScales, recogniseScale, scaleTypes, shiftDegrees, spreadIvls)
import Harmonia.Chord (Chord(..), Mode(Ionian), mcmullenYellow, mcmullenYellowNames, realize)

type Cell =
  { note :: Int
  , skip :: Boolean
  , gate :: Boolean
  , glide :: Boolean
  , dur :: Int       -- note sustain in steps (1..8); multiplies the gate length
  , ratchet :: Int   -- retriggers within the gate window (1 = a single hit, 2..8 = a roll)
  , vel :: Int       -- this cell's base MIDI velocity (1..127); accent + humanise add to it
  }

-- | A René-style access pattern: a name + an ordering (permutation of 0..15,
-- | row-major y*4+x) giving the sequence in which cells are visited.
type Pattern = { name :: String, order :: Array Int }

patternLibrary :: Array Pattern
patternLibrary =
  [ { name: "Rows",       order: [ 0,1,2,3, 4,5,6,7, 8,9,10,11, 12,13,14,15 ] }
  , { name: "Serpentine", order: [ 0,1,2,3, 7,6,5,4, 8,9,10,11, 15,14,13,12 ] }
  , { name: "Columns",    order: [ 0,4,8,12, 1,5,9,13, 2,6,10,14, 3,7,11,15 ] }
  , { name: "Spiral",     order: [ 0,1,2,3, 7,11,15,14, 13,12,8,4, 5,6,10,9 ] }
  , { name: "Diagonal",   order: [ 0,1,4, 2,5,8, 3,6,9,12, 7,10,13, 11,14, 15 ] }
  ]

defaultOrder :: Array Int
defaultOrder = [ 0,1,2,3, 4,5,6,7, 8,9,10,11, 12,13,14,15 ]

orderOf :: Int -> Array Int
orderOf ix = maybe defaultOrder _.order (patternLibrary !! ix)

-- | A playhead. `seqPos` is the index into its pattern's order; `cursor` is the
-- | derived grid cell (order !! seqPos). `direction` 0=fwd 1=back 2=pendulum.
type Head =
  { cursor :: Int
  , seqPos :: Int
  , accumulator :: Number
  , pendStep :: Int
  , speedIx :: Int
  , direction :: Int
  , transp :: Int
  , mute :: Boolean
  , patternIx :: Int
  , offset :: Int   -- emit this many of THIS head's steps ahead (phase / canon)
  , len :: Int      -- loop length: reset after L steps (polymeter)
  , pulses :: Int   -- Euclidean trigger count: the k in E(k, esteps) (pulses ≥ esteps ⇒ every step)
  , esteps :: Int   -- Euclidean step-count: the n in E(pulses, n) — INDEPENDENT of len, so E(5,12) etc.
  }

-- | A chord-progression quantiser overlay. When `on`, the output is snapped a
-- | second time — past the scale — to the tones of the current chord, across
-- | octaves. The progression is four chords drawn from Joe McMullen's Plaits
-- | "Yellow" table (`picks` = indices into `mcmullenYellow`), realised against
-- | the current root in Ionian — so it transposes with the key. It advances on
-- | its own clock: `phase` counts model steps and rolls `ix` every `period`.
type ChordSeq =
  { on :: Boolean
  , picks :: Array Int      -- four indices into the McMullen Yellow table
  , feed :: Array (Array Int) -- external progression as explicit PC sets (0-11);
                              -- when non-empty it OVERRIDES picks (the Vetula feed)
  , ix :: Int               -- current position in the progression
  , phase :: Int            -- model steps since the chord last advanced
  , period :: Int           -- steps per chord (its own clock, related to main)
  }

type Odonus =
  { cells :: Array Cell   -- length 16
  , heads :: Array Head
  , rootPc :: Int         -- scale root pitch-class 0..11
  , scaleIvls :: Array Int -- in-scale semitone offsets from root (the mask)
  , dist :: Distribution  -- how a cell integer becomes a pitch
  , octaveShift :: Int    -- global ± octaves applied to the output
  , degShift :: Int       -- global scalar transpose, in scale degrees (I..IX)
  , gatePct :: Int        -- gated-note length as % of step spacing (>100 = legato)
  , chord :: ChordSeq     -- the chord-progression quantiser overlay
  }

-- | The active scale built from the root + interval mask.
scaleOf :: Odonus -> Scale
scaleOf o = mkScaleFromIvls o.rootPc o.scaleIvls

-- | Auto-recognised name of the current scale (for display).
scaleTypeName :: Odonus -> String
scaleTypeName o = recogniseScale o.scaleIvls

-- | Render a cell's stored integer to its final MIDI pitch for a given head.
-- | One snap to a SINGLE pitch source (the reframe): the chromatic knob-values
-- | (cell + per-head transpose) map to ONE pitch-set.
-- |
-- | When an external source drives — a chord progression or a followed Vetula
-- | voice (`chord.on`) — the chromatic value snaps DIRECTLY to the nearest tone
-- | of the current chord across octaves. No scale pre-snap and no scalar
-- | transpose (both are scale-degree notions, meaningless off-scale), so a Vetula
-- | voice's borrowed / out-of-scale tones and key changes are reached AS-IS.
-- |
-- | Otherwise the scale is the lens: the cell is read through the distribution,
-- | head-transposed and re-snapped to the scale, then shifted by whole scale
-- | degrees. Global octave applies in both cases.
renderCell :: Odonus -> Head -> Cell -> Int
renderCell o hd c =
  let octave = 12 * o.octaveShift
  in
    if o.chord.on then
      quantiseToChordPCs (currentChordPCs o) (c.note + hd.transp) + octave
    else
      let
        scale = scaleOf o
        base = applyDistribution o.dist scale c.note
        headed = quantiseToScale scale (base + hd.transp)
        degreed = shiftDegrees scale o.degShift headed
      in
        degreed + octave

-- | How many chords the table offers, and a chord's display name.
numChordTable :: Int
numChordTable = length mcmullenYellow

chordNameAt :: Int -> String
chordNameAt ix = fromMaybe "?" (mcmullenYellowNames !! ix)

-- | The pitch classes (0..11) of the chord at the progression's current
-- | position. A non-empty `feed` (e.g. a Vetula progression) wins — its PC sets
-- | are absolute and used verbatim; otherwise the picked McMullen chord is
-- | realised against the current root in Ionian (so it transposes with the key).
currentChordPCs :: Odonus -> Array Int
currentChordPCs o
  | not (null o.chord.feed) = fromMaybe [] (o.chord.feed !! o.chord.ix)
  | otherwise =
      case mcmullenYellow !! fromMaybe 0 (o.chord.picks !! o.chord.ix) of
        Just dc -> case realize { tonic: o.rootPc, mode: Ionian } dc of Chord pcs -> pcs
        Nothing -> []

-- | How long the active progression is (the feed when present, else the picks).
chordSeqLen :: ChordSeq -> Int
chordSeqLen c = if null c.feed then length c.picks else length c.feed

-- | Replace the four-chord progression (the chord randomiser's output). Clears
-- | any external feed, handing control back to the McMullen table.
setChordPicks :: Array Int -> Odonus -> Odonus
setChordPicks ps o = o { chord = o.chord { picks = ps, feed = [], ix = 0, phase = 0 } }

-- | Drive the quantiser from an external progression of explicit PC sets (the
-- | Vetula feed): adopt it, restart at its head, and switch the overlay on so
-- | it's audible immediately. An empty feed clears it back to the McMullen path.
setChordFeed :: Array (Array Int) -> Odonus -> Odonus
setChordFeed pcs o =
  o { chord = o.chord { feed = pcs, ix = 0, phase = 0, on = not (null pcs) || o.chord.on } }

-- | Follow a single live chord from a Vetula voice (the live-follow bridge). A
-- | `Just pcs` installs it as a one-element feed with the overlay ON, so the
-- | output snaps to that chord; the shell overwrites it every poll as the voice
-- | advances. A `Nothing` (no voice followed) clears the feed and turns the
-- | overlay OFF — back to plain scale quantisation.
followChord :: Maybe (Array Int) -> Odonus -> Odonus
followChord mpcs o = case mpcs of
  Just pcs -> o { chord = o.chord { feed = [ pcs ], ix = 0, phase = 0, on = true } }
  Nothing -> o { chord = o.chord { feed = [], ix = 0, phase = 0, on = false } }

-- | The pitch classes the current harmony admits: the live chord if the chord
-- | overlay is running, else the whole scale. Used to seed a melodic line.
harmonyPCs :: Odonus -> Array Int
harmonyPCs o = if o.chord.on then currentChordPCs o else pitchClassesOf (scaleOf o)

chordPeriodMin :: Int
chordPeriodMin = 1

chordPeriodMax :: Int
chordPeriodMax = 64

-- | Advance the chord clock one model step: roll to the next chord when this
-- | one has held for `period` steps.
tickChord :: Odonus -> Odonus
tickChord o =
  let
    per = clampI chordPeriodMin chordPeriodMax o.chord.period
    nCh = chordSeqLen o.chord
    ph = o.chord.phase + 1
  in
    if nCh <= 0 then o
    else if ph >= per then o { chord = o.chord { phase = 0, ix = (o.chord.ix + 1) `mod` nCh } }
    else o { chord = o.chord { phase = ph } }

-- | Enable/disable the chord overlay, restarting the progression from its head.
toggleChord :: Odonus -> Odonus
toggleChord o = o { chord = o.chord { on = not o.chord.on, ix = 0, phase = 0 } }

-- | Set the chord clock's period (steps per chord), clamped to the musical range.
setChordPeriod :: Int -> Odonus -> Odonus
setChordPeriod v o = o { chord = o.chord { period = clampI chordPeriodMin chordPeriodMax v } }

speedTable :: Array Number
speedTable = [ 0.125, 0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0, 6.0, 8.0 ]

speedOf :: Head -> Number
speedOf h = fromMaybe 1.0 (speedTable !! h.speedIx)

replicate16 :: forall a. a -> Array a
replicate16 = replicate 16

mkHead :: Int -> Int -> Int -> Boolean -> Int -> Head
mkHead speedIx direction transp mute patternIx =
  { cursor: 0, seqPos: 0, accumulator: 0.0, pendStep: 1
  , speedIx, direction, transp, mute, patternIx, offset: 0, len: 16, pulses: 16, esteps: 16 }

-- | Head I runs (Rows, 1.0×); II–IV start muted with distinct patterns + fugue
-- | offsets — unmute to build the canon. (Speed indices into the widened
-- | 1/8…8× table: 4=1.0, 2=0.5, 6=2.0, 3=0.75.)
defaultHeads :: Array Head
defaultHeads =
  [ mkHead 4 0 0 false 0       -- I:   Rows, 1.0× fwd
  , mkHead 2 0 7 true 1        -- II:  Serpentine, 0.5× +7
  , mkHead 6 1 (-12) true 3    -- III: Spiral, 2.0× rev −12
  , mkHead 3 2 3 true 2        -- IV:  Columns, 0.75× pend +3
  ]

defaultCells :: Array Cell
defaultCells =
  mapWithIndex (\i _ -> { note: 60 + i, skip: false, gate: true, glide: false, dur: 1, ratchet: 1, vel: 100 })
    (replicate 16 unit)

-- | The prototype progression: ii7 – V7 – Imaj7 – vi9, a ii–V–I–vi from the
-- | McMullen Yellow table (indices 15, 10, 12, 13).
defaultChord :: ChordSeq
defaultChord = { on: false, picks: [ 15, 10, 12, 13 ], feed: [], ix: 0, phase: 0, period: 16 }

defaultOdonus :: Odonus
defaultOdonus =
  { cells: defaultCells, heads: defaultHeads
  , rootPc: 0, scaleIvls: [ 0, 2, 3, 5, 7, 8, 10 ], dist: Natural   -- C minor
  , octaveShift: 0, degShift: 0, gatePct: 90, chord: defaultChord }

-- ---------------------------------------------------------------------------
-- traversal — walk the head's pattern ordering, skip-aware
-- ---------------------------------------------------------------------------

modPos :: Int -> Int -> Int
modPos a b = ((a `mod` b) + b) `mod` b

skipAt :: Array Cell -> Int -> Boolean
skipAt cells i = maybe false _.skip (cells !! i)

gridAt :: Array Int -> Int -> Int
gridAt order pos = fromMaybe 0 (order !! pos)

data Dir = Fwd | Back | Pend

decodeDir :: Int -> Dir
decodeDir n
  | n <= 0 = Fwd
  | n == 1 = Back
  | otherwise = Pend

-- | Next seq position in `dir`, within a loop of `len` steps, hopping
-- | positions whose grid cell is skipped.
nextSeq :: Array Int -> Array Cell -> Int -> Int -> Int -> Int
nextSeq order cells len start dir = go (modPos (start + dir) len) 0
  where
  go pos n
    | n >= len = start
    | not (skipAt cells (gridAt order pos)) = pos
    | otherwise = go (modPos (pos + dir) len) (n + 1)

stepSeq :: Array Int -> Array Cell -> Int -> Dir -> { pos :: Int, pend :: Int } -> { pos :: Int, pend :: Int }
stepSeq order cells len dir st = case dir of
  Fwd -> { pos: nextSeq order cells len st.pos 1, pend: st.pend }
  Back -> { pos: nextSeq order cells len st.pos (-1), pend: st.pend }
  Pend ->
    let
      ns
        | st.pos <= 0 && st.pend == (-1) = 1
        | st.pos >= len - 1 && st.pend == 1 = -1
        | otherwise = st.pend
    in
      { pos: nextSeq order cells len st.pos ns, pend: ns }

advanceSeqN :: Array Int -> Array Cell -> Int -> Dir -> Int -> { pos :: Int, pend :: Int } -> { pos :: Int, pend :: Int }
advanceSeqN order cells len dir n st
  | n <= 0 = st
  | otherwise = advanceSeqN order cells len dir (n - 1) (stepSeq order cells len dir st)

advanceHead :: Array Cell -> Head -> Head
advanceHead cells h =
  let
    order = orderOf h.patternIx
    len = clampI 1 16 h.len
    newAcc = h.accumulator + speedOf h
    steps = floor newAcc
    remain = newAcc - toNumber steps
    r = advanceSeqN order cells len (decodeDir h.direction) steps { pos: h.seqPos, pend: h.pendStep }
  in
    h { seqPos = r.pos
      , cursor = gridAt order (modPos (r.pos + h.offset) len)
      , accumulator = remain, pendStep = r.pend }

step :: Odonus -> Odonus
step o = o { heads = map (advanceHead o.cells) o.heads }

cursorsOf :: Odonus -> Array Int
cursorsOf o = map _.cursor o.heads

-- | A note a head emits this tick: which head, the resulting pitch, whether the
-- | cell is marked glide (→ MIDI portamento / CV slew), the cell's note length
-- | and ratchet (retrigger) count, and the cell's base velocity.
type Fired =
  { headIdx :: Int, pitch :: Int, glide :: Boolean, dur :: Int, ratchet :: Int, vel :: Int }

-- | Advance one tick and report what fired: an unmuted head that MOVED onto a
-- | gated, non-skipped cell emits its note. (A head that didn't advance this
-- | tick — speed < 1 — holds, it doesn't retrigger.)
stepEmit :: Odonus -> { odo :: Odonus, fired :: Array Fired }
stepEmit o =
  let
    oldCursors = map _.cursor o.heads
    o2 = step o
    firedFor idx hd =
      let moved = hd.cursor /= fromMaybe (-1) (oldCursors !! idx)
          -- This voice's own Euclidean clock: only steps that land on a pulse of
          -- E(pulses, esteps) trigger. esteps is independent of len, so the
          -- Euclidean period can phase against the loop. pulses ≥ esteps ⇒ every step.
          pulse = euclidHit hd.pulses (clampI 1 16 hd.esteps) hd.seqPos
      in case o2.cells !! hd.cursor of
        Just c | moved && pulse && not hd.mute && c.gate && not c.skip ->
          Just { headIdx: idx, pitch: renderCell o2 hd c, glide: c.glide
               , dur: c.dur, ratchet: c.ratchet, vel: c.vel }
        _ -> Nothing
  in
    { odo: o2, fired: catMaybes (mapWithIndex firedFor o2.heads) }

-- ---------------------------------------------------------------------------
-- editing
-- ---------------------------------------------------------------------------

editCell :: Int -> (Cell -> Cell) -> Odonus -> Odonus
editCell i f o = o { cells = fromMaybe o.cells (modifyAt i f o.cells) }

editHead :: Int -> (Head -> Head) -> Odonus -> Odonus
editHead h f o = o { heads = fromMaybe o.heads (modifyAt h f o.heads) }

clampI :: Int -> Int -> Int -> Int
clampI lo hi v = if v < lo then lo else if v > hi then hi else v

toggleSkip :: Int -> Odonus -> Odonus
toggleSkip i = editCell i \c -> c { skip = not c.skip }

toggleGate :: Int -> Odonus -> Odonus
toggleGate i = editCell i \c -> c { gate = not c.gate }

toggleGlide :: Int -> Odonus -> Odonus
toggleGlide i = editCell i \c -> c { glide = not c.glide }

setNote :: Int -> Int -> Odonus -> Odonus
setNote i v = editCell i \c -> c { note = v }

-- | Flatten every cell to one note value — a register reset to sculpt from
-- | (MIN → bass register, CENTER → melodic register).
setAllNotes :: Int -> Odonus -> Odonus
setAllNotes v o = o { cells = map (_ { note = v }) o.cells }

-- | Write a whole array of note values onto the cells positionally (the
-- | Marbles generator's output). Cells past the array length are untouched.
setNotes :: Array Int -> Odonus -> Odonus
setNotes ns o = o { cells = mapWithIndex (\i c -> maybe c (\n -> c { note = n }) (ns !! i)) o.cells }

-- | Per-cell note duration in steps (1..8). 1 = a single-step gate (the old
-- | behaviour); higher sustains the note across that many steps.
setCellDur :: Int -> Int -> Odonus -> Odonus
setCellDur i v = editCell i \c -> c { dur = clampI 1 8 v }

-- | Per-cell ratchet count (1..8). 1 = a single hit; higher subdivides the cell's
-- | gate window into that many evenly-spaced retriggers (a drum-roll / arp burst).
setCellRatchet :: Int -> Int -> Odonus -> Odonus
setCellRatchet i v = editCell i \c -> c { ratchet = clampI 1 8 v }

-- | Per-cell base velocity (1..127). The cell's own accent; the global beat
-- | accent and HUMANISE jitter are added on top at emit time.
setCellVel :: Int -> Int -> Odonus -> Odonus
setCellVel i v = editCell i \c -> c { vel = clampI 1 127 v }

setHeadSpeedIx :: Int -> Int -> Odonus -> Odonus
setHeadSpeedIx h v = editHead h \hd -> hd { speedIx = clampI 0 (length speedTable - 1) v }

setHeadDir :: Int -> Int -> Odonus -> Odonus
setHeadDir h v = editHead h \hd -> hd { direction = clampI 0 2 v }

setHeadTransp :: Int -> Int -> Odonus -> Odonus
setHeadTransp h v = editHead h \hd -> hd { transp = clampI (-24) 24 v }

setHeadOffset :: Int -> Int -> Odonus -> Odonus
setHeadOffset h v = editHead h \hd -> hd { offset = clampI 0 15 v }

setHeadLen :: Int -> Int -> Odonus -> Odonus
setHeadLen h v = editHead h \hd -> hd { len = clampI 1 16 v }

-- | Set a head's Euclidean pulse count (DIV) — the k in E(k, esteps). 0 silences
-- | the voice; pulses ≥ esteps fires every step. The even (Bresenham) distribution.
setHeadPulses :: Int -> Int -> Odonus -> Odonus
setHeadPulses h v = editHead h \hd -> hd { pulses = clampI 0 16 v }

-- | Set a head's Euclidean step-count (STEPS) — the n in E(pulses, n). Independent
-- | of the loop length, so the Euclidean rhythm can phase against the pattern.
setHeadEuclidSteps :: Int -> Int -> Odonus -> Odonus
setHeadEuclidSteps h v = editHead h \hd -> hd { esteps = clampI 1 16 v }

-- | Is step `i` a pulse of the even Euclidean rhythm E(pulses, steps)? 0 pulses
-- | is silent; pulses ≥ steps is every step; otherwise pulses spread evenly.
euclidHit :: Int -> Int -> Int -> Boolean
euclidHit pulses steps i
  | pulses <= 0 = false
  | pulses >= steps = true
  | otherwise = ((i `mod` steps) * pulses) `mod` steps < pulses

toggleHeadMute :: Int -> Odonus -> Odonus
toggleHeadMute h = editHead h \hd -> hd { mute = not hd.mute }

-- | The current head-activation combination as a bitmask: bit `h` set ⇒
-- | head `h` is UNMUTED (sounding). Four heads ⇒ 16 possible combinations.
headMask :: Odonus -> Int
headMask o = foldl addBit 0 (mapWithIndex (\i hd -> { i, on: not hd.mute }) o.heads)
  where
  addBit acc r = if r.on then acc + shl 1 r.i else acc

-- | Set every head's mute state from an activation bitmask in one move —
-- | the head-matrix's single-click transition between any two combinations.
setHeadMask :: Int -> Odonus -> Odonus
setHeadMask mask o = o { heads = mapWithIndex setOne o.heads }
  where
  setOne i hd = hd { mute = and (shr mask i) 1 == 0 }

-- | Advance a head to the next pattern in the library (resets its position).
cyclePattern :: Int -> Odonus -> Odonus
cyclePattern h = editHead h \hd ->
  let ni = (hd.patternIx + 1) `mod` length patternLibrary
  in hd { patternIx = ni, seqPos = 0, cursor = gridAt (orderOf ni) 0 }

-- ---------------------------------------------------------------------------
-- quantizer — the live pitch lens (scale + distribution)
-- ---------------------------------------------------------------------------

-- | Step the scale root up a semitone (wraps at the octave).
cycleRoot :: Int -> Odonus -> Odonus
cycleRoot dir o = o { rootPc = (o.rootPc + dir + 12) `mod` 12 }

-- | Step to the next/prev preset scale shape, setting the mask. If the current
-- | mask is a custom (unrecognised) set, stepping forward lands on the first
-- | preset.
cycleScaleType :: Int -> Odonus -> Odonus
cycleScaleType dir o =
  let
    cur = findIndex (\t -> normaliseIvls t.intervals == normaliseIvls o.scaleIvls) scaleTypes
    base = fromMaybe (-1) cur
    ni = ((base + dir) `mod` length scaleTypes + length scaleTypes) `mod` length scaleTypes
  in
    o { scaleIvls = maybe o.scaleIvls _.intervals (scaleTypes !! ni) }

-- | How many curated scales the KEY·SCALE randomiser can pick from.
numRandScales :: Int
numRandScales = length randomisableScales

-- | Install one of the curated musical scales by index (the randomiser's tonal
-- | move — a whole reasonable mode, never a random note cluster).
setRandScale :: Int -> Odonus -> Odonus
setRandScale ix o = o { scaleIvls = fromMaybe o.scaleIvls (randomisableScales !! ix) }

-- | Toggle a pitch class in/out of the scale (direct note choice). The root is
-- | always kept. `pc` is absolute 0..11; membership is by interval from root.
toggleScaleNote :: Int -> Odonus -> Odonus
toggleScaleNote pc o =
  let iv = (((pc - o.rootPc) `mod` 12) + 12) `mod` 12
  in
    if iv == 0 then o
    else o { scaleIvls = normaliseIvls
               (if elem iv o.scaleIvls then filter (_ /= iv) o.scaleIvls else iv : o.scaleIvls) }

-- | Marbles "spread": set the scale to the first `k` consonance-ordered notes
-- | (1 = root only, growing out through fifth/fourth/… to the full chromatic).
setSpread :: Int -> Odonus -> Odonus
setSpread k o = o { scaleIvls = spreadIvls k }

-- | Flip between chromatic-snap (Natural) and degree-index (Equal).
toggleDistribution :: Odonus -> Odonus
toggleDistribution o = o { dist = case o.dist of
  Natural -> Equal
  Equal -> Natural }

-- | Set the key directly (0..11) — the "change key" gesture.
setRoot :: Int -> Odonus -> Odonus
setRoot pc o = o { rootPc = ((pc `mod` 12) + 12) `mod` 12 }

-- | Global octave shift (clamped ±3).
setOctaveShift :: Int -> Odonus -> Odonus
setOctaveShift n o = o { octaveShift = clampI (-3) 3 n }

-- | Global scalar transpose within the key, in whole scale degrees (0..8 =
-- | the I..IX buttons).
setDegShift :: Int -> Odonus -> Odonus
setDegShift n o = o { degShift = clampI 0 8 n }

-- | Gated-note length as a percentage of step spacing (10..200; >100 overlaps
-- | into the next note = legato, which a portamento synth slides across).
setGatePct :: Int -> Odonus -> Odonus
setGatePct n o = o { gatePct = clampI 10 200 n }

-- | Make every head a copy of head I, phase-aligned and unmuted: four voices
-- | in exact unison. The starting point for Steve Reich phasing — from here,
-- | nudge one head's LEN (metric phasing, Clapping-Music style) or OFF (static
-- | canon) and listen to them drift against each other.
unifyHeads :: Odonus -> Odonus
unifyHeads o = case o.heads !! 0 of
  Just h0 -> o { heads = map (\_ -> aligned h0) o.heads }
  Nothing -> o
  where
  aligned h = h { cursor = 0, seqPos = 0, accumulator = 0.0, pendStep = 1, mute = false }

-- ---------------------------------------------------------------------------
-- Reichian phasing macros — drive all four heads' OFFSET / LEN at once
-- ---------------------------------------------------------------------------

-- | FAN: spread the heads into a canon, offsets 0, n, 2n, 3n (head-steps) —
-- | a static phase fan. n = 0 collapses to unison phase.
fanOffsets :: Int -> Odonus -> Odonus
fanOffsets n o = o { heads = mapWithIndex (\i hd -> hd { offset = clampI 0 15 (i * n) }) o.heads }

-- | STAGGER: ramp the loop lengths 16, 16−n, 16−2n, 16−3n — metric phasing
-- | (the heads fall out of step and slowly realign, Clapping-Music style).
staggerLengths :: Int -> Odonus -> Odonus
staggerLengths n o = o { heads = mapWithIndex (\i hd -> hd { len = clampI 1 16 (16 - i * n) }) o.heads }

-- | PHASE ±: rotate the whole canon — shift every head's offset by `d` steps.
nudgeOffsets :: Int -> Odonus -> Odonus
nudgeOffsets d o = o { heads = map (\hd -> hd { offset = clampI 0 15 (hd.offset + d) }) o.heads }

-- ---------------------------------------------------------------------------
-- scenes — load a saved setting over the live one, preserving playhead phase
-- ---------------------------------------------------------------------------

-- | Load `scene` over the currently-`live` patch, but carry each playhead's
-- | LIVE phase (cursor / seqPos / accumulator / pendStep) across the swap — so
-- | a scene change flows (new notes/scale/mutes/head-config take effect) rather
-- | than hard-resetting every cursor to 0. This is what makes sequencing whole
-- | settings sound like a continuing fugue with key changes and voices coming
-- | and going, not a stack of restarts.
recallScene :: Odonus -> Odonus -> Odonus
recallScene live scene =
  scene { heads = zipWith carry live.heads scene.heads }
  where
  carry lh sh = sh
    { cursor = lh.cursor, seqPos = lh.seqPos
    , accumulator = lh.accumulator, pendStep = lh.pendStep }
