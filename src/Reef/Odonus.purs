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
  , currentChordPCs
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
  , nudgeHeadPulses
  , nudgeHeadEuclidSteps
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
  , spreadOctaves
  , spreadVoicings
  , nudgeOffsets
  , scaleOf
  , renderCell
  , knobMax
  , effectivePitchSet
  , setPitchSet
  , clearPitchSet
  , cellIndexMax
  , cellLabel
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
import Data.Int.Bits (and, shl, shr)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Reef.Scale (Scale, Distribution(..), mkScaleFromIvls, normaliseIvls, pitchClassesOf, quantiseToChordPCs, quantiseToScale, randomisableScales, recogniseScale, scaleTypes, spreadIvls)
import Reef.PitchSet (PitchSet(..), cardinality)
import Reef.PitchSet (realizeEqualShift) as PS

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
  , accumulator :: Int   -- phase carry in 1/8-step units (`stepDenom`), 0..stepDenom-1.
                         -- INTEGER, not a fractional float: it cannot drift across
                         -- runtimes and serialises identically in state snapshots (a
                         -- Number diverges in JSON formatting JS vs BEAM — cf the seed).
  , pendStep :: Int
  , etick :: Int    -- Euclidean phase: a free-running per-base-tick counter (0..esteps-1).
                    -- The Euclidean rhythm CLOCKS the advance — the melody steps to the
                    -- next cell only on a pulse of E(pulses, esteps), so no cell is
                    -- skipped; `etick` is what samples the rhythm each tick, distinct
                    -- from `seqPos` (which now only moves on a pulse).
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
  , feed :: Array (Array Int) -- the progression as explicit PC sets (0-11), the
                              -- Vetula feed; empty when the overlay is off
  , ix :: Int               -- current position in the progression
  , phase :: Int            -- model steps since the chord last advanced
  , period :: Int           -- steps per chord (its own clock, related to main)
  }

type Odonus =
  { cells :: Array Cell   -- length 16
  , heads :: Array Head
  , rootPc :: Int         -- scale root pitch-class 0..11 (legacy: drives scaleOf / UI / chord path)
  , scaleIvls :: Array Int -- in-scale semitone offsets from root (legacy: as above)
  , dist :: Distribution  -- how a cell integer becomes a pitch (legacy: chord-off path now uses pitchSet)
  , pitchSet :: Maybe PitchSet  -- explicit quantisation target (Vetula feed / pushed record); Nothing = derive from the scale fields
  , span :: Int           -- how many periods the cell indices span (replaces spread); bounds the cell index
  , octaveShift :: Int    -- global ± periods (coarse), applied in index space
  , degShift :: Int       -- global ± indices (fine scalar transpose)
  , gatePct :: Int        -- gated-note length as % of step spacing (>100 = legato)
  , chord :: ChordSeq     -- the chord-progression quantiser overlay
  }

-- | The active scale built from the root + interval mask.
scaleOf :: Odonus -> Scale
scaleOf o = mkScaleFromIvls o.rootPc o.scaleIvls

-- | Auto-recognised name of the current scale (for display).
scaleTypeName :: Odonus -> String
scaleTypeName o = recogniseScale o.scaleIvls

-- | Render a cell's stored knob to its final MIDI pitch for a given head — the
-- | two-stage pipeline of `Harmonia.Voice` / `docs/PLAN-odonus-pitch-pipeline.md`,
-- | built from reef's own primitives so it stays conformance-identical node↔BEAM.
-- |
-- |   q1 (EQUAL, over the SCALE): the raw knob `cell.note` (0..`knobMax`) maps by
-- |   equal spacing across `span` periods of the scale to a scale tone at real
-- |   register — the `home` note. This is the STABLE MELODIC SHAPE: the scale is
-- |   ALWAYS the index source, so a chord change never rewrites the melody.
-- |
-- |   + per-head CHROMATIC offset (`transp`, ±semitones), fired into the constraint.
-- |
-- |   q2 (NEAREST): snap to the active set — the current chord if the overlay is on
-- |   (a Vetula feed / picked progression), else the scale itself (a no-op on an
-- |   untransposed home, by the fixed-point law). The chord COLOURS the melody at
-- |   the end; because nearest is local, the register follows the melody, never
-- |   jumps to the chord's own octave.
-- |
-- |   + global octave shift (`octaveShift`·12 semitones), moving every voice
-- |   together — applied AFTER the snap, so it is a literal octave, not re-snapped.
-- |
-- | A spread of per-head chromatic offsets makes voices land on different chord
-- | tones (voice-spread, axis 3) with no special-casing. Scalar transpose
-- | (`degShift`) shifts the equal-mapped INDEX before realizing — the whole
-- | melody moves by that many scale-degrees, in index space, so it stays a
-- | scalar (in-set) transpose ahead of the chord snap. `degShift = 0` is a
-- | no-op, so the untransposed pipeline (and the conformance goldens) are
-- | byte-identical.
renderCell :: Odonus -> Head -> Cell -> Int
renderCell o hd c =
    let scaleSet = effectivePitchSet o
        home = PS.realizeEqualShift scaleSet o.span knobMax o.degShift c.note
        target = home + hd.transp
        snapped =
          if o.chord.on
            then quantiseToChordPCs (currentChordPCs o) target
            else quantiseToScale (scaleOf o) target
    in snapped + o.octaveShift * 12

-- | The raw NOTE-knob ceiling. Cells hold a value in `0..knobMax`, shown on the
-- | knob face; the label shows what it currently quantises to. Fixed (independent
-- | of the set's cardinality), so the knob range never collapses when a small
-- | chord fires — the register the knob sweeps is `span` periods of the scale.
knobMax :: Int
knobMax = 255

-- | The PitchSet the cells realize through: explicit if set (a Vetula feed or a
-- | pushed record), else derived from the scale fields (standalone). This is why
-- | the scale-authoring setters never touch pitchSet — they edit the scale and the
-- | derived set follows. (Display layers can call this to label a cell's pitch.)
effectivePitchSet :: Odonus -> PitchSet
effectivePitchSet o = case o.pitchSet of
  Just s -> s
  Nothing -> PitchSet { offsets: o.scaleIvls, root: 48 + o.rootPc, period: Just 12 }

-- | Install an explicit PitchSet (the Vetula / pushed-record path).
setPitchSet :: PitchSet -> Odonus -> Odonus
setPitchSet ps o = o { pitchSet = Just ps }

-- | Drop back to the scale-derived set (standalone authoring).
clearPitchSet :: Odonus -> Odonus
clearPitchSet o = o { pitchSet = Nothing }

-- | The top cell INDEX for this voice: `span` periods of the effective set, minus
-- | one. The UI uses it as the NOTE knob's range — cells are indices now, not
-- | chromatic values, so the old 36..84 range no longer applies.
cellIndexMax :: Odonus -> Int
cellIndexMax o = max 1 (o.span * cardinality (effectivePitchSet o)) - 1

-- | The MIDI pitch a cell's KNOB currently labels: equal-map the knob over the
-- | scale to its melodic home, colour it by the active chord if one is firing,
-- | then add the GLOBAL octave shift so the label tracks the register you hear.
-- | Re-colours live as the harmony moves (the Ciani "same pattern, recoloured"
-- | made visible). Still ABSENT the per-head chromatic offset (`transp`), which
-- | differs per head and so can't be shown on a single shared cell — the label is
-- | the shared melodic home, in the played octave. Display-only (not `renderCell`,
-- | so outside the conformance surface).
cellLabel :: Odonus -> Int -> Int
cellLabel o knob =
  let
    h = PS.realizeEqualShift (effectivePitchSet o) o.span knobMax o.degShift (clampI 0 knobMax knob)
    coloured = if o.chord.on then quantiseToChordPCs (currentChordPCs o) h else h
  in coloured + o.octaveShift * 12

-- | The pitch classes (0..11) of the chord at the feed's current position. The
-- | feed (a Vetula progression) is absolute and used verbatim; an empty feed
-- | means no colour (the overlay is off).
currentChordPCs :: Odonus -> Array Int
currentChordPCs o = fromMaybe [] (o.chord.feed !! o.chord.ix)

-- | How long the active progression (the Vetula feed) is.
chordSeqLen :: ChordSeq -> Int
chordSeqLen c = length c.feed

-- | Drive the quantiser from an external progression of explicit PC sets (the
-- | Vetula feed): adopt it, restart at its head, and switch the overlay on so
-- | it's audible immediately. An empty feed clears it and turns the overlay off.
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

-- | The phase accumulator counts 1/8-step units; `stepDenom` is that 8. Every
-- | speed is a whole multiple of 1/8 (0.125 is the finest), so the phase advances
-- | in exact integers — provably drift-free across runtimes, where a fractional
-- | float `accumulator` would only be EMPIRICALLY identical (and would serialise
-- | differently JS vs BEAM). `speedNumTable` is `speedTable * stepDenom`.
stepDenom :: Int
stepDenom = 8

speedNumTable :: Array Int
speedNumTable = [ 1, 2, 4, 6, 8, 12, 16, 24, 32, 48, 64 ]

speedNumOf :: Head -> Int
speedNumOf h = fromMaybe stepDenom (speedNumTable !! h.speedIx)

replicate16 :: forall a. a -> Array a
replicate16 = replicate 16

mkHead :: Int -> Int -> Int -> Boolean -> Int -> Head
mkHead speedIx direction transp mute patternIx =
  { cursor: 0, seqPos: 0, accumulator: 0, pendStep: 1, etick: 0
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

-- | Cells hold raw knob values now (0..`knobMax`), so the default spreads the 16
-- | steps evenly across the register (a rising line) rather than 0..15.
defaultCells :: Array Cell
defaultCells =
  mapWithIndex (\i _ -> { note: (i * knobMax) / 15, skip: false, gate: true, glide: false, dur: 1, ratchet: 1, vel: 100 })
    (replicate 16 unit)

-- | The chord overlay starts off, with an empty feed (a Vetula progression fills
-- | it). `period` is the feed's own advance clock (steps per chord).
defaultChord :: ChordSeq
defaultChord = { on: false, feed: [], ix: 0, phase: 0, period: 16 }

defaultOdonus :: Odonus
defaultOdonus =
  { cells: defaultCells, heads: defaultHeads
  , rootPc: 0, scaleIvls: [ 0, 2, 3, 5, 7, 8, 10 ], dist: Natural   -- C minor
  -- pitchSet Nothing → derive from the scale fields above (standalone C minor at
  -- middle C); a Vetula feed or pushed record installs an explicit set.
  , pitchSet: Nothing
  , span: 3
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

-- | Run `n` base ticks. The Euclidean rhythm CLOCKS the melodic advance: on each
-- | tick that lands on a pulse of E(pulses, esteps) the head steps to its next
-- | (skip-aware, direction-aware) cell; a rest tick holds. `et` advances every
-- | tick — it samples the rhythm — while `pos` only moves on a pulse, so no cell
-- | is skipped past silently.
advanceEuclid
  :: Array Int -> Array Cell -> Int -> Dir -> Int -> Int -> Int
  -> { pos :: Int, pend :: Int, et :: Int } -> { pos :: Int, pend :: Int, et :: Int }
advanceEuclid order cells len dir pulses es n st
  | n <= 0 = st
  | otherwise =
      let stepped = if euclidHit pulses es st.et
                      then stepSeq order cells len dir { pos: st.pos, pend: st.pend }
                      else { pos: st.pos, pend: st.pend }
      in advanceEuclid order cells len dir pulses es (n - 1)
           { pos: stepped.pos, pend: stepped.pend, et: (st.et + 1) `mod` es }

advanceHead :: Array Cell -> Head -> Head
advanceHead cells h =
  let
    order = orderOf h.patternIx
    len = clampI 1 16 h.len
    es = clampI 1 16 h.esteps
    -- Exact integer phase: accumulate 1/8-step units, whole base ticks are the
    -- integer quotient, the carry is the remainder. (Both ≥ 0 ⇒ unsigned div/mod.)
    newAcc = h.accumulator + speedNumOf h
    ticks = newAcc `div` stepDenom
    remain = newAcc `mod` stepDenom
    r = advanceEuclid order cells len (decodeDir h.direction) h.pulses es ticks
          { pos: h.seqPos, pend: h.pendStep, et: h.etick }
  in
    h { seqPos = r.pos
      , cursor = gridAt order (modPos (r.pos + h.offset) len)
      , accumulator = remain, pendStep = r.pend, etick = r.et }

-- | Did this head land on a Euclidean pulse during this model step? Recomputes the
-- | same `ticks` window `advanceHead` runs, over the PRE-step `etick`/`accumulator`
-- | — true iff any of those base ticks is a pulse (so the head advanced + should
-- | sound). Speed < 1 with no whole tick ⇒ no pulse ⇒ the head holds.
pulsedThisStep :: Head -> Boolean
pulsedThisStep h =
  let es = clampI 1 16 h.esteps
      ticks = (h.accumulator + speedNumOf h) `div` stepDenom
  in anyPulse h.pulses es h.etick ticks

anyPulse :: Int -> Int -> Int -> Int -> Boolean
anyPulse pulses es et n
  | n <= 0 = false
  | euclidHit pulses es et = true
  | otherwise = anyPulse pulses es ((et + 1) `mod` es) (n - 1)

step :: Odonus -> Odonus
step o = o { heads = map (advanceHead o.cells) o.heads }

cursorsOf :: Odonus -> Array Int
cursorsOf o = map _.cursor o.heads

-- | A note a head emits this tick: which head, the resulting pitch, whether the
-- | cell is marked glide (→ MIDI portamento / CV slew), the cell's note length
-- | and ratchet (retrigger) count, and the cell's base velocity.
type Fired =
  { headIdx :: Int, pitch :: Int, glide :: Boolean, dur :: Int, ratchet :: Int, vel :: Int }

-- | Advance one tick and report what fired: an unmuted head that landed on a
-- | Euclidean PULSE this tick sounds the cell it advanced onto. The Euclidean
-- | rhythm clocks the advance (see `advanceEuclid`), so every pulse both moves the
-- | melody one cell and sounds it — no cell is skipped past silently. A head whose
-- | tick was a rest (or whose speed < 1 gave no whole tick) holds and stays quiet.
stepEmit :: Odonus -> { odo :: Odonus, fired :: Array Fired }
stepEmit o =
  let
    oldHeads = o.heads
    o2 = step o
    firedFor idx hd =
      let pulsed = maybe false pulsedThisStep (oldHeads !! idx)
      in case o2.cells !! hd.cursor of
        Just c | pulsed && not hd.mute && c.gate && not c.skip ->
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

-- | Nudge a head's pulse count by a signed delta (clamped 0..16). RELATIVE so a
-- | burst of clicks accumulates even while the edit is buffered for the rig — an
-- | absolute `current±1` would re-read the same stale value on every click.
nudgeHeadPulses :: Int -> Int -> Odonus -> Odonus
nudgeHeadPulses h d = editHead h \hd -> hd { pulses = clampI 0 16 (hd.pulses + d) }

-- | Set a head's Euclidean step-count (STEPS) — the n in E(pulses, n). Independent
-- | of the loop length, so the Euclidean rhythm can phase against the pattern.
setHeadEuclidSteps :: Int -> Int -> Odonus -> Odonus
setHeadEuclidSteps h v = editHead h \hd -> hd { esteps = clampI 1 16 v }

-- | Nudge a head's Euclidean step-count by a signed delta (clamped 1..16).
-- | RELATIVE, like `nudgeHeadPulses`, so click bursts accumulate under buffering.
nudgeHeadEuclidSteps :: Int -> Int -> Odonus -> Odonus
nudgeHeadEuclidSteps h d = editHead h \hd -> hd { esteps = clampI 1 16 (hd.esteps + d) }

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
  aligned h = h { cursor = 0, seqPos = 0, accumulator = 0, pendStep = 1, mute = false }

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

-- | SPREAD (Marbles-style voicing morph): the knob walks a CURATED progression of
-- | four-voice register spreads rather than a linear fan. Early steps are strictly
-- | consonant — unison, then octaves, then fifths added, out to a symmetric ±octave
-- | (the "noon" of the knob); later steps add less-consonant but still diatonic
-- | colour (thirds, fourths, sixths). Values are per-head transposes in scale-degree
-- | index units (octave ≈ the scale's cardinality; ±7 reads as an octave diatonically).
-- | Hand-shaped and bounded, so the spread can't run away the way the linear fan did.
spreadVoicings :: Array (Array Int)
spreadVoicings =
  [ [  0,  0,  0,  0 ]   -- unison
  , [  0,  0,  0,  7 ]   -- one octave up
  , [ -7,  0,  0,  7 ]   -- octaves out (±octave)
  , [ -7,  0,  4,  7 ]   -- + a fifth
  , [ -7, -4,  4,  7 ]   -- fifths + octaves, fully consonant (knob "noon")
  , [ -7, -4,  4, 11 ]   -- stretch an upper voice to a twelfth
  , [ -7, -3,  2,  9 ]   -- diatonic colour: fourth below, third, sixth
  , [ -8, -3,  5, 10 ]   -- wider diatonic spread
  ]

spreadOctaves :: Int -> Odonus -> Odonus
spreadOctaves n o =
  let v = fromMaybe [ 0, 0, 0, 0 ] (spreadVoicings !! clampI 0 (length spreadVoicings - 1) n)
  in o { heads = mapWithIndex (\i hd -> hd { transp = clampI (-24) 24 (fromMaybe 0 (v !! i)) }) o.heads }

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
