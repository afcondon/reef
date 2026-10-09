-- | Triggerfish's Odonus model. A 4×4 grid of 16 cells (note + skip/gate/glide);
-- | each of four playheads walks the grid along its OWN access **pattern** (a
-- | René-style ordering of the 16 cells), at its own speed/direction, with a
-- | per-head transposition. Per-head pattern × speed × direction × interval is a
-- | richer Fugue Machine than either René (global pattern) or Fugue Machine
-- | (linear only). Patterns are drawn as small-multiple thumbnails by the grid.
module Reef.Odonus
  ( Cell
  , Head
  , HeadOf
  , HeadWire
  , headFromWire
  , headToWire
  , Odonus
  , OdonusOf
  , odonusFromWire
  , odonusToWire
  , currentChordPCs
  , followChord
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
  , setHeadClock
  , setHeadPulses
  , setHeadEuclidSteps
  , nudgeHeadPulses
  , nudgeHeadEuclidSteps
  , setCellRatchet
  , setCellVel
  , euclidHit
  , maxEsteps
  , harmonyPCs
  , toggleHeadMute
  , headMask
  , setHeadMask
  , cyclePattern
  , setHeadPattern
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
  , setHarmony
  , followHarmony
  , setScalePattern
  , setOutScale
  , followScale
  , setGridHarmony
  , followGridHarmony
  , releaseScale
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
  , recallGesture
  ) where

import Prelude

import Data.Array (catMaybes, concat, elem, mapMaybe, snoc, filter, findIndex, mapWithIndex, nub, null, replicate, modifyAt, length, sort, zipWith, (!!), (:))
import Data.Foldable (foldl)
import Data.Int.Bits (and, shl, shr)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Reef.Scale (Scale, Distribution(..), mkScaleFromIvls, normaliseIvls, pitchClassesOf, quantiseToChordPCs, quantiseToScale, randomisableScales, recogniseScale, scaleTypes, spreadIvls)
import Reef.PitchSet (PitchSet(..), cardinality)
import Reef.PitchSet (nearestIn, periodsIn, quantiseToVoicing, realizeEqualShift, voicing) as PS

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
-- | A head's fields, but for its clock: `Head` has them, and `HeadWire` (what
-- | is read from JSON) has them optional, so state saved before the duration
-- | clock still reads, on the step clock.
type HeadOf r =
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
  | r
  }

type Head = HeadOf
  ( clock :: Int    -- what moves the head on: 0 = its steps (the Euclidean clock, as
                    -- ever); 1 = its notes' lengths: it holds each cell for that cell's
                    -- `dur` steps, so the cells' durations are its rhythm (AC,
                    -- 2026-10-03), and a gate-off cell is a rest of its own length
  , hold :: Int     -- clock 1: base ticks left on the current cell (0 = move on now)
  )

type HeadWire = HeadOf (clock :: Maybe Int, hold :: Maybe Int)

headFromWire :: HeadWire -> Head
headFromWire h = h { clock = fromMaybe 0 h.clock, hold = fromMaybe 0 h.hold }

headToWire :: Head -> HeadWire
headToWire h = h { clock = Just h.clock, hold = Just h.hold }

-- | Odonus as read from JSON (heads as `HeadWire`), and back.
odonusFromWire :: OdonusOf HeadWire -> Odonus
odonusFromWire o = o { heads = map headFromWire o.heads }

odonusToWire :: Odonus -> OdonusOf HeadWire
odonusToWire o = o { heads = map headToWire o.heads }

type Odonus = OdonusOf Head

-- | Odonus with its heads of type `h`: `Head`, or `HeadWire` on the wire.
type OdonusOf h =
  { cells :: Array Cell   -- length 16
  , heads :: Array h
  , rootPc :: Int         -- scale root pitch-class 0..11 (legacy: drives scaleOf / UI / chord path)
  , scaleIvls :: Array Int -- in-scale semitone offsets from root (legacy: as above)
  , dist :: Distribution  -- how a cell integer becomes a pitch (legacy: chord-off path now uses pitchSet)
  , pitchSet :: Maybe PitchSet  -- explicit quantisation target (Vetula feed / pushed record); Nothing = derive from the scale fields
  , span :: Int           -- how many periods the cell indices span (replaces spread); bounds the cell index
  , octaveShift :: Int    -- global ± periods (coarse), applied in index space
  , degShift :: Int       -- global ± indices (fine scalar transpose)
  , gatePct :: Int        -- gated-note length as % of step spacing (>100 = legato)
  , chord :: Maybe (Array Int) -- the chord the output snaps to past the scale (pitch
                                -- classes, 0-11); filled each step from `harmony`
                                -- by followHarmony. Nothing = the scale alone
  , harmony :: Maybe String -- a Tidal note pattern (`"<c'maj7 a'min7>/2"`) the
                            -- overlay follows; sampled by the host, see followHarmony
  , scalePattern :: Maybe String -- a pattern of Tidal scale names (`"<dorian
                                 -- mixolydian>/4"`) scaleIvls follows; sampled
                                 -- by the host, see followScale
  , scaleHeld :: Maybe (Array Int) -- the authored scale, kept while a pattern
                                   -- plays it; `scale off` puts it back
  , outScale :: Maybe { pattern :: String, root :: Int }
      -- a pattern of Tidal scale names the OUTPUT snaps to (q2), on its own
      -- root (a pitch class), as `harmony` gives it a chord: offsets forced
      -- into a scale. Sampled by the host; one of harmony and outScale at a
      -- time. A Maybe, so a handed-over state from before it decodes as none
  , gridHarmony :: Maybe String
      -- a Tidal chord pattern the GRID takes its shape from (q1): sampled by
      -- the host each step as voiced (`Tidal.Harmony.voicingAt`) into the
      -- pitch set, so the cells play arpeggios of the chord, octaves kept
      -- (docs/kb/plans/harmony-routes-coherent.md). Nothing: the scale. A
      -- Maybe, so a handed-over state from before it decodes as none
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
        home = PS.realizeEqualShift scaleSet (PS.periodsIn scaleSet o.span) knobMax o.degShift c.note
        target = home + hd.transp
        snapped =
          case o.chord of
            Just notes -> PS.quantiseToVoicing notes target
            -- no output set: back onto the grid's own set (a no-op on its
            -- notes), so a grid shaped by a chord or Vetula's key keeps to it
            Nothing -> case o.pitchSet of
              Just ps -> PS.nearestIn ps target
              Nothing -> quantiseToScale (scaleOf o) target
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
cellIndexMax o =
  let ps = effectivePitchSet o
  in max 1 (PS.periodsIn ps o.span * cardinality ps) - 1

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
    ps = effectivePitchSet o
    h = PS.realizeEqualShift ps (PS.periodsIn ps o.span) knobMax o.degShift (clampI 0 knobMax knob)
    coloured = maybe h (\notes -> PS.quantiseToVoicing notes h) o.chord
  in coloured + o.octaveShift * 12

-- | The pitch classes (0..11) of the chord the output is snapping to; none
-- | while it follows the scale alone.
currentChordPCs :: Odonus -> Array Int
currentChordPCs o = sort (nub (map pc12 (fromMaybe [] o.chord)))
  where
  pc12 n = ((n `mod` 12) + 12) `mod` 12

-- | Snap to this chord past the scale (`Just pcs`), or to the scale alone.
-- | `followHarmony` calls it each step with what the harmony pattern gives.
followChord :: Maybe (Array Int) -> Odonus -> Odonus
followChord mpcs o = o { chord = mpcs }

-- | Set (or, with `Nothing`, clear) the Tidal pattern the chord overlay
-- | follows. Clearing turns the overlay off, back to the scale; setting leaves
-- | it for `followHarmony` to fill on the next step.
setHarmony :: Maybe String -> Odonus -> Odonus
setHarmony h o = case h of
  Just _ -> o { harmony = h, outScale = Nothing }
  Nothing -> followChord Nothing o { harmony = Nothing }

-- | **The output's scale** (the second quantise point, `odonus.out`): the
-- | output snaps, past each head's offset, to the pitch classes of a scale
-- | named by a pattern of Tidal's scale names, on its own root, rather than
-- | to a chord. The host samples it into the same chord the harmony pattern
-- | fills (`Reef.Engine.sampleInput`), so the snap itself is q2 unchanged.
-- | Setting it drops the harmony pattern, and the harmony pattern drops it:
-- | one source per input. `Nothing` returns the output to the scale.
setOutScale :: Maybe String -> Int -> Odonus -> Odonus
setOutScale p root o = case p of
  Just pattern -> o { outScale = Just { pattern, root: ((root `mod` 12) + 12) `mod` 12 }, harmony = Nothing }
  Nothing -> followChord Nothing o { outScale = Nothing }

-- | **Harmony as a Tidal pattern.** Reef cannot read Tidal (Littorina is GPL
-- | and reef is not), so the host that steps the machine supplies the reader:
-- | `sample` turns the pattern text into the pitch classes sounding at this
-- | step (`Tidal.Harmony.harmonyAt` in purerl-tidal and Triggerfish alike,
-- | which run the same engine on the BEAM and JS). The overlay follows that
-- | chord; an empty set (a rest, or text the host cannot read) falls back to
-- | the scale. No harmony set: unchanged. Called before each step.
followHarmony :: (String -> Array Int) -> Odonus -> Odonus
followHarmony sample o = case o.harmony of
  Nothing -> o
  Just txt -> case sample txt of
    [] -> followChord Nothing o
    pcs -> followChord (Just pcs) o

-- | **Scales by name, as a Tidal pattern.** `setScalePattern (Just "<dorian
-- | mixolydian>/4")` hands the scale to a pattern of Tidal's scale names: the
-- | host samples it each step (`Tidal.Scales.scaleSampler`, Littorina) and
-- | `followScale` writes the steps into `scaleIvls`, so the grid re-indexes
-- | as a hand-picked scale would. The root stays Odonus's own (`rootPc`), as
-- | Tidal adds a root to `scale` as a note. The scale authored before is kept
-- | (`scaleHeld`) and `Nothing` puts it back. Taking the pattern also drops an
-- | explicit pitch set (a Vetula feed), which would otherwise mask it: the
-- | last writer wins.
setScalePattern :: Maybe String -> Odonus -> Odonus
setScalePattern p o = case p of
  Just _ -> o { scalePattern = p, scaleHeld = Just (fromMaybe o.scaleIvls o.scaleHeld), pitchSet = Nothing, gridHarmony = Nothing }
  Nothing -> o { scalePattern = Nothing, scaleIvls = fromMaybe o.scaleIvls o.scaleHeld, scaleHeld = Nothing }

-- | **The grid's shape from a chord** (`odonus.grid <- harmony …` or `<- vetula
-- | N`). `setGridHarmony (Just pattern)` hands the grid to a Tidal chord
-- | pattern: the host samples it each step as voiced and `followGridHarmony`
-- | makes the pitch set the chord's (`PitchSet.voicing`), so the cells play
-- | arpeggios of it. It replaces a scale pattern; `Nothing` puts the grid back
-- | on the scale.
setGridHarmony :: Maybe String -> Odonus -> Odonus
setGridHarmony h o = case h of
  Just _ -> o { gridHarmony = h, scalePattern = Nothing, scaleIvls = fromMaybe o.scaleIvls o.scaleHeld, scaleHeld = Nothing }
  Nothing -> o { gridHarmony = Nothing, pitchSet = Nothing }

-- | The host's sample of the grid's chord pattern for this step, as voiced.
-- | Empty (a rest, or text the host cannot read) keeps the set as it is.
followGridHarmony :: (String -> Array Int) -> Odonus -> Odonus
followGridHarmony sample o = case o.gridHarmony of
  Nothing -> o
  Just txt -> case PS.voicing (sample txt) of
    Nothing -> o
    Just ps -> o { pitchSet = Just ps }

-- | The host's sample of the scale pattern for this step: the steps of the
-- | scale named there (0 first). Empty (a rest, or text the host cannot
-- | read) keeps the scale as it is. Called before each step.
followScale :: (String -> Array Int) -> Odonus -> Odonus
followScale sample o = case o.scalePattern of
  Nothing -> o
  Just txt -> case sample txt of
    [] -> o
    ivls -> o { scaleIvls = normaliseIvls ivls }

-- | A hand on the scale (cycle, toggle, spread, randomise) takes it from the
-- | pattern: the scale it is playing now becomes the authored one.
releaseScale :: Odonus -> Odonus
releaseScale o = o { scalePattern = Nothing, scaleHeld = Nothing }

-- | The pitch classes the current harmony admits: the chord if one is
-- | sounding, else the whole scale. Used to seed a melodic line.
harmonyPCs :: Odonus -> Array Int
harmonyPCs o = maybe (pitchClassesOf (scaleOf o)) (const (currentChordPCs o)) o.chord

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
  , speedIx, direction, transp, mute, patternIx, offset: 0, len: 16, pulses: 16, esteps: 16
  , clock: 0, hold: 0 }

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

defaultOdonus :: Odonus
defaultOdonus =
  { cells: defaultCells, heads: defaultHeads
  , rootPc: 0, scaleIvls: [ 0, 2, 3, 5, 7, 8, 10 ], dist: Natural   -- C minor
  -- pitchSet Nothing → derive from the scale fields above (standalone C minor at
  -- middle C); a Vetula feed or pushed record installs an explicit set.
  , pitchSet: Nothing
  , span: 3
  , octaveShift: 0, degShift: 0, gatePct: 90, chord: Nothing, harmony: Nothing
  , scalePattern: Nothing, scaleHeld: Nothing, outScale: Nothing, gridHarmony: Nothing }

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

-- | One whole tick of a head's own clock, inside a model step: where in the
-- | step it falls (`num / den` of it, so exact on both runtimes), whether it
-- | was a pulse (the head stepped onto a cell and should sound it), and the
-- | cell it is on afterwards.
type Tick = { num :: Int, den :: Int, pulsed :: Boolean, cursor :: Int }

-- | Advance one head through one model step. SPEED MULTIPLIES THE HEAD'S
-- | CLOCK (AC, 2026-10-09): a head at speed 2 has two ticks of its own in
-- | each model step, and each of them is a whole tick of everything the head
-- | does: its Euclidean counter, its duration hold, and a step of the melody
-- | that sounds. So a 5-in-16 rhythm at speed 2 cycles in half the time and
-- | every pulse in it plays, at its own place inside the step.
-- |
-- | The phase is counted in exact 1/8-step units. A tick falls where the
-- | count reaches a multiple of `stepDenom`, and the step covers counts
-- | `[acc, acc + speed)`, so a tick is at the START of its own span: a head at
-- | speed ½ plays on the first of its two steps, on the beat.
headTicks :: Array Cell -> Head -> { head :: Head, ticks :: Array Tick }
headTicks cells h0 =
  let
    a = h0.accumulator
    s = speedNumOf h0
    first = if a == 0 then 0 else stepDenom
    count = (a + s + stepDenom - 1) `div` stepDenom - (a + stepDenom - 1) `div` stepDenom
    go k h acc
      | k >= count = { head: h { accumulator = (a + s) `mod` stepDenom }, ticks: acc }
      | otherwise =
          let r = tickHead cells h
          in go (k + 1) r.head (snoc acc { num: first + k * stepDenom - a, den: s, pulsed: r.pulsed, cursor: r.head.cursor })
  in
    go 0 h0 []

-- | One tick of a head's own clock: on the duration clock, the hold counts
-- | down and the head steps when it runs out; on the Euclidean clock, the
-- | head steps on a pulse.
tickHead :: Array Cell -> Head -> { head :: Head, pulsed :: Boolean }
tickHead cells h =
  let
    order = orderOf h.patternIx
    len = clampI 1 16 h.len
    es = clampI 1 maxEsteps h.esteps
    dir = decodeDir h.direction
    cursorAt pos = gridAt order (modPos (pos + h.offset) len)
  in
    if h.clock == 1 then
      if h.hold > 1 then { head: h { hold = h.hold - 1 }, pulsed: false }
      else
        let
          st = stepSeq order cells len dir { pos: h.seqPos, pend: h.pendStep }
          cur = cursorAt st.pos
          d = maybe 1 (\c -> clampI 1 8 c.dur) (cells !! cur)
        in { head: h { seqPos = st.pos, cursor = cur, pendStep = st.pend, hold = d }, pulsed: true }
    else
      let
        hit = euclidHit h.pulses es h.etick
        st = if hit then stepSeq order cells len dir { pos: h.seqPos, pend: h.pendStep }
             else { pos: h.seqPos, pend: h.pendStep }
      in
        { head: h { seqPos = st.pos, cursor = cursorAt st.pos, pendStep = st.pend, etick = (h.etick + 1) `mod` es }
        , pulsed: hit }

advanceHead :: Array Cell -> Head -> Head
advanceHead cells h = (headTicks cells h).head

step :: Odonus -> Odonus
step o = o { heads = map (advanceHead o.cells) o.heads }

cursorsOf :: Odonus -> Array Int
cursorsOf o = map _.cursor o.heads

-- | A note a head emits this tick: which head, the resulting pitch, whether the
-- | cell is marked glide (→ MIDI portamento / CV slew), the cell's note length
-- | and ratchet (retrigger) count, and the cell's base velocity.
type Fired =
  { headIdx :: Int, pitch :: Int, glide :: Boolean, dur :: Int, ratchet :: Int, vel :: Int
  -- where in the model step it falls: `offNum / offDen` of the step (0 for a
  -- head at speed 1 or below; 0 and ½ for the two notes of a head at 2)
  , offNum :: Int, offDen :: Int }

-- | Advance one model step and report what fired: every tick of each head's
-- | own clock that was a PULSE sounds the cell the head stepped onto, at that
-- | tick's place in the step, unless the head is muted or the cell is gated
-- | off or skipped. So no cell a head steps onto is passed over silently,
-- | whatever its speed.
stepEmit :: Odonus -> { odo :: Odonus, fired :: Array Fired }
stepEmit o =
  let
    rs = map (headTicks o.cells) o.heads
    o2 = o { heads = map _.head rs }
    firedFor idx r = mapMaybe (fire idx r.head) r.ticks
    fire idx hd t = case o2.cells !! t.cursor of
      Just c | t.pulsed && not hd.mute && c.gate && not c.skip ->
        Just { headIdx: idx, pitch: renderCell o2 hd c, glide: c.glide
             , dur: c.dur, ratchet: c.ratchet, vel: c.vel
             , offNum: t.num, offDen: t.den }
      _ -> Nothing
  in
    { odo: o2, fired: concat (mapWithIndex firedFor rs) }

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

-- | What moves head `h` on: 0 its steps, 1 its notes' lengths. Changing it
-- | starts the new clock fresh (hold 0: move on at the next tick).
setHeadClock :: Int -> Int -> Odonus -> Odonus
setHeadClock h v = editHead h \hd -> if hd.clock == clampI 0 1 v then hd else hd { clock = clampI 0 1 v, hold = 0 }

-- | The Euclidean step-count ceiling — the largest n in E(k, n) a head may hold.
-- |
-- | Deliberately larger than the 16-cell grid: `esteps` is INDEPENDENT of `len`,
-- | so a long ring against a short cell loop is the point, not an accident —
-- | E(7, 24) over 16 cells decouples *which cell* from *when* and runs a long
-- | phrase before repeating. Was 16, raised 2026-08-07 (AC).
-- |
-- | Raising a clamp is behaviour-preserving for everything at or below the old
-- | value, so stored patches and the conformance goldens are unaffected. This is
-- | NOT the 16 of the cell grid (`cells`, `replicate16`, `len`, `offset`), which
-- | is structural and unrelated.
maxEsteps :: Int
maxEsteps = 64

-- | Set a head's Euclidean pulse count (DIV) — the k in E(k, esteps). 0 silences
-- | the voice; pulses ≥ esteps fires every step. The even (Bresenham) distribution.
-- | Bounded by `maxEsteps` rather than by the head's own n, since k > n is a legal
-- | way to say "every step".
setHeadPulses :: Int -> Int -> Odonus -> Odonus
setHeadPulses h v = editHead h \hd -> hd { pulses = clampI 0 maxEsteps v }

-- | Nudge a head's pulse count by a signed delta (clamped 0..`maxEsteps`).
-- | RELATIVE so a burst of clicks accumulates even while the edit is buffered for
-- | the rig — an absolute `current±1` would re-read the same stale value on every
-- | click.
nudgeHeadPulses :: Int -> Int -> Odonus -> Odonus
nudgeHeadPulses h d = editHead h \hd -> hd { pulses = clampI 0 maxEsteps (hd.pulses + d) }

-- | Set a head's Euclidean step-count (STEPS) — the n in E(pulses, n). Independent
-- | of the loop length, so the Euclidean rhythm can phase against the pattern.
-- | Pulses are clamped down to the new step-count so k never exceeds n: shrinking
-- | n to below k must lower k with it, or the display (which shows `min k n`) and
-- | the model drift apart and a later k− has to burn off the hidden surplus first.
setHeadEuclidSteps :: Int -> Int -> Odonus -> Odonus
setHeadEuclidSteps h v = editHead h \hd ->
  let n = clampI 1 maxEsteps v in hd { esteps = n, pulses = min hd.pulses n }

-- | Nudge a head's Euclidean step-count by a signed delta (clamped 1..`maxEsteps`).
-- | RELATIVE, like `nudgeHeadPulses`, so click bursts accumulate under buffering.
-- | Same k ≤ n clamp as `setHeadEuclidSteps` — n− carries k down with it.
nudgeHeadEuclidSteps :: Int -> Int -> Odonus -> Odonus
nudgeHeadEuclidSteps h d = editHead h \hd ->
  let n = clampI 1 maxEsteps (hd.esteps + d) in hd { esteps = n, pulses = min hd.pulses n }

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

-- | Set a head's access pattern by library index, clamped (resets its
-- | position, as `cyclePattern` does). Idempotent, where cycling is not.
setHeadPattern :: Int -> Int -> Odonus -> Odonus
setHeadPattern h ix = editHead h \hd ->
  let ni = max 0 (min (length patternLibrary - 1) ix)
  in hd { patternIx = ni, seqPos = 0, cursor = gridAt (orderOf ni) 0 }

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
    , accumulator = lh.accumulator, pendStep = lh.pendStep, hold = lh.hold }

-- | Recall only the *gesture* of `scene` over the `live` patch, KEEPING the live
-- | harmonic context — root, scale intervals, distribution, any explicit
-- | pitchSet (a Vetula feed), and the chord overlay all stay as they sound now.
-- | Only the authored gesture crosses over: the sixteen cells, the playhead
-- | configuration, the register transforms (octave / scalar-transpose / span)
-- | and the global gate. Because a cell's `note` is a scale DEGREE, not an
-- | absolute pitch, the recalled riff re-voices through whatever key or
-- | progression is live — the same lick in the current key. Playhead phase
-- | carries across exactly as `recallScene` does, so the swap flows.
recallGesture :: Odonus -> Odonus -> Odonus
recallGesture live scene =
  live
    { cells = scene.cells
    , heads = zipWith carry live.heads scene.heads
    , octaveShift = scene.octaveShift
    , degShift = scene.degShift
    , span = scene.span
    , gatePct = scene.gatePct
    }
  where
  carry lh sh = sh
    { cursor = lh.cursor, seqPos = lh.seqPos
    , accumulator = lh.accumulator, pendStep = lh.pendStep, hold = lh.hold }
