-- | `Reef.Conspicillum.Harmonic` — how well a recorded chord hit voices a
-- | wanted chord.
-- |
-- | This is the join the whole instrument was proposed for: harmonia knows what
-- | chord is wanted, a Quadrat chord-hits set knows the **actual MIDI notes**
-- | that were struck to make each sample, and so a progression can be realised
-- | onto recorded material rather than transposed onto one sample. Nothing else
-- | in the stack can do this, because nothing else recorded what it sampled.
-- |
-- | ## The target carries its root explicitly, and must
-- |
-- | `Harmonia.Chord.realize` returns `Chord (nub (sort pcs))` — **sorted**, so
-- | the root is not element 0 and cannot be recovered from the set. Harmonia
-- | says so itself, in `chordRoot`'s docstring: it exists so callers can
-- | "LABEL it by its root rather than by the lowest pitch-class of a sorted
-- | voicing". Taking `pcs[0]` would silently weight a first-inversion chord as
-- | though its third were its root, and every scoring decision downstream would
-- | tilt. So a `Target` takes `root` and `bass` from `chordRoot`/`chordBass`.
-- |
-- | ## Why foreign notes have to be expensive
-- |
-- | Measured on the real sets (2026-09-22): of 136 chord hits recorded so far,
-- | 105 carry a clean note set, 24 carry none at all, and 7 are smears of ten
-- | or more pitch classes — a stuck-note artefact of the capture.
-- |
-- | **A twelve-pitch-class smear covers every chord perfectly.** On coverage
-- | alone it would win every selection, every time, and the instrument would
-- | reliably choose its worst material. So the foreign-note penalty is not a
-- | refinement, it is what makes the ranking usable at all — and it is the same
-- | parsimony `Harmonia.Recognise` describes when it normalises the fit by
-- | template weight but deliberately not the extras.
-- |
-- | ## On the weights
-- |
-- | They are expressed over bare intervals from the root rather than borrowed
-- | from `Harmonia.Recognise.intervalWeight`, which is keyed by `Quality` — and
-- | a realised `Chord` has no quality left in it to key on. The *intent* is
-- | harmonia's, and is worth stating because it is not arbitrary: the root and
-- | third are what identify a chord, the seventh colours it, and the perfect
-- | fifth is nearly free — it is the note real players drop and real estimators
-- | miss, so demanding it would reject good material.
module Reef.Conspicillum.Harmonic
  ( Target
  , Harmonic
  , NamedChord
  , namedChords
  , chordNamed
  , nameOfChord
  , pitchClasses
  , bassPitchClass
  , intervalWeight
  , foreignCost
  , fit
  ) where

import Prelude

import Data.Array (elem, find, foldl, length, nub, null, sort)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))

-- | A wanted chord: its realised pitch classes, plus the two things the set
-- | itself cannot tell you. See the module note.
type Target =
  { pcs :: Array Int
  , root :: Int
  , bass :: Int
  }

-- | A harmonic constraint on a cloud.
-- |
-- | `minFit` is a HARD cut and `strength` a soft lean, the same pairing the
-- | numeric axes use in `Reef.Conspicillum.Corpus`: "only grains that voice
-- | this chord" and "mostly grains that voice this chord" are different
-- | instruments and both are wanted.
type Harmonic =
  { target :: Target
  , minFit :: Number
  , strength :: Number
  }

-- | The pitch classes a sample sounds, from its recorded MIDI notes.
pitchClasses :: Array Int -> Array Int
pitchClasses notes = nub (sort (map (\n -> ((n `mod` 12) + 12) `mod` 12) notes))

-- | The pitch class of the sample's lowest recorded note.
bassPitchClass :: Array Int -> Maybe Int
bassPitchClass notes = case foldl lower Nothing notes of
  Nothing -> Nothing
  Just n -> Just (((n `mod` 12) + 12) `mod` 12)
  where
  lower acc n = case acc of
    Nothing -> Just n
    Just m -> Just (if n < m then n else m)

-- | How much a chord tone matters, by its interval above the root.
-- |
-- | Harmonia's intent over bare intervals — see the module note for why it is
-- | not `Harmonia.Recognise.intervalWeight` itself.
intervalWeight :: Int -> Number
intervalWeight iv = case ((iv `mod` 12) + 12) `mod` 12 of
  0 -> 1.0    -- root
  3 -> 1.0    -- minor third  ) the pair that says which chord this is
  4 -> 1.0    -- major third  )
  6 -> 0.9    -- ♭5 / ♯11: strongly identifying wherever it appears
  8 -> 0.8    -- ♯5
  10 -> 0.7   -- ♭7 ) colour rather than identity
  11 -> 0.7   -- ♮7 )
  7 -> 0.25   -- perfect fifth: nearly free, and the note everyone drops
  _ -> 0.5    -- 9ths, 11ths, 13ths

-- | What a note the chord cannot explain costs, by its distance from the
-- | nearest chord tone.
-- |
-- | A semitone is the expensive one: it is the interval that makes a voicing
-- | sound wrong rather than coloured. A whole tone reads as an added tension
-- | and is cheap. Anything further is somewhere in between — it is foreign,
-- | but it is not grinding against anything.
foreignCost :: Int -> Number
foreignCost d = case d of
  1 -> 1.0
  2 -> 0.35
  _ -> 0.6

-- | How well a sample's recorded notes voice the target, in [0,1].
-- |
-- | A sample with no recorded notes scores **zero rather than being skipped**:
-- | 24 of the 136 hits recorded so far carry none, and a scorer that treated
-- | "I don't know" as "no objection" would rank exactly those first.
fit :: Target -> Array Int -> Number
fit target notes
  | null notes = 0.0
  | null target.pcs = 0.0
  | otherwise =
      let
        samp = pitchClasses notes

        -- Coverage: the weighted share of the chord that is actually sounding.
        weightOf pc = intervalWeight (pc - target.root)
        wanted = foldl (\a pc -> a + weightOf pc) 0.0 target.pcs
        got = foldl (\a pc -> if pc `elem` samp then a + weightOf pc else a) 0.0 target.pcs
        coverage = if wanted <= 0.0 then 0.0 else got / wanted

        -- Foreign: what the chord cannot explain, costed by how much it grinds.
        foreign_ = foldl (\a pc -> if pc `elem` target.pcs then a else a + foreignCost (distanceTo target.pcs pc)) 0.0 samp

        -- Saturating, and normalised against the SIZE of the chord: a triad can
        -- absorb less strangeness than a six-note voicing before it stops being
        -- that chord. This is the term that stops a twelve-pitch-class smear —
        -- which covers everything perfectly — from winning every selection.
        -- SQUARED, and that is not decoration. Un-squared, a twelve-pitch-class
        -- smear scores about 0.32 against a D-minor target and beats an
        -- unrelated real chord (Gm(maj7), ~0.25) — the instrument would prefer
        -- its broken material to its good material whenever the good material
        -- happened to be the wrong chord. Squaring is one multiply, exact on
        -- both runtimes, and puts the smear below everything real.
        room = toNumber (length target.pcs)
        ratio = if foreign_ <= 0.0 then 1.0 else room / (room + foreign_)
        keep = ratio * ratio

        -- The bass is the strongest single cue that a voicing is the RIGHT
        -- inversion, so it earns a bonus rather than being folded into
        -- coverage, where a root in the bass and a root in the middle would
        -- score the same.
        bassBonus = case bassPitchClass notes of
          Nothing -> 0.0
          Just b
            | b == target.bass -> 0.10
            | b `elem` target.pcs -> 0.04
            | otherwise -> 0.0
      in
        clamp01 (coverage * keep + bassBonus)

-- | Semitone distance from `pc` to the nearest member of `pcs`, 0..6.
distanceTo :: Array Int -> Int -> Int
distanceTo pcs pc = foldl (\acc t -> min acc (ringDistance pc t)) 12 pcs

ringDistance :: Int -> Int -> Int
ringDistance a b =
  let d = ((a - b) `mod` 12 + 12) `mod` 12
  in if d > 6 then 12 - d else d

clamp01 :: Number -> Number
clamp01 x = if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x


-- ── chords by name ───────────────────────────────────────────────────────────

type NamedChord = { name :: String, target :: Target }

-- | The chords a progression is written in, by name. Named for what the two
-- | chord-hit corpora actually contain, which sit around E, B and F# and are
-- | full of minor-majors, augmented and half-diminished chords: every
-- | progression the presets use was scored against the material before it was
-- | written, and none goes silent. A chord not here cannot be written in a line
-- | yet; the harmonia join (chord symbols parsed rather than listed) is the way
-- | to lift that.
namedChords :: Array NamedChord
namedChords =
  [ chord "Em(maj7)" [ 4, 7, 11, 3 ] 4 4
  , chord "Bm" [ 11, 2, 6 ] 11 11
  , chord "F#m7b5" [ 6, 9, 0, 4 ] 6 6
  , chord "Em7b5" [ 4, 7, 10, 2 ] 4 4
  , chord "F#m" [ 6, 9, 1 ] 6 6
  , chord "Bdim" [ 11, 2, 5 ] 11 11
  , chord "Bm(maj7)" [ 11, 2, 6, 10 ] 11 11
  , chord "Daug" [ 2, 6, 10 ] 2 6
  , chord "E" [ 4, 8, 11 ] 4 4
  , chord "B7" [ 11, 3, 6, 9 ] 11 11
  , chord "C#dim" [ 1, 4, 7 ] 1 1
  , chord "F#dim" [ 6, 9, 0 ] 6 6
  , chord "F#m(maj7)" [ 6, 9, 1, 5 ] 6 6
  , chord "D" [ 2, 6, 9 ] 2 2
  , chord "D#m" [ 3, 6, 10 ] 3 3
  , chord "G" [ 7, 11, 2 ] 7 7
  ]
  where
  chord name pcs root bass = { name, target: { pcs, root, bass } }

chordNamed :: String -> Maybe Target
chordNamed name = map _.target (find (\c -> c.name == name) namedChords)

-- | The name a chord is written as, if it is one of `namedChords`.
nameOfChord :: Target -> Maybe String
nameOfChord t = map _.name (find (\c -> c.target.pcs == t.pcs && c.target.root == t.root && c.target.bass == t.bass) namedChords)
