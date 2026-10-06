-- | `Reef.Vetula.Chord` — the chord model, the generation families, and the
-- | spatial layout.
-- |
-- | A `ChordNode` is a concrete chord on the surface. The surface starts as the
-- | McMullen palette and GROWS: hover a chord and a generation family
-- | (`revoicings` / `inversions` / `suspensions` / `extensions`) spawns its
-- | children. `place` positions every node over an imaginary piano keyboard:
-- | x = the chord's ROOT on the keys, y = how far outside the chosen scale it
-- | is (in-scale chords hover just above the keys; outside chords float higher).
-- | Collision then beeswarms the chords sharing a key.
-- |
-- | Moved from Triggerfish (`Vetula.Harmony`) on 2026-10-02 so the rig can read Vetula's
-- | cards (docs/kb/plans/gpl-boundary-review.md, step 3); Triggerfish re-exports it.
module Reef.Vetula.Chord
  ( ChordNode
  , Kind(..)
  , Family(..)
  , mcmullenChords
  , borrowedChords
  , interchangeChords
  , diatonicTriads
  , diatonicSevenths
  , triadNode
  , latticeFamily
  , latticeChild
  , triadOn
  , suspendSet
  , generate
  , voicingCandidates
  , place
  , placeOutside
  , playNotes
  , bassMidi
  , octaveShift
  , noteName
  , scaleSet
  , keyX
  , keyboard
  , whiteKeyPcs
  , blackKeyPcs
  ) where

import Prelude

import Data.Array (elemIndex, filter, length, mapWithIndex, nub, sort, (!!), (:))
import Data.Foldable (any, elem, foldr, maximum, minimum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Harmonia.Anchor (Anchor(..))
import Harmonia.Chord (Chord(..), DegreeChord, Key, Mode, chordBass, chordRoot, mcmullenYellow, modeIntervals, realize)
import Harmonia.Voicing (Voicing(..), closeVoicing, cluster, drop2, drop2and4, enumerateVoicings, openTriad, quartal, spread, voicingMidi)

data Kind = Seed | Voiced | Inverted | Suspended | Extended | Borrowed

derive instance eqKind :: Eq Kind

-- | The note-set-changing growth moves. Re-voicing (rearranging the SAME notes)
-- | is retired from the cloud — that now lives in the ladder widget (Tab-cycle +
-- | favourites), keeping every bubble a distinct note set.
data Family = Invert | Suspend | Extend | Borrow

type ChordNode =
  { id :: Int
  , parentId :: Maybe Int  -- the chord this was generated from (Nothing for seeds)
  , root :: Int           -- pitch class — the key it sits over + extension anchor
  , bassPc :: Int         -- bass pitch class — for inversions + grounding
  -- The octave the bass sounds in. This used to be the constant 36 baked into
  -- `playNotes`, which meant a chord could not be moved in register at all:
  -- the uppers could be shifted and the bass stayed nailed to C2, so a
  -- whole-chord octave was not expressible and an inversion that turned all
  -- the way round left its bass three octaves below the notes it belonged to.
  -- 3 reproduces the old pitch exactly (pc + 12*3 = pc + 36).
  , bassOct :: Int
  , pcs :: Array Int      -- absolute pitch classes of the chord's content
  , voicing :: Array Int  -- uppers as ascending MIDI (no grounding bass)
  , kind :: Kind
  , label :: String
  , pinned :: Boolean
  , outside :: Int         -- non-scale tone count (set by `place`)
  , targetX :: Number      -- the root's key (set by `place`)
  , targetY :: Number      -- inside (near keys) ↔ outside (high) (set by `place`)
  , isCentre :: Boolean
  , anchor :: Anchor        -- Harmonia's scale reading: `Located` for seeds that
                            -- know their recipe, `Free` for pitch-space growths.
                            -- Carried into a Specimen on catch (grade + verbs).
  }

playNotes :: ChordNode -> Array Int
playNotes c = [ bassMidi c ] <> c.voicing

-- | Where the bass actually sounds. One place, so nothing re-derives it from
-- | `bassPc` and a remembered constant.
bassMidi :: ChordNode -> Int
bassMidi c = mod c.bassPc 12 + 12 * c.bassOct

-- | Move a whole chord in register — bass and uppers together, which is the
-- | point: shifting only the uppers spreads the chord, it does not transpose
-- | it.
-- |
-- | The transform itself is `Harmonia.Voicing.transposeOctaves`; what is here is
-- | the POLICY, which is rightly the app's: a shift that would carry the chord
-- | off the ladder is refused whole rather than applied by halves. Bounding the
-- | bass octave instead was the first attempt and it failed silently — a few
-- | inversions drove the bass into the clamp and the control then did nothing
-- | at all, moving the uppers by the same zero. What needs bounding is where the
-- | notes END UP.
octaveShift :: Int -> ChordNode -> ChordNode
octaveShift d c =
  let ns = playNotes c
      lo = fromMaybe 60 (minimum ns) + 12 * d
      hi = fromMaybe 60 (maximum ns) + 12 * d
  in if lo >= 12 && hi <= 120 then c { bassOct = c.bassOct + d, voicing = map (_ + 12 * d) c.voicing }
     else c

-- ---------------------------------------------------------------------------
-- Seeds — the McMullen palette
-- ---------------------------------------------------------------------------

mcmullenChords :: Key -> Array ChordNode
mcmullenChords key = mapWithIndex (fromDegree key) mcmullenYellow

fromDegree :: Key -> Int -> DegreeChord -> ChordNode
fromDegree key i dc =
  let Chord pcs = realize key dc
      r = chordRoot key dc
  in { id: i
     , parentId: Nothing
     , root: r
     , bassPc: chordBass key dc
     , bassOct: 3
     , pcs
     , voicing: voicingMidi (closeVoicing { centre: 4 } (Chord pcs))
     , kind: Seed
     , label: noteName r
     , pinned: false
     , outside: 0, targetX: 0.0, targetY: 0.0, isCentre: false
     , anchor: Located key dc   -- the seed knows its recipe → a full reading
     }

-- | The curated borrowed / chromatic-colour chords on the key's tonic — the
-- | "Borrowed" exterior populator. The same seven moves `borrows` offers from a
-- | chord, but rooted on the home tonic so the button drops a fixed signpost set
-- | (♭VI / ♭III / ♭VII / borrowed iv / Neapolitan / secondary-dom / tritone sub).
borrowedChords :: Key -> Array ChordNode
borrowedChords key = mapWithIndex mk moves
  where
  mk i m =
    let nr = mod (key.tonic + m.off) 12
        ps = sort (nub (map (\iv -> mod (nr + iv) 12) m.ivs))
    in { id: i
       , parentId: Nothing
       , root: nr
       , bassPc: nr
     , bassOct: 3
       , pcs: ps
       , voicing: voicingMidi (closeVoicing { centre: 4 } (Chord ps))
       , kind: Borrowed
       , label: noteName nr <> " " <> m.lbl
       , pinned: false
       , outside: 0, targetX: 0.0, targetY: 0.0, isCentre: false
       , anchor: Free   -- borrowed; no DegreeChord yet → Free for now (F1)
       }

-- | Modal interchange — the explicit, general "borrow from ‹mode›". Builds the
-- | diatonic triads of the PARALLEL mode (the same tonic, the chosen source mode)
-- | and keeps only those carrying at least one tone outside the CURRENT scale —
-- | i.e. the chords that are genuinely borrowed in this context. From C Ionian,
-- | `interchangeChords Aeolian` yields the familiar ♭III / iv / v / ♭VI / ♭VII.
interchangeChords :: Mode -> Key -> Array ChordNode
interchangeChords srcMode key =
  let cur = scaleSet key
      triads = diatonicTriads (key { mode = srcMode })
  in filter (\c -> any (\p -> not (elem (mod p 12) cur)) c.pcs) triads

-- | One diatonic triad per scale degree — the Lattice tab's seeds. Each is the
-- | tertian stack (degree, +2, +4) drawn from the scale itself, so it is major /
-- | minor / diminished as the mode dictates. Ids 0..(n-1), no parent.
diatonicTriads :: Key -> Array ChordNode
diatonicTriads key =
  let s = scaleSet key
      n = length s
  in mapWithIndex (\i _ -> triadAt s n i) s

-- | One diatonic seventh chord per scale degree: the triad with the scale's
-- | seventh above the root stacked on (degree, +2, +4, +6). Ids follow the
-- | triads' (n..2n-1), so the two sets can sit in one pool.
diatonicSevenths :: Key -> Array ChordNode
diatonicSevenths key =
  let s = scaleSet key
      n = length s
      at j = fromMaybe 0 (s !! mod j n)
      seventh i =
        let t = triadAt s n i
            pcs = nub (t.pcs <> [ at (i + 6) ])
        in t { id = n + i, pcs = pcs, voicing = voicingMidi (closeVoicing { centre: 4 } (Chord pcs)) }
  in mapWithIndex (\i _ -> seventh i) s

triadAt :: Array Int -> Int -> Int -> ChordNode
triadAt s n i =
  let at j = fromMaybe 0 (s !! mod j n)
      root = at i
      pcs = nub [ root, at (i + 2), at (i + 4) ]
  in { id: i
     , parentId: Nothing
     , root
     , bassPc: root
     , bassOct: 3
     , pcs
     , voicing: voicingMidi (closeVoicing { centre: 4 } (Chord pcs))
     , kind: Seed
     , label: noteName root
     , pinned: false
     , outside: 0, targetX: 0.0, targetY: 0.0, isCentre: false
     , anchor: Free   -- diatonic lattice triad; Located reading is F1b (see note)
     }

-- | A bare triad ChordNode rooted on a pitch class, voiced like the seeds (close
-- | voicing, centre octave 4). The Tonnetz lens picks triads straight off the
-- | tonal net with these — `anchor` is `Free` (a root+quality with no committed
-- | scale reading, like the palette/borrowed chords). The id is provisional.
triadNode :: Int -> Array Int -> String -> ChordNode
triadNode root rawPcs label =
  let pcs = sort (nub rawPcs)
  in { id: 0
     , parentId: Nothing
     , root
     , bassPc: root
     , bassOct: 3
     , pcs
     , voicing: voicingMidi (closeVoicing { centre: 4 } (Chord pcs))
     , kind: Seed
     , label
     , pinned: false
     , outside: 0, targetX: 0.0, targetY: 0.0, isCentre: false
     , anchor: Free
     }

-- | The whole tertian family of a seed triad: EVERY non-empty subset of the
-- | colour tones stacked in thirds above the root (3·5·7·9·11·13, drawn from the
-- | scale), with the root always kept. This is the unifying view — a "sus2" is a
-- | chord with the 9th and no 3rd; an "11 with no 3,5,9" is just another subset —
-- | so extensions, omissions and suspensions all fall out of one enumeration.
-- | Each result is tagged with `level` (highest tertian degree present: 0 = triad
-- | tones only, 1 = 7th … 4 = 13th) for the vertical rank, and `lean` (a small
-- | signed sideways nudge, see `familyMeta`) for the horizontal beeswarm.
latticeFamily :: Key -> ChordNode -> Array { chord :: ChordNode, level :: Int, lean :: Int }
latticeFamily key seed =
  let s = scaleSet key
      n = length s
      rootDeg = fromMaybe 0 (elemIndex seed.root s)
      -- the six colour tones above the root, k = 1 (3rd) .. 6 (13th)
      upper = map (\k -> { k, pc: fromMaybe 0 (s !! mod (rootDeg + 2 * k) n) }) [ 1, 2, 3, 4, 5, 6 ]
  in map build (filter (\sub -> length sub > 0) (powerset upper))
  where
  build sub =
    let pcs = sort (nub (seed.root : map _.pc sub))
        meta = familyMeta (map _.k sub)
    in { level: meta.level
       , lean: meta.lean
       , chord:
           { id: 0
           , parentId: Nothing
           , root: seed.root
           , bassPc: seed.root
     , bassOct: 3
           , pcs
           , voicing: voicingMidi (closeVoicing { centre: 4 } (Chord pcs))
           , kind: Extended
           , label: noteName seed.root
           , pinned: false
           , outside: 0, targetX: 0.0, targetY: 0.0, isCentre: false
           , anchor: Free   -- extended lattice subset; faithful DegreeChord is F1b
           }
       }

-- | All subsets of an array (2^n, includes the empty set).
powerset :: forall a. Array a -> Array (Array a)
powerset = foldr (\x acc -> acc <> map (\rest -> x : rest) acc) [ [] ]

-- | Layout metadata for a family member, given the colour-tone indices present
-- | (k = 1 the 3rd, 2 the 5th, 3 the 7th, 4 the 9th/2nd, 5 the 11th/4th, 6 the
-- | 13th/6th). `level` = the vertical rank (extension height: 0 triad … 4 the
-- | 13th). `lean` = a small signed sideways nudge for the beeswarm: a chord only
-- | leans when it SUBSTITUTES for the 3rd (the 3rd is absent) — the 9th/2nd pulls
-- | LEFT, the 11th/4th pulls RIGHT (so sus2 sits left of the column, sus4 right).
-- | A full tertian stack keeps its 3rd, so its 9th/11th are anchored extensions
-- | that do NOT lean — it stays on the column and stacks climb straight up.
familyMeta :: Array Int -> { level :: Int, lean :: Int }
familyMeta ks =
  let has k = elem k ks
      level = maybe 0 (\k -> k - 2) (maximum (filter (\k -> k >= 3) ks))
      lean = (if has 4 && not (has 1) then -1 else 0)
           + (if has 5 && not (has 1) then 1 else 0)
  in { level, lean }

-- | Build a family child for an explicit pitch-class set (the number / `e` / `s`
-- | populators), tagged with the same `level`/`count` the powerset enumeration
-- | would give it, so it lands in the right lattice stratum and dedupes against
-- | the `l`-exploded nodes of the same seed. The root is always kept.
latticeChild :: Key -> ChordNode -> Array Int -> { chord :: ChordNode, level :: Int, lean :: Int }
latticeChild key seed rawPcs =
  let s = scaleSet key
      n = length s
      rootDeg = fromMaybe 0 (elemIndex seed.root s)
      pcs = sort (nub (seed.root : rawPcs))
      -- the colour indices k = 1..6 (3rd … 13th) whose tone is present
      ks = filter (\k -> elem (fromMaybe (-1) (s !! mod (rootDeg + 2 * k) n)) pcs) [ 1, 2, 3, 4, 5, 6 ]
      meta = familyMeta ks
  in { level: meta.level
     , lean: meta.lean
     , chord:
         { id: 0
         , parentId: Nothing
         , root: seed.root
         , bassPc: seed.root
     , bassOct: 3
         , pcs
         , voicing: voicingMidi (closeVoicing { centre: 4 } (Chord pcs))
         , kind: Extended
         , label: noteName seed.root
         , pinned: false
         , outside: 0, targetX: 0.0, targetY: 0.0, isCentre: false
         , anchor: Free   -- extended lattice subset; faithful DegreeChord is F1b
         }
     }

-- | The diatonic triad rooted on a pitch class (root, +2, +4 scale degrees) — the
-- | bare-root shortcut for the friendly `e` populator.
triadOn :: Key -> Int -> Array Int
triadOn key root =
  let s = scaleSet key
      n = length s
      d = fromMaybe 0 (elemIndex root s)
      at j = fromMaybe root (s !! mod j n)
  in nub [ root, at (d + 2), at (d + 4) ]

-- | The scale-pure suspension subsets of a seed (sus2, sus4, root+fifth, root+
-- | third) — the friendly `s` populator. Each is a member of the seed's lattice
-- | family, so they dedupe against / wire into the rest of the interior web.
suspendSet :: Key -> ChordNode -> Array (Array Int)
suspendSet key seed =
  let s = scaleSet key
      n = length s
      d = fromMaybe 0 (elemIndex seed.root s)
      at j = fromMaybe seed.root (s !! mod j n)
  in [ [ seed.root, at (d + 1), at (d + 4) ]   -- sus2
     , [ seed.root, at (d + 3), at (d + 4) ]   -- sus4
     , [ seed.root, at (d + 4) ]               -- no3 (root + fifth)
     , [ seed.root, at (d + 2) ]               -- no5 (root + third)
     ]

-- ---------------------------------------------------------------------------
-- Generation — children of a chord (id 0; the caller re-ids)
-- ---------------------------------------------------------------------------

generate :: Family -> ChordNode -> Array ChordNode
generate = case _ of
  Invert -> inversions
  Suspend -> suspensions
  Extend -> extensions
  Borrow -> borrows

-- | The candidate upper-voicings to audition for a chord, current first — for
-- | Tab-cycling. `enumerateVoicings` gives the octave-permutations of the upper
-- | pitch classes ordered by smoothness (closest first); the named transforms
-- | (open / drop-2 / quartal / cluster / spread) add the wider rearrangements.
-- | All deduped, so cycling never repeats a voicing.
voicingCandidates :: ChordNode -> Array (Array Int)
voicingCandidates c =
  let cur = c.voicing
      upperPcs = nub (map (\m -> mod m 12) cur)
      enum = map (\(Tuple v _) -> voicingMidi v) (enumerateVoicings (Voicing cur) (Chord upperPcs))
      fam = map (\f -> voicingMidi (f (Voicing cur)))
              [ openTriad, drop2, drop2and4, quartal, cluster, spread { low: 3, high: 5 } ]
  in nub (cur : (enum <> fam))

inversions :: ChordNode -> Array ChordNode
inversions c =
  map (\pc -> c { bassPc = pc, kind = Inverted, pinned = false, isCentre = false
                , label = noteName c.root <> "/" <> noteName pc })
      (filter (\pc -> pc /= c.bassPc) (nub c.pcs))

suspensions :: ChordNode -> Array ChordNode
suspensions c =
  let r = c.root
      isThird pc = elem (mod (pc - r) 12) [ 3, 4 ]
      isFifth pc = elem (mod (pc - r) 12) [ 6, 7, 8 ]
      noThirds = filter (\pc -> not (isThird pc)) c.pcs
      noFifths = filter (\pc -> not (isFifth pc)) c.pcs
      variants =
        [ Tuple "sus2" (mod (r + 2) 12 : noThirds)
        , Tuple "sus4" (mod (r + 5) 12 : noThirds)
        , Tuple "no5" noFifths
        , Tuple "no3" noThirds
        ]
  in map (\(Tuple lbl ps) -> fromPcs c Suspended (noteName r <> " " <> lbl) ps) variants

extensions :: ChordNode -> Array ChordNode
extensions c =
  let r = c.root
      variants =
        [ Tuple "9" 2, Tuple "11" 5, Tuple "13" 9, Tuple "maj7" 11, Tuple "7" 10 ]
      addable = filter (\(Tuple _ iv) -> not (elem (mod (r + iv) 12) c.pcs)) variants
  in map (\(Tuple lbl iv) ->
            fromPcs c Extended (noteName r <> " add" <> lbl) (mod (r + iv) 12 : c.pcs))
         addable

-- | `b` — move to a NEW root: classic borrowed / chromatic colour chords. These
-- | land over their own (often out-of-scale) keys, so the keyboard's chromatic
-- | keys become the "borrowings" territory. The only family that changes root.
borrows :: ChordNode -> Array ChordNode
borrows c = map mk moves
  where
  mk m =
    let nr = mod (c.root + m.off) 12
        ps = map (\iv -> mod (nr + iv) 12) m.ivs
    in chordAt c nr ps (noteName nr <> " " <> m.lbl)

moves :: Array { off :: Int, ivs :: Array Int, lbl :: String }
moves =
  [ { off: 8, ivs: [ 0, 4, 7 ], lbl: "♭VI" }        -- chromatic mediant / borrowed
  , { off: 3, ivs: [ 0, 4, 7 ], lbl: "♭III" }
  , { off: 10, ivs: [ 0, 4, 7 ], lbl: "♭VII" }
  , { off: 5, ivs: [ 0, 3, 7 ], lbl: "iv" }          -- borrowed minor subdominant
  , { off: 1, ivs: [ 0, 4, 7 ], lbl: "♭II" }         -- Neapolitan
  , { off: 7, ivs: [ 0, 4, 7, 10 ], lbl: "V7" }      -- secondary dominant
  , { off: 6, ivs: [ 0, 4, 7, 10 ], lbl: "tritone" } -- tritone sub
  ]

-- | Build a chord rooted on a NEW pitch class (root + bass move together).
chordAt :: ChordNode -> Int -> Array Int -> String -> ChordNode
chordAt c root ps label =
  let sorted = sort (nub ps)
  in c { root = root
       , bassPc = root
       , pcs = sorted
       , voicing = voicingMidi (closeVoicing { centre: 4 } (Chord sorted))
       , kind = Borrowed
       , label = label
       , pinned = false
       , isCentre = false
       }

fromPcs :: ChordNode -> Kind -> String -> Array Int -> ChordNode
fromPcs c kind label ps =
  let sorted = sort (nub ps)
  in c { pcs = sorted
       , voicing = voicingMidi (closeVoicing { centre: 4 } (Chord sorted))
       , bassPc = c.root
       , kind = kind
       , label = label
       , pinned = false
       , isCentre = false
       }

-- ---------------------------------------------------------------------------
-- The imaginary keyboard (x axis) + scale membership (y axis)
-- ---------------------------------------------------------------------------

-- | Keyboard geometry, in the surface's centred coordinate space. One octave
-- | across the middle ~2/3; the keys live in a strip near the bottom.
keyboard :: { left :: Number, whiteW :: Number, top :: Number, bot :: Number }
keyboard = { left: -290.0, whiteW: 580.0 / 7.0, top: 232.0, bot: 286.0 }

whiteKeyPcs :: Array Int
whiteKeyPcs = [ 0, 2, 4, 5, 7, 9, 11 ]

blackKeyPcs :: Array Int
blackKeyPcs = [ 1, 3, 6, 8, 10 ]

-- | White-key slot (0..6) for naturals; black keys sit on the .5 boundaries.
slotOf :: Int -> Number
slotOf pc = case mod pc 12 of
  0 -> 0.0
  1 -> 0.5
  2 -> 1.0
  3 -> 1.5
  4 -> 2.0
  5 -> 3.0
  6 -> 3.5
  7 -> 4.0
  8 -> 4.5
  9 -> 5.0
  10 -> 5.5
  _ -> 6.0

-- | The x of a pitch class's key centre.
keyX :: Int -> Number
keyX pc = keyboard.left + (slotOf pc + 0.5) * keyboard.whiteW

place :: Key -> ChordNode -> ChordNode -> ChordNode
place key focus c =
  let out = outsideOf (scaleSet key) c.pcs
  in c { outside = out
       , isCentre = c.id == focus.id
       , targetX = keyX c.root
       , targetY = clampN (-240.0) 200.0 (200.0 - toNumber out * 90.0)
       }

-- | Place an exterior signpost (a dropped McMullen / borrowed chord, or an
-- | extension grown from one) over its own root key, on a distinct SHELF above
-- | the interior lattice — clearly its own stratum, sub-ranked by ring index (the
-- | out-of-scale tone count) so the further outside the scale, the higher it sits.
-- | An in-scale signpost (ring 0, e.g. a diatonic McMullen voicing) drops back
-- | down near the interior rather than onto the outside shelf.
placeOutside :: Key -> ChordNode -> ChordNode
placeOutside key c =
  let out = outsideOf (scaleSet key) c.pcs
  in c { outside = out
       , isCentre = false
       , targetX = keyX c.root
       , targetY =
           if out == 0 then 160.0
           else clampN (-265.0) (-95.0) (-110.0 - toNumber (out - 1) * 52.0)
       }

scaleSet :: Key -> Array Int
scaleSet key = map (\iv -> mod (key.tonic + iv) 12) (modeIntervals key.mode)

outsideOf :: Array Int -> Array Int -> Int
outsideOf scl pcs = length (filter (\p -> not (elem (mod p 12) scl)) (nub pcs))

clampN :: Number -> Number -> Number -> Number
clampN lo hi x = max lo (min hi x)

noteName :: Int -> String
noteName pc = fromMaybe "?" (names !! mod pc 12)
  where
  names = [ "C", "C♯", "D", "E♭", "E", "F", "F♯", "G", "A♭", "A", "B♭", "B" ]
