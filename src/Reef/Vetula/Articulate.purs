-- | `Reef.Vetula.Articulate` — an ARTICULATOR turns the shared progression into one
-- | **alphabet** (an ordered, low→high note list) per chord. The Axis-B note-pattern
-- | then indexes that alphabet, and the block/arp/strum renderer sounds it, exactly as
-- | Tidal's `n "0 2 4" # scale "major"` indexes a scale: here harmonia supplies the
-- | alphabet (the voiced notes) and the pattern picks positions out of it. So the
-- | articulator decides *what the numbers mean* and *what the renderer voices*; the
-- | pattern decides *the order and rhythm* they sound in.
-- |
-- | The family:
-- |
-- |   * `ABlock`     — the chord's OWN notes (`sort chord.notes`). Index 0 is that
-- |                    chord's lowest sounding note; the count is the chord's size.
-- |   * `AVoiceLead` — harmonia's minimum-motion voice-leading carried through the loop,
-- |                    forced to a FIXED voice count N (= the largest chord's) so roles
-- |                    stay stable across the WHOLE progression: index 0 is the bass
-- |                    line, `-1` the soprano, the same connected line across every
-- |                    chord — `♪ 0` a bass part, `♪ -1` a melody, size-independent.
-- |                    Equal-size chords use harmonia's optimal `voiceLead` unchanged;
-- |                    smaller chords double a voice (nearest placement) to fill to N.
-- |   * `AEntering`  — the notes ENTERING each chord (in it, not the previous chord's
-- |                    voice-led alphabet), low→high. The first chord: all notes. This is
-- |                    strum's "new notes" as an addressable alphabet, so `♪ 0 1 2` arps
-- |                    the newcomers in and `♪ 0` picks just the lowest new note. (A
-- |                    chord all of whose notes are held has an empty alphabet — silence,
-- |                    which is musically honest: nothing new to articulate.)
-- |   * `AShell`     — just the lowest `shellSize` notes of the chord (`take n . sort`).
-- |                    A deliberately THIN pad: the bottom of the voicing (bass + a couple
-- |                    of low voices) instead of the full extended stack, so the colour
-- |                    tones are left UNSTATED — a chord-follow voice (Odonus) can then
-- |                    reveal them without the pad pre-empting the surprise. The quantiser
-- |                    feed still carries the full chord, so only what SOUNDS is thinned.
-- |
-- | `featureVoice` (solo one line) is already reachable as `AVoiceLead` + `♪ 0`/`♪ -1`,
-- | so it needs no constructor. `walkingBass`/`counterMelody` (time-varying *within* a
-- | chord — a pattern, not a static alphabet) and `pedal`/`thicken` (windowed / poly
-- | alphabets) want a richer seam and are a later step.
-- |
-- | Pure (`Prelude`/`Data.*`/harmonia) so it compiles to both the Triggerfish JS
-- | frontend and — when the rig catches up (#77) — the BEAM. `articulate` is a fold over
-- | the KNOWN finite loop, which is exactly what lets a voice-led alphabet see the whole
-- | progression (impossible in open-ended Tidal); the render seams
-- | (`Reef.Vetula.Perf.renderAlpha{Block,Clock}MidiAt`) stay byte-identical either way.
module Reef.Vetula.Articulate
  ( VArticulator(..)
  , articulate
  , articLabel
  , nextArtic
  ) where

import Prelude

import Data.Array (filter, length, mapWithIndex, nub, range, scanl, sort, take, uncons, updateAt, (!!))
import Data.Foldable (elem, foldl, maximum, minimumBy)
import Data.Maybe (Maybe(..), fromMaybe)
import Harmonia.Chord (Chord(..))
import Harmonia.Voicing (Voicing(..), closeVoicing, nearestNote, voiceLead, voicingMidi)
import Reef.Vetula.Perf (VChord)

-- | How the pattern's numbers are read and the renderer voiced: the chord's own notes
-- | (`ABlock`), a fixed-N voice-led line carried through the loop (`AVoiceLead`), or just
-- | the notes entering each chord (`AEntering`). Carried on the frontend voice (SOLO);
-- | reef only needs the alphabet it produces, so `VVoice`/the wire shape are untouched.
data VArticulator = ABlock | AVoiceLead | AEntering | AShell

derive instance eqVArticulator :: Eq VArticulator

-- | How many of the lowest notes `AShell` keeps — a low shell, fuller than a bare
-- | root (`♪ 0`) but far thinner than the whole voicing.
shellSize :: Int
shellSize = 3

-- | The alphabet (ordered low→high notes) each chord presents, one array per progression
-- | chord in order. See the module note for each articulator.
articulate :: VArticulator -> Array VChord -> Array (Array Int)
articulate ABlock chords = map (sort <<< _.notes) chords
articulate AVoiceLead chords = voiceLed chords
articulate AEntering chords =
  let vl = voiceLed chords
  in mapWithIndex
       (\i notes -> case vl !! (i - 1) of
          Just prev -> sort (filter (\n -> not (elem n prev)) notes)
          Nothing -> notes)   -- first chord (i = 0): everything enters
       vl
articulate AShell chords = map (take shellSize <<< sort <<< _.notes) chords

-- | Short UI label for the articulator button.
articLabel :: VArticulator -> String
articLabel = case _ of
  ABlock -> "block"
  AVoiceLead -> "voice-led"
  AEntering -> "entering"
  AShell -> "shell"

-- | Cycle to the next articulator (the UI's one-button vocabulary).
nextArtic :: VArticulator -> VArticulator
nextArtic = case _ of
  ABlock -> AVoiceLead
  AVoiceLead -> AEntering
  AEntering -> AShell
  AShell -> ABlock

-- ---------------------------------------------------------------------------
-- Voice-leading, forced to a fixed voice count
-- ---------------------------------------------------------------------------

-- | Voice-led voicings, one per chord, every one holding exactly N = the largest chord's
-- | voice count, so index k is the same connected line across the whole loop. For a
-- | progression of equal-size chords this is exactly harmonia's optimal `voiceLead` fold
-- | (N = the shared size; nothing is doubled), so `AVoiceLead` is unchanged there.
voiceLed :: Array VChord -> Array (Array Int)
voiceLed chords = case uncons chords of
  Nothing -> []
  Just { head, tail } ->
    let n = fromMaybe 0 (maximum (map (length <<< nub <<< _.pcs) chords))
    in if n <= 0 then map (const []) chords
       else
         let v0 = padTo n (voicingMidi (closeVoicing { centre: 4 } (Chord head.pcs)))
         in [ v0 ] <> scanl (\prev c -> leadFixed prev (nub c.pcs)) v0 tail

-- | Pad an ascending voicing up to `n` voices by doubling its lowest notes an octave
-- | down (a standard, non-lossy way to thicken — never drops a chord tone). Only the
-- | first chord needs this; the rest inherit N from the running voicing.
padTo :: Int -> Array Int -> Array Int
padTo n notes =
  let m = length notes
  in if m == 0 || m >= n then sort notes
     else sort (notes <> map (\i -> fromMaybe 0 (notes !! (i `mod` m)) - 12) (range 0 (n - m - 1)))

-- | Voice-lead the previous voicing (n notes) into `nextPcs` (m distinct pcs, m ≤ n),
-- | returning n notes. m == n uses harmonia's optimal `voiceLead`. m < n places each
-- | voice at its nearest next pc, then forces any pc left uncovered onto the cheapest
-- | voice — so all chord tones sound and the surplus voices double, all near the prior
-- | register (smooth motion, stable count).
leadFixed :: Array Int -> Array Int -> Array Int
leadFixed prev nextPcs =
  let n = length prev
      m = length nextPcs
  in if m == 0 then prev
     else if m == n then voicingMidi (voiceLead (Voicing prev) (Chord nextPcs))
     else sort (coverMissing prev (map (\p -> nearestNote p (nearestPc p nextPcs)) prev) nextPcs)

-- | The pc whose nearest placement to `target` moves it the least.
nearestPc :: Int -> Array Int -> Int
nearestPc target pcs =
  fromMaybe 0 (minimumBy (comparing (\pc -> absInt (nearestNote target pc - target))) pcs)

-- | Ensure every next-chord pc is voiced: any pc not present in `base` is forced onto the
-- | voice it can reach with least motion (from its original prev pitch).
coverMissing :: Array Int -> Array Int -> Array Int -> Array Int
coverMissing prev base nextPcs =
  let coveredPcs = nub (map (\x -> x `mod` 12) base)
      missing = filter (\pc -> not (elem pc coveredPcs)) nextPcs
  in foldl (forcePc prev) base missing

forcePc :: Array Int -> Array Int -> Int -> Array Int
forcePc prev notes pc =
  let cands = mapWithIndex
                (\i _ -> let target = fromMaybe 0 (prev !! i)
                         in { i, placed: nearestNote target pc, cost: absInt (nearestNote target pc - target) })
                notes
  in case minimumBy (comparing _.cost) cands of
       Just b -> fromMaybe notes (updateAt b.i b.placed notes)
       Nothing -> notes

absInt :: Int -> Int
absInt x = if x < 0 then -x else x
