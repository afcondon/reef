-- | `Reef.Vetula.Articulate` — an ARTICULATOR turns the shared progression into one
-- | **alphabet** (an ordered, low→high note list) per chord. The Axis-B note-pattern
-- | then indexes that alphabet, exactly as Tidal's `n "0 2 4" # scale "major"` indexes
-- | a scale: here harmonia supplies the alphabet (the voiced notes) and the pattern
-- | picks positions out of it. So the articulator decides *what the numbers mean*; the
-- | pattern decides *the order and rhythm* they sound in.
-- |
-- | Two articulators to start:
-- |
-- |   * `ABlock`     — the chord's OWN notes (`sort chord.notes`). Index 0 is that
-- |                    chord's lowest sounding note; the count is the chord's size, so
-- |                    `-1` (top) picks a different voice per chord when sizes differ.
-- |                    This is Slice 3½'s behaviour, unchanged.
-- |   * `AVoiceLead` — harmonia's minimum-motion voice-leading carried through the whole
-- |                    loop (`closeVoicing` the first chord, then `voiceLead` each next
-- |                    from the previous). When the chords share a voice count this gives
-- |                    STABLE voice roles: index 0 is the bass line, `-1` the soprano,
-- |                    the SAME connected line across every chord — so `♪ 0` is a bass
-- |                    part and `♪ -1` a melody, size-independent. (On a size change
-- |                    harmonia falls back to a fresh close voicing for that chord, so
-- |                    roles only stay stable across equal-size runs — the honest limit
-- |                    of the pairwise voice-leader; forcing a fixed N is a later step.)
-- |
-- | Pure (`Prelude`/`Data.*`/harmonia) so it compiles to both the Triggerfish JS
-- | frontend and — when the rig catches up (#77) — the BEAM. `articulate` is a fold
-- | over the KNOWN finite loop, which is exactly what lets a voice-led alphabet see the
-- | whole progression (impossible in open-ended Tidal); the note-pattern seam
-- | (`Reef.Vetula.Perf.renderAlphaClockMidiAt`) stays byte-identical either way.
module Reef.Vetula.Articulate
  ( VArticulator(..)
  , articulate
  , articLabel
  , nextArtic
  ) where

import Prelude

import Data.Array (cons, scanl, sort, uncons)
import Data.Maybe (Maybe(..))
import Harmonia.Chord (Chord(..))
import Harmonia.Voicing (closeVoicing, voiceLead, voicingMidi)
import Reef.Vetula.Perf (VChord)

-- | How the pattern's numbers are interpreted: as positions in the chord's own notes
-- | (`ABlock`) or as positions in a voice-led line carried through the loop
-- | (`AVoiceLead`). Carried on the frontend voice (SOLO); reef only needs the alphabet
-- | it produces, so `VVoice`/the wire shape are untouched.
data VArticulator = ABlock | AVoiceLead

derive instance eqVArticulator :: Eq VArticulator

-- | The alphabet (ordered low→high notes) each chord presents to the note-pattern, one
-- | array per progression chord in order. `ABlock` is the chord's own notes; `AVoiceLead`
-- | is the loop's voice-led voicings (see the module note for the stable-roles caveat).
articulate :: VArticulator -> Array VChord -> Array (Array Int)
articulate ABlock chords = map (sort <<< _.notes) chords
articulate AVoiceLead chords = case uncons chords of
  Nothing -> []
  Just { head, tail } ->
    let firstV = closeVoicing { centre: 4 } (Chord head.pcs)
        voicings = cons firstV
          (scanl (\prev c -> voiceLead prev (Chord c.pcs)) firstV tail)
    in map voicingMidi voicings

-- | Short UI label for the articulator button.
articLabel :: VArticulator -> String
articLabel = case _ of
  ABlock -> "block"
  AVoiceLead -> "voice-led"

-- | Cycle to the next articulator (the UI's one-button vocabulary).
nextArtic :: VArticulator -> VArticulator
nextArtic = case _ of
  ABlock -> AVoiceLead
  AVoiceLead -> ABlock
