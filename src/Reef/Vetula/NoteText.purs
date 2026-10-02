-- | `Reef.Vetula.NoteText` — render a progression (a path's chords, with their chosen
-- | voicings) as TidalCycles source, so a sequence built by ear in Vetula can be
-- | preserved and dropped into a live-coding cell. We emit the *voiced* notes
-- | (bass + uppers, via `playNotes`) as explicit note lists rather than chord
-- | names, so the exact voicing the user crafted in the Revoice tab survives the
-- | round trip — chord-name shorthand (`c'maj7`) would throw the voicing away.
-- |
-- | Moved from Triggerfish (`Vetula.Tidal`) on 2026-10-02 so the rig can read Vetula's
-- | cards (docs/kb/plans/gpl-boundary-review.md, step 3); Triggerfish re-exports it.
module Reef.Vetula.NoteText
  ( progressionSource
  , parseProgression
  , tidalNoteName
  ) where

import Prelude

import Data.Array (drop, filter, head, index, length, mapMaybe, mapWithIndex, sort)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, stripPrefix)
import Data.String.CodeUnits as SCU
import Data.String.Common (joinWith, toLower, trim)
import Data.Tuple (Tuple(..))
import Data.Foldable (find)
import Reef.Vetula.Chord (ChordNode, playNotes)

-- | A MIDI note as a Tidal-safe note name with octave (e.g. 60 → "c5", 61 →
-- | "cs5"). Tidal's note-name octave convention matches `midi `div` 12` (c5 = 0 =
-- | MIDI 60). Accidentals follow the display spelling but in `s`/`f` form, since
-- | Tidal's parser won't take "♯"/"♭".
tidalNoteName :: Int -> String
tidalNoteName midi = name <> show (midi `div` 12)
  where
  names = [ "c", "cs", "d", "ef", "e", "f", "fs", "g", "af", "a", "bf", "b" ]
  name = fromMaybe "?" (index names (mod midi 12))

-- | A chord as a Tidal stack: the voiced notes, ascending, comma-separated in
-- | brackets (Tidal mini-notation for simultaneous notes).
chordBracket :: ChordNode -> String
chordBracket c = "[" <> joinWith "," (map tidalNoteName (sort (playNotes c))) <> "]"

-- | The whole progression as a Tidal source block: a header comment naming the
-- | key and the chords, then a `note "<…>"` pattern (one chord per cycle — the
-- | natural reading of a progression), with the all-in-one-cycle form offered as
-- | a commented alternative.
progressionSource :: String -> Array ChordNode -> String
progressionSource keyLabel steps =
  joinWith "\n"
    [ "-- vetula progression · " <> show (length steps) <> " chords · " <> keyLabel
    , "-- " <> joinWith "   " (mapWithIndex (\i c -> show (i + 1) <> " " <> c.label) steps)
    , "note \"<" <> joinWith " " brackets <> ">\""
    , "-- all in one cycle:"
    , "-- note \"" <> joinWith " " brackets <> "\""
    ]
  where
  brackets = map chordBracket steps

-- ---------------------------------------------------------------------------
-- Round trip — parse a Tidal `note "<…>"` block back into note lists, so a
-- saved progression can be pasted in and its voicings worked on again.
-- ---------------------------------------------------------------------------

-- | Pull the chord note-lists out of a pasted Tidal source: drop comment lines
-- | (so the commented all-in-one-cycle alt is ignored), then extract every
-- | `[a,b,c]` group from the remaining text and parse each note token to MIDI.
-- | Lenient — unparseable tokens drop out, empty groups are filtered — so a
-- | half-typed paste never throws.
parseProgression :: String -> Array (Array Int)
parseProgression text =
  let body = joinWith " " (filter (\l -> stripPrefix (Pattern "--") (trim l) == Nothing) (split (Pattern "\n") text))
      groups = mapMaybe (\piece -> head (split (Pattern "]") piece)) (drop 1 (split (Pattern "[") body))
  in filter (\g -> length g > 0) (map parseGroup groups)
  where
  parseGroup g = mapMaybe noteToMidi (split (Pattern ",") g)

-- | A single note token → MIDI. Accepts Tidal-safe note names (`cs5`, `ef4`,
-- | `c5`) — the inverse of `tidalNoteName` — and also a bare note value (`0` =
-- | middle C = MIDI 60), so hand-written `note "0,4,7"` round-trips too.
noteToMidi :: String -> Maybe Int
noteToMidi raw =
  let t = toLower (trim raw)
  in case Int.fromString t of
       Just n -> Just (n + 60)
       Nothing -> nameToMidi t

nameToMidi :: String -> Maybe Int
nameToMidi t = do
  base <- letterPc (SCU.take 1 t)
  let rest = SCU.drop 1 t
      Tuple acc octStr = case SCU.take 1 rest of
        "s" -> Tuple 1 (SCU.drop 1 rest)
        "f" -> Tuple (-1) (SCU.drop 1 rest)
        _ -> Tuple 0 rest
  oct <- Int.fromString octStr
  pure (oct * 12 + base + acc)

letterPc :: String -> Maybe Int
letterPc l = map (\(Tuple _ pc) -> pc)
  (find (\(Tuple n _) -> n == l)
    [ Tuple "c" 0, Tuple "d" 2, Tuple "e" 4, Tuple "f" 5, Tuple "g" 7, Tuple "a" 9, Tuple "b" 11 ])
