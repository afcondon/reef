-- | **Vetula's harmony, written as Tidal.** Odonus quantises to a Tidal note
-- | pattern (`Reef.Odonus.followHarmony`); this module writes the pattern that
-- | says what Vetula is conducting, so "Vetula sets Odonus's harmony" is the
-- | same act as a user typing `odonus $ harmony "..."`.
-- |
-- | Reef only writes the text. Reading it is Littorina's job, in the hosts, so
-- | nothing here imports Tidal. The text must sample, step for step, to the
-- | chord Vetula's own clock gives (`Reef.Vetula.Perf.cursorAtClock`); Triggerfish
-- | checks that against Littorina in its tests.
-- |
-- | Chords are written as stacks of pitch-class names (`[c,e,g]`), since only
-- | pitch classes reach the quantiser, with Tidal's names (`cs`, `fs`, ...).
-- | Time follows Vetula's pulse: 16 pulses (16th notes) to the cycle, four beats,
-- | the same Link-absolute grid Odonus samples on.
module Reef.Vetula.Harmony
  ( chordText
  , clockHarmony
  , seqHarmony
  , Shape(..)
  ) where

import Prelude

import Data.Array (all, catMaybes, drop, foldl, head, last, length, null, nub, range, snoc, sort, takeWhile, uncons, unsnoc, (!!))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as String
import Data.String.CodeUnits as CU
import Reef.Vetula.Perf (PerfClock, segAtClock)

-- | One chord as a Tidal stack of pitch-class names: `[c,e,g]`; one note bare,
-- | none a rest.
chordText :: Array Int -> String
chordText notes = case sort (nub (map pc notes)) of
  [] -> "~"
  [ p ] -> name p
  ps -> "[" <> String.joinWith "," (map name ps) <> "]"
  where
  pc n = ((n `mod` 12) + 12) `mod` 12
  name p = fromMaybe "c" (names !! p)
  names = [ "c", "cs", "d", "ds", "e", "f", "fs", "g", "gs", "a", "as", "b" ]

-- | A voice's clock as a pattern: chord `i` wherever the read-head sits on it at
-- | pulse `p` (`segAtClock clock phase p`). In a rest between segments the
-- | head keeps the chord it last had, as the conductor does, so a rest repeats
-- | the chord before it, round the loop. One loop is `loopLen / 16` cycles,
-- | aligned to cycle 0 as the pulses are. `Nothing` for an empty clock.
clockHarmony :: Array (Array Int) -> PerfClock -> Int -> Maybe String
clockHarmony chords clock phase
  | clock.loopLen <= 0 || null clock.segs = Nothing
  | otherwise =
      let
        at p = _.ix <$> segAtClock clock phase p
        raw = map at (range 0 (clock.loopLen - 1))
        -- the chord in force at the loop's end carries round into its start
        carry = last (catMaybes raw)
        held = (foldl (\acc m -> { cur: if isJust m then m else acc.cur, out: snoc acc.out (if isJust m then m else acc.cur) }) { cur: carry, out: [] } raw).out
        runs = foldl run [] held
        run acc ix = case unsnoc acc of
          Just { init, last: r } | r.ix == ix -> snoc init r { len = r.len + 1 }
          _ -> snoc acc { ix, len: 1 }
        word r = chordText (fromMaybe [] (r.ix >>= (chords !! _))) <> weight r.len
        weight n = if n == 1 then "" else "@" <> show n
        body = "[" <> String.joinWith " " (map word runs) <> "]"
      in
        Just (body <> slowBy clock.loopLen)
  where
  slowBy pulses = case Int.rem pulses 16 of
    0 | pulses == 16 -> ""
    0 -> "/" <> show (pulses / 16)
    _ -> "/" <> decimal pulses
  -- pulses / 16 written exactly: a sixteenth is 0.0625, four decimal places
  decimal pulses =
    let whole = pulses / 16
        frac = (Int.rem pulses 16) * 625
        digits = String.joinWith "" (map show (padTo4 frac))
    in show whole <> "." <> trimZeros digits
  padTo4 n = map (\d -> (n / d) `mod` 10) [ 1000, 100, 10, 1 ]
  trimZeros s = case String.stripSuffix (String.Pattern "0") s of
    Just t | t /= "" -> trimZeros t
    _ -> s

-- | The parts of a perform box that decide WHICH chord sounds, as opposed to how
-- | it is voiced or broken up: its chord-index sequence (`seqText`, cycle = one
-- | bar), and the layers that move it in time or pitch when they always apply.
-- | Voicing, selection, arp and strum shape the chord, not the harmony, and a
-- | layer gated by a `when` clause applies to some cycles only, which a
-- | constant transform cannot say; harmony leaves both out.
data Shape = Slow Int | Fast Int | Transpose Int

-- | A box's harmony: its sequence with each chord index replaced by that
-- | chord's stack, then its shapes, innermost first. An empty `seqText` is the
-- | default, one chord per cycle (`<...>`). `Nothing` if there are no chords.
seqHarmony :: Array (Array Int) -> String -> Array Shape -> Maybe String
seqHarmony chords0 seqText shapes
  | null chords0 = Nothing
  | otherwise =
      let
        shift = foldl (\acc s -> case s of
          Transpose n -> acc + n
          _ -> acc) 0 shapes
        chords = map (map (_ + shift)) chords0
        base =
          if String.trim seqText == "" then "<" <> String.joinWith " " (map chordText chords) <> ">"
          else substitute chords seqText
      in
        Just (foldl (\p s -> case s of
          Slow n | n > 1 -> "[" <> p <> "]/" <> show n
          Fast n | n > 1 -> "[" <> p <> "]*" <> show n
          _ -> p) base shapes)

-- | Replace each chord index that stands as an atom with its chord. A number
-- | after a modifier (`*2`, `@3`, `!2`, `?0.5`, `/2`, `%4`, `:1`) or inside a
-- | Euclid's brackets `(3,8)` is not an index and is left alone; an index past
-- | the end is a rest, as Vetula plays it.
substitute :: Array (Array Int) -> String -> String
substitute chords src = go "" ' ' 0 (CU.toCharArray src)
  where
  go acc prev depth cs = case uncons cs of
    Nothing -> acc
    Just { head: c, tail }
      | c == '(' -> go (acc <> "(") c (depth + 1) tail
      | c == ')' -> go (acc <> ")") c (depth - 1) tail
      | isDigit c && depth == 0 && atomStart prev ->
          let
            digits = takeWhile isDigit cs
            rest = drop (length digits) cs
            word = CU.fromCharArray digits
          in
            -- a decimal (`0.5`) or a range (`0 .. 3`) is not an index: keep it
            case head rest of
              Just '.' -> go (acc <> word) (fromMaybe c (last digits)) depth rest
              _ ->
                let chord = Int.fromString word >>= (chords !! _)
                in go (acc <> maybe "~" chordText chord) 'x' depth rest
      | otherwise -> go (acc <> CU.singleton c) c depth tail
  atomStart p = all (_ /= p) [ '*', '/', '@', '!', '?', '%', ':', '.', '\'' ] && not (isDigit p) && not (isLetter p)
  isDigit c = c >= '0' && c <= '9'
  isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  maybe d f = case _ of
    Just x -> f x
    Nothing -> d
