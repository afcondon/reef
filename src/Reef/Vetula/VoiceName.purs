-- | **A Vetula voice's name**: a capital letter, P to W (AC, 2026-10-06),
-- | so that a number is always a MIDI channel (`ch3`) and a letter always a
-- | voice (`Q $ …`, `odonus.out <- vetula Q`). P..W avoid the note names
-- | A..G. Inside, a voice is still numbered 1..8 (P = 1), which nobody types.
module Reef.Vetula.VoiceName
  ( voiceLetter
  , voiceOfName
  , voiceCount
  ) where

import Prelude

import Data.Array (elemIndex, index)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), stripPrefix)

letters :: Array String
letters = [ "P", "Q", "R", "S", "T", "U", "V", "W" ]

-- | How many voices there can be.
voiceCount :: Int
voiceCount = 8

-- | Voice `n`'s letter (1 = P); past W, `vN`, as before letters.
voiceLetter :: Int -> String
voiceLetter n = fromMaybe ("v" <> show n) (index letters (n - 1))

-- | A voice from its letter, or from the old `vN` / bare number.
voiceOfName :: String -> Maybe Int
voiceOfName s = case elemIndex s letters of
  Just i -> Just (i + 1)
  Nothing -> case stripPrefix (Pattern "v") s of
    Just rest -> Int.fromString rest
    Nothing -> Int.fromString s
