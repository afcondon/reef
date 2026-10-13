-- | `Reef.Word` — the word a saved thing plays by in a line
-- | (`balistes $ "<lo-house-110 disco-120>"`, `odonus $ "<verse chorus>"`),
-- | and its key on the stage. A word is given once, from the thing's name
-- | when it is first named, and kept when it is renamed, so a line that
-- | names it never goes silent.
module Reef.Word
  ( slug
  , freeWord
  ) where

import Prelude

import Data.Array as Array
import Data.Array (dropWhile, foldl, reverse)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)

-- | A name as a word. Letters, digits and `. - _` are kept (a Tidal word may
-- | hold `:` too, but it means a sample index there and the stage refuses
-- | it); each run of anything else becomes one `-`, and a word starts with a
-- | letter or digit and ends without a `-`: "lo house 110" is `lo-house-110`.
slug :: String -> String
slug name = fromCharArray (reverse (dropWhile (_ == '-') (reverse (dropWhile (not <<< alphaNum) dashed))))
  where
  dashed = foldl step [] (toCharArray name)
  step acc c
    | wordChar c = acc <> [ c ]
    | otherwise = case Array.last acc of
        Just '-' -> acc
        _ -> acc <> [ '-' ]
  wordChar c = alphaNum c || c == '.' || c == '-' || c == '_'
  alphaNum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | The word for `name` that `taken` does not hold: its slug, else the slug
-- | with the first free `-2`, `-3`, … ("" for a name with no word in it).
freeWord :: (String -> Boolean) -> String -> String
freeWord taken name = case slug name of
  "" -> ""
  w -> go w 1
  where
  go w n =
    let candidate = if n == 1 then w else w <> "-" <> show n
    in if taken candidate then go w (n + 1) else candidate
