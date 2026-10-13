-- | Reef.Balistes.Lepidoptera — the eDSL print/parse for Balistes fixed
-- | rhythms (Lepidoptera A2). Per AC's rule, a fixed rhythm is rendered as a
-- | **fully-structured record**, not mini-notation: each lane lists its hits as
-- | `hit <step> <vel>` (default overlay) or `cell <step> <vel> { … }` (tweaked).
-- | Mini-notation is reserved for where it compresses and reveals structure;
-- | a dense per-cell grid is neither, and a structured list reads far better
-- | than a rhythm string aligned against a parallel gain string.
-- |
-- | The output is valid `Tidal.Balistes` eDSL (`balistesPattern` / `lane` /
-- | `hit` / `cell` are functions), so it's consumable by Calypso and shippable
-- | to purerl-tidal — the canonical, transferable form of a rhythm.
module Reef.Balistes.Lepidoptera
  ( printPattern
  , parsePattern
  , rhythmKey
  , rhythmOfKey
  , rhythmSlug
  ) where

import Prelude

import Reef.Word as Word

import Data.Array (mapMaybe, range, span, uncons)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), split, stripPrefix) as Str
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Common (joinWith)
import Reef.Balistes.Pattern as P

-- ---------------------------------------------------------------------------
-- Print
-- ---------------------------------------------------------------------------

-- | Render a fixed rhythm as `balistesPattern "<name>" <steps> [ <lanes> ]`.
printPattern :: P.FixedPattern -> String
printPattern p =
  "balistesPattern " <> show p.name <> " " <> show p.steps <> "\n"
    <> "  [ " <> joinWith "\n  , " (map (printLane p) (P.usedLanes p)) <> "\n  ]"

-- | One lane: `lane "<name>" [ <cells> ]`, listing only its hits. The lane
-- | is the drum; which note reaches an instrument is the routing's
-- | (2026-10-13), so a rhythm names none.
printLane :: P.FixedPattern -> Int -> String
printLane p lane =
  "lane " <> show (P.laneName lane)
    <> " [ " <> joinWith ", " (mapMaybe atStep (range 0 (p.steps - 1))) <> " ]"
  where
  atStep step =
    let c = P.cellAt p lane step
    in if c.vel <= 0 then Nothing else Just (printCell step c)

-- | A plain hit, or — when the overlay is tweaked — the fuller `cell` form.
printCell :: Int -> P.Cell -> String
printCell step c =
  if P.cellTweaked c then
    "cell " <> show step <> " " <> show c.vel
      <> " { prob: " <> show c.prob
      <> ", cond: " <> show (P.condLabel c.cond)
      <> ", ratchet: " <> show c.ratchet <> " }"
  else
    "hit " <> show step <> " " <> show c.vel

-- ---------------------------------------------------------------------------
-- On the stage
-- ---------------------------------------------------------------------------

-- | **A saved rhythm on the stage**: `balistes/rhythm/<slug>`, its text this
-- | module's printed form, which carries the name as typed. Balistes keeps one
-- | for every named rhythm in its bank; a `balistes $ "<a b>"` line on the rig
-- | plays them by the word, which is the slug.
rhythmKey :: String -> String
rhythmKey name = "balistes/rhythm/" <> rhythmSlug name

-- | The slug a stage key names, or Nothing for any other key.
rhythmOfKey :: String -> Maybe String
rhythmOfKey = Str.stripPrefix (Str.Pattern "balistes/rhythm/")

-- | **A rhythm's name as one word of a Tidal pattern**, and the last part of
-- | its stage key. Letters, digits and `. - _` are kept (a Tidal word may hold
-- | `:` too, but it means a sample index there and the stage refuses it); each
-- | run of anything else becomes one `-`, and a word starts with a letter or
-- | digit and ends without a `-`: "lo house 110" is `lo-house-110`.
rhythmSlug :: String -> String
rhythmSlug = Word.slug

-- ---------------------------------------------------------------------------
-- Parse
-- ---------------------------------------------------------------------------
--
-- By hand, over tokens, rather than with `parsing`: the JS and purerl package
-- sets carry versions of it with different module names, and this text is
-- read on both (the page prints it, the rig plays it).

data Token = TStr String | TInt Int | TWord String | TSym Char

derive instance Eq Token

-- | Parse the eDSL back to a fixed rhythm (Nothing on malformed input — the
-- | Store then drops it and falls back to the bundled patterns).
parsePattern :: String -> Maybe P.FixedPattern
parsePattern input = do
  toks <- tokenize (toCharArray input)
  r <- patternP toks
  case r.rest of
    [] -> Just r.value
    _ -> Nothing

type Step a = { value :: a, rest :: Array Token }

tokenize :: Array Char -> Maybe (Array Token)
tokenize = go []
  where
  go acc cs = case uncons cs of
    Nothing -> Just acc
    Just { head: c, tail }
      | c == ' ' || c == '\n' || c == '\t' || c == '\r' -> go acc tail
      | c == '"' ->
          let s = span (_ /= '"') tail
          in case uncons s.rest of
            Just { tail: after } -> go (Array.snoc acc (TStr (fromCharArray s.init))) after
            Nothing -> Nothing
      | isDigit c || c == '-' ->
          let s = span isDigit tail
          in case Int.fromString (fromCharArray (Array.cons c s.init)) of
            Just n -> go (Array.snoc acc (TInt n)) s.rest
            Nothing -> Nothing
      | isLetter c ->
          let s = span (\x -> isLetter x || isDigit x || x == ':') tail
          in go (Array.snoc acc (TWord (fromCharArray (Array.cons c s.init)))) s.rest
      | otherwise -> go (Array.snoc acc (TSym c)) tail
  isDigit c = c >= '0' && c <= '9'
  isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

tok :: Token -> Array Token -> Maybe (Array Token)
tok t ts = case uncons ts of
  Just { head, tail } | head == t -> Just tail
  _ -> Nothing

str :: Array Token -> Maybe (Step String)
str ts = case uncons ts of
  Just { head: TStr s, tail } -> Just { value: s, rest: tail }
  _ -> Nothing

int :: Array Token -> Maybe (Step Int)
int ts = case uncons ts of
  Just { head: TInt n, tail } -> Just { value: n, rest: tail }
  _ -> Nothing

-- | `[ x, y, … ]`, possibly empty.
bracketed :: forall a. (Array Token -> Maybe (Step a)) -> Array Token -> Maybe (Step (Array a))
bracketed item ts0 = do
  ts <- tok (TSym '[') ts0
  case tok (TSym ']') ts of
    Just rest -> Just { value: [], rest }
    Nothing -> more [] ts
  where
  more acc ts = do
    r <- item ts
    let acc' = Array.snoc acc r.value
    case tok (TSym ',') r.rest of
      Just rest -> more acc' rest
      Nothing -> do
        rest <- tok (TSym ']') r.rest
        pure { value: acc', rest }

type PLane = { nm :: String, cells :: Array { step :: Int, cell :: P.Cell } }

patternP :: Array Token -> Maybe (Step P.FixedPattern)
patternP ts0 = do
  ts1 <- tok (TWord "balistesPattern") ts0
  name <- str ts1
  steps <- int name.rest
  lanes <- bracketed laneP steps.rest
  pure { value: build name.value steps.value lanes.value, rest: lanes.rest }

laneP :: Array Token -> Maybe (Step PLane)
laneP ts0 = do
  ts1 <- tok (TWord "lane") ts0
  nm <- str ts1
  -- a rhythm written before notes moved to the routing names one: read past it
  let afterNote = maybe nm.rest _.rest (int nm.rest)
  cells <- bracketed cellP afterNote
  pure { value: { nm: nm.value, cells: cells.value }, rest: cells.rest }

cellP :: Array Token -> Maybe (Step { step :: Int, cell :: P.Cell })
cellP ts0 = case uncons ts0 of
  Just { head: TWord "hit", tail } -> do
    s <- int tail
    v <- int s.rest
    pure { value: { step: s.value, cell: P.hitCell v.value }, rest: v.rest }
  Just { head: TWord "cell", tail } -> do
    s <- int tail
    v <- int s.rest
    ts1 <- tok (TSym '{') v.rest
    ts2 <- tok (TWord "prob:") ts1
    prob <- int ts2
    ts3 <- tok (TSym ',') prob.rest >>= tok (TWord "cond:")
    cs <- str ts3
    ts4 <- tok (TSym ',') cs.rest >>= tok (TWord "ratchet:")
    rt <- int ts4
    rest <- tok (TSym '}') rt.rest
    pure { value: { step: s.value, cell: { vel: v.value, prob: prob.value, cond: parseCond cs.value, ratchet: rt.value } }, rest }
  _ -> Nothing

-- "X:Y" → CEvery X Y; anything else (incl. "—") → CAlways.
parseCond :: String -> P.TrigCond
parseCond s = case Str.split (Str.Pattern ":") s of
  [ a, b ] -> case Int.fromString a, Int.fromString b of
    Just x, Just y -> P.CEvery x y
    _, _ -> P.CAlways
  _ -> P.CAlways

build :: String -> Int -> Array PLane -> P.FixedPattern
build name steps lanes = foldl applyLane (P.emptyPattern name steps) lanes
  where
  applyLane p l = case P.laneIndexOf l.nm of
    Nothing -> p
    Just idx -> foldl (placeCell idx) p l.cells
  placeCell idx p c = P.modifyCell idx c.step (const c.cell) p
