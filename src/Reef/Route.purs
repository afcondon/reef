-- | **Harmony routes**: what feeds each of a machine's harmony inputs
-- | (docs/kb/plans/matrix-router.md). Odonus has two:
-- |
-- | - `odonus.grid`, q1: the set a cell's value is mapped onto (the shape): a
-- |   scale, or a chord as voiced, whose cells then play its arpeggios;
-- | - `odonus.out`, q2: the set the output snaps to past each head's offset.
-- |
-- | A route names one source for one input; an input with no route follows
-- | Odonus's own hand-set scale. Sources:
-- |
-- | - `scale "<dorian lydian>/4" d`: a pattern of Tidal's scale names, on a root;
-- | - `harmony "<c'maj7 a'min7>/2"`: a Tidal note pattern of chords, as voiced;
-- | - `chromatic`: null quantisation, Tidal's `scale "chromatic"`;
-- | - `vetula key`: Vetula's scale; `vetula 3`: Vetula's voice 3, its chords.
-- |
-- | The table is text, one route a line, as the router keeps it on the stage
-- | (`routing/harmony`) and Limulus can show it:
-- |
-- |     odonus.grid <- scale "<dorian lydian>/4" d
-- |     odonus.out <- vetula 3
-- |
-- | The rig applies a change as Odonus inputs, the same gestures `odonus $
-- | scale …` and `harmony …` make. Vetula's two sources need what Vetula is
-- | doing, which the rig knows and reef does not: its key (the stage object
-- | `vetula/key`, `parseKey`) and each voice's chords as a harmony pattern
-- | (the rig reads a card with Tidal). The rig hands them over as a `Context`;
-- | `resolve` turns the routes into what each input is fed (`Feeds`), and
-- | `feedInputs` gives the inputs that move Odonus from one to the next, so a
-- | card edited under a route re-feeds Odonus as a changed route does.
module Reef.Route
  ( Input(..)
  , Source(..)
  , Route
  , Routes
  , Key
  , Context
  , Feed(..)
  , Feeds
  , inputName
  , parse
  , print
  , sourceOf
  , setRoute
  , applyLine
  , parseKey
  , printKey
  , noContext
  , resolve
  , feedInputs
  , odonusInputs
  , printFeeds
  , parseFeeds
  ) where

import Prelude

import Data.Array (filter, find, foldl, index, mapMaybe, nub, sort)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Data.String (Pattern(..), joinWith, lastIndexOf, indexOf, split, trim)
import Data.String.CodeUnits as CU
import Data.Traversable (traverse)
import Reef.Input as I
import Reef.Move (pitchClass)
import Reef.PitchSet (PitchSet(..))

data Input = OdonusGrid | OdonusOut

derive instance Eq Input

data Source
  = Scale { pattern :: String, root :: Int }
  | Harmony String
  | VetulaKey
  | VetulaVoice Int

derive instance Eq Source

type Route = { input :: Input, source :: Source }

-- | At most one route per input, in input order.
type Routes = Array Route

inputs :: Array Input
inputs = [ OdonusGrid, OdonusOut ]

inputName :: Input -> String
inputName = case _ of
  OdonusGrid -> "odonus.grid"
  OdonusOut -> "odonus.out"

sourceOf :: Input -> Routes -> Maybe Source
sourceOf i rs = _.source <$> find (\r -> r.input == i) rs

-- | The table, from its text. Blank lines and `--` comments are skipped; a
-- | later route for an input replaces an earlier one.
parse :: String -> Either String Routes
parse text = do
  rs <- traverse line (filter keep (map trim (split (Pattern "\n") text)))
  pure (mapMaybe (\i -> find (\r -> r.input == i) (reverseArr rs)) inputs)
  where
  keep l = l /= "" && CU.take 2 l /= "--"
  reverseArr = foldl (\acc x -> [ x ] <> acc) []

line :: String -> Either String Route
line l = case split (Pattern "<-") l of
  [ lhs, rhs ] -> do
    input <- inputText (trim lhs)
    source <- sourceText (trim rhs)
    -- every input takes every source: a chord shapes the grid as its
    -- arpeggios (docs/kb/plans/harmony-routes-coherent.md)
    Right { input, source }
  _ -> Left ("a route is INPUT <- SOURCE: '" <> l <> "'")

sourceText :: String -> Either String Source
sourceText t = case split (Pattern " ") t of
  -- null quantisation (AC): every semitone on the grid; each note itself at
  -- the output. Tidal's own `scale "chromatic"`, by name.
  [ "chromatic" ] -> Right (Scale { pattern: "chromatic", root: 0 })
  [ "vetula", "key" ] -> Right VetulaKey
  [ "vetula", n ] | Just v <- Int.fromString n -> Right (VetulaVoice v)
  _ -> case quoted t of
    Just { head: "scale", body, after } -> do
      root <- if after == "" then Right 0 else pitchClass after
      Right (Scale { pattern: body, root })
    Just { head: "harmony", body, after: "" } -> Right (Harmony body)
    _ -> Left ("no source '" <> t <> "' (scale \"…\" ROOT, harmony \"…\", chromatic, vetula key, vetula N)")

-- | `head "body" after`, split at the first and last quote.
quoted :: String -> Maybe { head :: String, body :: String, after :: String }
quoted t = do
  a <- indexOf (Pattern "\"") t
  b <- lastIndexOf (Pattern "\"") t
  if b <= a then Nothing
  else Just
    { head: trim (CU.take a t)
    -- take and drop, not slice: purerl's slice returns a Maybe
    , body: CU.take (b - a - 1) (CU.drop (a + 1) t)
    , after: trim (CU.drop (b + 1) t)
    }

print :: Routes -> String
print rs = joinWith "\n" (map one rs)
  where
  one r = inputName r.input <> " <- " <> printSource r.source

printSource :: Source -> String
printSource = case _ of
  Scale { pattern: "chromatic", root: 0 } -> "chromatic"
  Scale s -> "scale \"" <> s.pattern <> "\"" <> (if s.root == 0 then "" else " " <> noteName s.root)
  Harmony h -> "harmony \"" <> h <> "\""
  VetulaKey -> "vetula key"
  VetulaVoice n -> "vetula " <> show n

noteName :: Int -> String
noteName pc = fromMaybe (show pc) (index [ "c", "cs", "d", "ds", "e", "f", "fs", "g", "gs", "a", "as", "b" ] pc)

-- | Route `input` to `source`, replacing what fed it; inputs stay in order.
setRoute :: Input -> Source -> Routes -> Routes
setRoute input source routes =
  mapMaybe (\i -> find (\r -> r.input == i) others) inputs
  where
  others = filter (\r -> r.input /= input) routes <> [ { input, source } ]

-- | **One route, as Limulus writes it** (`route $ odonus.grid <- vetula 2`):
-- | the table with that input's route set, or with `none` taken away. The rest
-- | of the table is as it was. The same syntax as a table line, so the
-- | Dashboard, Limulus and Odonus's labels all describe one table.
applyLine :: String -> Routes -> Either String Routes
applyLine l routes = case split (Pattern "<-") l of
  [ lhs, rhs ] | trim rhs == "none" -> do
    input <- inputText (trim lhs)
    Right (filter (\r -> r.input /= input) routes)
  _ -> do
    r <- line (trim l)
    Right (setRoute r.input r.source routes)

inputText :: String -> Either String Input
inputText = case _ of
  "odonus.grid" -> Right OdonusGrid
  "odonus.out" -> Right OdonusOut
  other -> Left ("no input '" <> other <> "' (odonus.grid, odonus.out)")

-- | Vetula's key: a root (pitch class) and the scale's steps above it.
type Key = { root :: Int, offsets :: Array Int }

-- | The key as the stage keeps it (`vetula/key`): the root, then the steps,
-- | `d 0 2 3 5 7 9 10`. The root reads as `pitchClass` does (`c`, `fs`, `bf`,
-- | or 0-11); steps are taken mod 12, sorted, and always hold the root.
parseKey :: String -> Either String Key
parseKey t = case filter (_ /= "") (split (Pattern " ") (trim t)) of
  [] -> Left "a key is ROOT STEPS…: d 0 2 3 5 7 9 10"
  ws -> do
    root <- pitchClass (fromMaybe "" (Array.head ws))
    steps <- traverse step (Array.drop 1 ws)
    pure { root, offsets: sort (nub ([ 0 ] <> map (\n -> ((n `mod` 12) + 12) `mod` 12) steps)) }
  where
  step w = case Int.fromString w of
    Just n -> Right n
    Nothing -> Left ("a key's steps are numbers: '" <> w <> "'")

printKey :: Key -> String
printKey k = joinWith " " ([ noteName k.root ] <> map show k.offsets)

-- | What the rig knows of Vetula: its key, if the page has said, and the
-- | harmony each voice (by channel) is playing, as a Tidal note pattern.
type Context =
  { key :: Maybe Key
  , voices :: Array { channel :: Int, harmony :: String }
  }

noContext :: Context
noContext = { key: Nothing, voices: [] }

-- | What an input is fed. `Unfed` is no route, or a Vetula source with
-- | nothing behind it: the input follows Odonus's hand-set scale.
data Feed
  = FeedScale { pattern :: String, root :: Int }
  | FeedHarmony String
  | FeedKey Key
  | Unfed

derive instance Eq Feed

type Feeds = { grid :: Feed, out :: Feed }

resolve :: Context -> Routes -> Feeds
resolve ctx rs = { grid: feed (sourceOf OdonusGrid rs), out: feed (sourceOf OdonusOut rs) }
  where
  feed = case _ of
    Just (Scale s) -> FeedScale s
    Just (Harmony h) -> FeedHarmony h
    Just VetulaKey -> maybe Unfed FeedKey ctx.key
    Just (VetulaVoice n) -> maybe Unfed (FeedHarmony <<< _.harmony) (find (\v -> v.channel == n) ctx.voices)
    Nothing -> Unfed

-- | The Odonus inputs that take it from feeding `old` to feeding `new`: for
-- | each input whose feed changed, what the new one sets, or, with none, what
-- | returns it to the hand-set scale. Each clears what the others set, since
-- | an explicit pitch set wins over the scale, and an output scale over a
-- | harmony.
feedInputs :: Feeds -> Feeds -> Array I.Input
feedInputs old new = grid <> out
  where
  grid
    | old.grid == new.grid = []
    | otherwise = case new.grid of
        FeedScale s -> [ I.SetGridHarmony Nothing, I.ClearPitchSet, I.SetScalePattern (Just s.pattern), I.SetRoot s.root ]
        FeedKey k -> [ I.SetGridHarmony Nothing, I.SetScalePattern Nothing, I.SetPitchSet (PitchSet { offsets: k.offsets, root: 48 + k.root, period: Just 12 }) ]
        FeedHarmony h -> [ I.SetScalePattern Nothing, I.SetGridHarmony (Just h) ]
        Unfed -> [ I.SetGridHarmony Nothing, I.ClearPitchSet, I.SetScalePattern Nothing ]
  out
    | old.out == new.out = []
    | otherwise = case new.out of
        FeedHarmony h -> [ I.SetOutScale Nothing 0, I.SetHarmony (Just h) ]
        FeedScale s -> [ I.SetHarmony Nothing, I.SetOutScale (Just s.pattern) s.root ]
        _ -> [ I.SetHarmony Nothing, I.SetOutScale Nothing 0 ]

-- | `feedInputs` with nothing known of Vetula, from the routes `old` to `new`:
-- | what a table written by itself does, and what checks it before keeping.
odonusInputs :: Routes -> Routes -> Array I.Input
odonusInputs old new = feedInputs (resolve noContext old) (resolve noContext new)

-- | The feeds as text, as the rig publishes them once resolved (`odonus/feeds`)
-- | for a page that plays Odonus itself (Solo) to apply: the routes' syntax,
-- | with Vetula's sources replaced by what they gave, and `key` for a key.
-- | An unfed input has no line.
-- |
-- |     odonus.grid <- key d 0 2 3 5 7 9 10
-- |     odonus.out <- harmony "<[0,4,7] [2,5,9]>"
printFeeds :: Feeds -> String
printFeeds fs = joinWith "\n" (mapMaybe one [ Tuple OdonusGrid fs.grid, Tuple OdonusOut fs.out ])
  where
  one (Tuple i f) = (\t -> inputName i <> " <- " <> t) <$> case f of
    FeedScale s -> Just (printSource (Scale s))
    FeedHarmony h -> Just (printSource (Harmony h))
    FeedKey k -> Just ("key " <> printKey k)
    Unfed -> Nothing

parseFeeds :: String -> Either String Feeds
parseFeeds text = foldl step (Right { grid: Unfed, out: Unfed }) (filter keep (map trim (split (Pattern "\n") text)))
  where
  keep l = l /= "" && CU.take 2 l /= "--"
  step acc l = do
    fs <- acc
    case split (Pattern "<-") l of
      [ lhs, rhs ] -> do
        f <- feedText (trim rhs)
        case trim lhs of
          "odonus.grid" -> Right fs { grid = f }
          "odonus.out" -> Right fs { out = f }
          other -> Left ("no input '" <> other <> "' (odonus.grid, odonus.out)")
      _ -> Left ("a feed is INPUT <- FEED: '" <> l <> "'")
  feedText t = case CU.take 4 t of
    "key " -> FeedKey <$> parseKey (CU.drop 4 t)
    _ -> sourceText t >>= case _ of
      Scale s -> Right (FeedScale s)
      Harmony h -> Right (FeedHarmony h)
      _ -> Left ("a feed is resolved, not a Vetula source: '" <> t <> "'")
