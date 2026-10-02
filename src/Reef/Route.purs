-- | **Harmony routes**: what feeds each of a machine's harmony inputs
-- | (docs/kb/plans/matrix-router.md). Odonus has two:
-- |
-- | - `odonus.grid`, q1: the scale a cell's value is mapped onto (the shape);
-- | - `odonus.out`, q2: the set the output snaps to past each head's offset.
-- |
-- | A route names one source for one input; an input with no route follows
-- | Odonus's own hand-set scale. Sources:
-- |
-- | - `scale "<dorian lydian>/4" d`: a pattern of Tidal's scale names, on a root;
-- | - `harmony "<c'maj7 a'min7>/2"`: a Tidal note pattern of chords (out only);
-- | - `vetula key`: Vetula's scale; `vetula 3`: Vetula's voice 3, its chords.
-- |
-- | The table is text, one route a line, as the router keeps it on the stage
-- | (`routing/harmony`) and Limulus can show it:
-- |
-- |     odonus.grid <- scale "<dorian lydian>/4" d
-- |     odonus.out <- vetula 3
-- |
-- | The rig applies a change as Odonus inputs (`odonusInputs`), the same
-- | gestures `odonus $ scale …` and `harmony …` make. Vetula's sources are
-- | conducted from Vetula, so they make no inputs here.
module Reef.Route
  ( Input(..)
  , Source(..)
  , Route
  , Routes
  , inputName
  , parse
  , print
  , sourceOf
  , odonusInputs
  ) where

import Prelude

import Data.Array (filter, find, foldl, index, mapMaybe)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, lastIndexOf, indexOf, split, trim)
import Data.String.CodeUnits as CU
import Data.Traversable (traverse)
import Reef.Input as I
import Reef.Move (pitchClass)

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
    input <- case trim lhs of
      "odonus.grid" -> Right OdonusGrid
      "odonus.out" -> Right OdonusOut
      other -> Left ("no input '" <> other <> "' (odonus.grid, odonus.out)")
    source <- sourceText (trim rhs)
    case input, source of
      OdonusGrid, Harmony _ -> Left "odonus.grid takes a scale (scale \"…\" or vetula key), not a harmony"
      OdonusGrid, VetulaVoice _ -> Left "odonus.grid takes a scale (scale \"…\" or vetula key), not a voice's chords"
      _, _ -> Right { input, source }
  _ -> Left ("a route is INPUT <- SOURCE: '" <> l <> "'")

sourceText :: String -> Either String Source
sourceText t = case split (Pattern " ") t of
  [ "vetula", "key" ] -> Right VetulaKey
  [ "vetula", n ] | Just v <- Int.fromString n -> Right (VetulaVoice v)
  _ -> case quoted t of
    Just { head: "scale", body, after } -> do
      root <- if after == "" then Right 0 else pitchClass after
      Right (Scale { pattern: body, root })
    Just { head: "harmony", body, after: "" } -> Right (Harmony body)
    _ -> Left ("no source '" <> t <> "' (scale \"…\" ROOT, harmony \"…\", vetula key, vetula N)")

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
  one r = inputName r.input <> " <- " <> sourceText' r.source
  sourceText' = case _ of
    Scale s -> "scale \"" <> s.pattern <> "\"" <> (if s.root == 0 then "" else " " <> noteName s.root)
    Harmony h -> "harmony \"" <> h <> "\""
    VetulaKey -> "vetula key"
    VetulaVoice n -> "vetula " <> show n

noteName :: Int -> String
noteName pc = fromMaybe (show pc) (index [ "c", "cs", "d", "ds", "e", "f", "fs", "g", "gs", "a", "as", "b" ] pc)

-- | The Odonus inputs that take it from the routes `old` to `new`: for each
-- | input whose source changed, what its new source sets, or, with none, what
-- | returns it to the hand-set scale. Vetula's sources set nothing here (Vetula
-- | conducts them), but routing one in still releases what fed the input.
odonusInputs :: Routes -> Routes -> Array I.Input
odonusInputs old new = grid <> out
  where
  changed i = sourceOf i old /= sourceOf i new
  grid
    | not (changed OdonusGrid) = []
    | otherwise = case sourceOf OdonusGrid new of
        Just (Scale s) -> [ I.SetScalePattern (Just s.pattern), I.SetRoot s.root ]
        _ -> [ I.SetScalePattern Nothing ]
  out
    | not (changed OdonusOut) = []
    | otherwise = case sourceOf OdonusOut new of
        Just (Harmony h) -> [ I.SetHarmony (Just h) ]
        Just (Scale s) -> [ I.SetOutScale (Just s.pattern) s.root ]
        _ -> [ I.SetHarmony Nothing, I.SetOutScale Nothing 0 ]
