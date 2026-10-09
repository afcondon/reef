-- | **Moves: Odonus by the line.** A move is several gestures landing at once,
-- | written as a line and evaluated, as a Tidal pattern is:
-- |
-- |     odonus $ unison # phase 2
-- |     odonus $ notes low # mutate notes 30
-- |     odonus $ for 4 (mutate notes 30)
-- |     odonus $ harmony "<c'maj7 a'min7>/2" # mutate notes 20
-- |
-- | Clicking the same controls one at a time is a sequence of small
-- | modulations, each heard as it happens; a move changes them together, on
-- | one step, and re-evaluating its line gives the same result. So a move
-- | compiles to a batch of `Reef.Input`s (the lockstep gesture vocabulary)
-- | applied at one tick, on either runtime, by `applyInputs`.
-- |
-- | **Not Tidal.** The `odonus` head word marks the line as not-Tidal, so it
-- | cannot muddy purerl-tidal's parity with Haskell Tidal. The two operators
-- | keep Tidal's meanings: `#` chains left to right (`a # b` is a, then b) and
-- | `.` composes as Haskell does (`a . b` is b, then a), binding tighter. So
-- | `unison # phase 2` and `phase 2 . unison` are the same move; `unison .
-- | phase 2` would unify after phasing, which undoes the phase.
-- |
-- | **Timed moves.** `for n m` makes the move m, then n bars later restores the
-- | *settings* m changed (mutation, freeze, offsets, lengths, patterns,
-- | transposition, gate) to what they were. It never restores *material*: the
-- | notes a mutation produced, or a unison, stay. That is the point of
-- | "mutate for a bit": the line evolves out of where it was and keeps what it
-- | found.
-- |
-- | **Harmony.** `harmony "PATTERN"` gives Odonus a Tidal note pattern to
-- | quantise to, as `note` would read it; `harmony off` returns it to its
-- | scale. Reef stores the text and never reads it: the host samples it each
-- | step with Littorina (`Reef.Odonus.followHarmony`), and rejects a pattern
-- | Tidal would refuse before the move is sent.
-- |
-- | **Scales by name.** `scale "<dorian mixolydian>/4"` gives the grid's scale
-- | to a pattern of Tidal's scale names (`Tidal.Scales`), sampled by the host
-- | the same way; `scale off` returns to the scale set by hand. The root is
-- | separate, as in Tidal, where it is added as a note: `root d`, `root fs`,
-- | `root 2` (a pitch class, C = 0).
module Reef.Move
  ( Move(..)
  , parse
  , inputsOf
  , Scheduled
  , schedule
  , verbs
  , pitchClass
  ) where

import Prelude

import Data.Traversable (traverse)

import Data.Foldable (foldl)
import Data.Array (catMaybes, concatMap, cons, drop, filter, find, head, length, mapWithIndex, null, reverse, snoc, takeWhile, uncons, (!!))
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Reef.Gen (GenKind(..))
import Reef.Input (Input(..), SimState)
import Reef.Odonus (knobMax)
import Reef.Odonus.Patch as Patch

-- | A parsed line: gestures, a sequence of moves, or a move for n bars.
data Move
  = Gestures (Array Input)
  | Sequence (Array Move)
  | For Int Move

-- | The gestures to apply now, in order.
inputsOf :: Move -> Array Input
inputsOf = case _ of
  Gestures is -> is
  Sequence ms -> concatMap inputsOf ms
  For _ m -> inputsOf m

-- | What to apply now, and what to apply after some bars. The voice turns
-- | bars into its own model steps.
type Scheduled = { now :: Array Input, later :: Array { bars :: Int, inputs :: Array Input } }

-- | Schedule a move against the state it will be applied to: its gestures
-- | now, and for each `for`, the settings it changes restored afterwards.
schedule :: Move -> SimState -> Scheduled
schedule m s = { now: inputsOf m, later: fors m }
  where
  fors = case _ of
    Gestures _ -> []
    Sequence ms -> concatMap fors ms
    For n inner -> cons { bars: n, inputs: restore (inputsOf inner) s } (fors inner)

-- | The gestures that put back the settings these inputs change.
restore :: Array Input -> SimState -> Array Input
restore is s = concatMap undo is
  where
  heads = s.odo.heads
  perHead f = mapWithIndex f heads
  gen k = find (\g -> g.kind == k) s.gen
  undo = case _ of
    SetGenOn k _ -> genBack k
    SetAmt k _ -> genBack k
    SetRate k _ -> genBack k
    SetFrozen _ -> [ SetFrozen s.frozen ]
    FanOffsets _ -> perHead \i h -> SetHeadOffset i h.offset
    NudgeOffsets _ -> perHead \i h -> SetHeadOffset i h.offset
    SetHeadOffset i _ -> catMaybes [ (\h -> SetHeadOffset i h.offset) <$> heads !! i ]
    StaggerLengths _ -> perHead \i h -> SetHeadLen i h.len
    SetHeadLen i _ -> catMaybes [ (\h -> SetHeadLen i h.len) <$> heads !! i ]
    SetHeadClock i _ -> catMaybes [ (\h -> SetHeadClock i h.clock) <$> heads !! i ]
    SetHeadPattern i _ -> catMaybes [ (\h -> SetHeadPattern i h.patternIx) <$> heads !! i ]
    SetHeadTransp i _ -> catMaybes [ (\h -> SetHeadTransp i h.transp) <$> heads !! i ]
    SetGatePct _ -> [ SetGatePct s.odo.gatePct ]
    SetHarmony _ -> [ SetHarmony s.odo.harmony ]
    SetHeadsPattern _ -> [ SetHeadsPattern s.odo.headsPattern ]
    -- one of a scale pattern and a chord pattern shapes the grid
    SetScalePattern _ -> [ SetGridHarmony s.odo.gridHarmony, SetScalePattern s.odo.scalePattern ]
    SetGridHarmony _ -> [ SetScalePattern s.odo.scalePattern, SetGridHarmony s.odo.gridHarmony ]
    -- one of harmony and outScale holds the output, so restore whichever did
    SetOutScale _ _ -> [ SetOutScale (_.pattern <$> s.odo.outScale) (maybe 0 _.root s.odo.outScale), SetHarmony s.odo.harmony ]
    SetRoot _ -> [ SetRoot s.odo.rootPc ]
    _ -> []
  genBack k = case gen k of
    Just g -> [ SetGenOn k g.on, SetAmt k g.amt, SetRate k g.rate ]
    Nothing -> []

-- ── the verbs ────────────────────────────────────────────────────────────────

-- | Every verb, with its arguments, for help text and errors.
verbs :: Array String
verbs =
  [ "unison"
  , "phase N"
  , "stagger N"
  , "notes low|mid|melody|roll"
  , "mutate KIND [AMOUNT]"
  , "still [KIND]"
  , "rate KIND N"
  , "freeze"
  , "thaw"
  , "gate PERCENT"
  , "pattern N N N N"
  , "transp N N N N"
  , "len N N N N"
  , "clock dur|step [dur|step ...]"
  , "offset N N N N"
  , "heads \"PATTERN\" | off"
  , "harmony \"PATTERN\" | off"
  , "scale \"PATTERN\" | off"
  , "outscale \"PATTERN\" [ROOT] | off"
  , "root NOTE"
  , "for BARS (MOVE)"
  ]

kinds :: Array (Tuple String GenKind)
kinds =
  [ Tuple "notes" GNotes, Tuple "gate" GGate, Tuple "skip" GSkip, Tuple "glide" GGlide
  , Tuple "len" GLen, Tuple "ratchet" GRatchet, Tuple "heads" GHeads, Tuple "transp" GTransp
  , Tuple "pattern" GPattern, Tuple "speed" GSpeed, Tuple "key" GKey, Tuple "vel" GVel
  ]

kindNamed :: String -> Either String GenKind
kindNamed w = case find (\(Tuple n _) -> n == w) kinds of
  Just (Tuple _ k) -> Right k
  Nothing -> Left ("no generator '" <> w <> "' (one of " <> String.joinWith ", " (map (\(Tuple n _) -> n) kinds) <> ")")

-- | One verb and its words, as gestures.
verb :: String -> Array String -> Either String (Array Input)
verb name args = case name, args of
  "unison", [] -> Right [ UnifyHeads ]
  "phase", [ n ] -> one FanOffsets <$> int n
  "stagger", [ n ] -> one StaggerLengths <$> int n
  "notes", [ "low" ] -> Right [ SetAllNotes 0 ]
  "notes", [ "mid" ] -> Right [ SetAllNotes (knobMax `div` 2) ]
  "notes", [ "melody" ] -> Right [ SeedMelody ]
  "notes", [ "roll" ] -> Right [ RollAllNotes ]
  "mutate", [ k ] -> (\g -> [ SetGenOn g true ]) <$> kindNamed k
  "mutate", [ k, a ] -> (\g amt -> [ SetGenOn g true, SetAmt g amt ]) <$> kindNamed k <*> int a
  "still", [] -> Right (map (\(Tuple _ g) -> SetGenOn g false) kinds)
  "still", [ k ] -> (\g -> [ SetGenOn g false ]) <$> kindNamed k
  "rate", [ k, n ] -> (\g r -> [ SetRate g r ]) <$> kindNamed k <*> int n
  "freeze", [] -> Right [ SetFrozen true ]
  "thaw", [] -> Right [ SetFrozen false ]
  "gate", [ n ] -> one SetGatePct <$> int n
  -- Pattern numbers count from 1, as the heads do on the panel.
  "pattern", ns | not (null ns) -> perHead (\i v -> SetHeadPattern i (v - 1)) ns
  "transp", ns | not (null ns) -> perHead SetHeadTransp ns
  "len", ns | not (null ns) -> perHead SetHeadLen ns
  -- What moves each head on: `dur`, its notes' lengths (it holds each cell
  -- for the cell's dur), or `step`, its steps. One word sets every head.
  "clock", [ w ] -> (\c -> mapWithIndex (\i _ -> SetHeadClock i c) heads0) <$> clockWord w
  "clock", ws | not (null ws) -> mapWithIndex SetHeadClock <$> traverse clockWord ws
  "offset", ns | not (null ns) -> perHead SetHeadOffset ns
  "root", [ w ] -> one SetRoot <$> pitchClass w
  _, _ ->
    if isVerb name then Left ("'" <> name <> "' takes: " <> usage name)
    else Left ("no verb '" <> name <> "' (try " <> String.joinWith ", " verbs <> ")")
  where
  one f v = [ f v ]
  perHead f ns = mapWithIndex f <$> traverseInts ns
  isVerb n = find (\v -> firstWord v == n) verbs /= Nothing
  usage n = fromMaybe n (find (\v -> firstWord v == n) verbs)
  firstWord v = fromMaybe v (head (String.split (String.Pattern " ") v))

clockWord :: String -> Either String Int
clockWord = case _ of
  "dur" -> Right 1
  "step" -> Right 0
  w -> Left ("a clock is dur or step, not '" <> w <> "'")

-- | Four heads, for a word that sets every one.
heads0 :: Array Unit
heads0 = [ unit, unit, unit, unit ]

int :: String -> Either String Int
int w = case Int.fromString w of
  Just n -> Right n
  Nothing -> Left ("'" <> w <> "' is not a whole number")

-- | A root: a pitch class (`2`), or a note name as Tidal spells one, a
-- | letter then any of `s` (sharp), `f` (flat), `n` (natural), and an octave,
-- | which a pitch class ignores (`d`, `fs`, `bf`, `c5`).
pitchClass :: String -> Either String Int
pitchClass w = case Int.fromString w of
  Just n -> Right (((n `mod` 12) + 12) `mod` 12)
  Nothing -> case CU.uncons w of
    Just { head: l, tail } | Just base <- letter l ->
      let
        mods = CU.takeWhile (\c -> c == 's' || c == 'f' || c == 'n') tail
        octave = CU.drop (CU.length mods) tail
        shift = foldl (\acc c -> acc + (if c == 's' then 1 else if c == 'f' then -1 else 0)) 0 (CU.toCharArray mods)
      in
        if octave == "" || Int.fromString octave /= Nothing then Right ((((base + shift) `mod` 12) + 12) `mod` 12)
        else Left ("'" <> w <> "' is not a note name")
    _ -> Left ("'" <> w <> "' is not a note name or a pitch class")
  where
  letter = case _ of
    'c' -> Just 0
    'd' -> Just 2
    'e' -> Just 4
    'f' -> Just 5
    'g' -> Just 7
    'a' -> Just 9
    'b' -> Just 11
    _ -> Nothing

traverseInts :: Array String -> Either String (Array Int)
traverseInts ws = go [] ws
  where
  go acc xs = case uncons xs of
    Nothing -> Right acc
    Just { head: x, tail } -> int x >>= \n -> go (snoc acc n) tail

-- ── the line ─────────────────────────────────────────────────────────────────

data Token = Word String | Quoted String | Dot | Hash | Open | Close

derive instance Eq Token

-- | Words and operators; a `"..."` is one token whatever it holds, so a
-- | pattern's `.` and spaces stay its own.
tokenize :: String -> Either String (Array Token)
tokenize src = go [] (CU.toCharArray src) ""
  where
  go acc cs w = case uncons cs of
    Nothing -> Right (flush acc w)
    Just { head: c, tail } -> case c of
      '"' ->
        let
          body = CU.fromCharArray (takeWhile (_ /= '"') tail)
          after = drop (CU.length body) tail
        in
          case uncons after of
            Just { tail: rest } -> go (snoc (flush acc w) (Quoted body)) rest ""
            Nothing -> Left "unclosed '\"'"
      '.' -> go (snoc (flush acc w) Dot) tail ""
      '#' -> go (snoc (flush acc w) Hash) tail ""
      '(' -> go (snoc (flush acc w) Open) tail ""
      ')' -> go (snoc (flush acc w) Close) tail ""
      _ | c == ' ' || c == '\t' || c == '\n' || c == '\r' -> go (flush acc w) tail ""
      _ -> go acc tail (w <> CU.singleton c)
  flush acc w = if w == "" then acc else snoc acc (Word w)

-- | Parse a line: `odonus $ MOVE`, or the bare `MOVE`.
parse :: String -> Either String Move
parse src = do
  let
    -- Patch.trimText, not String.trim: purerl's leaves a multi-line block
    -- (a recall Limulus sends whole) untrimmed, so the `$` stayed on
    body = case String.stripPrefix (String.Pattern "odonus") (Patch.trimText src) of
      Just rest -> fromMaybe rest (String.stripPrefix (String.Pattern "$") (Patch.trimText rest))
      Nothing -> src
  -- A recall, as a mark's code gives it (Reef.Odonus.Patch): the whole body
  -- is one value, read before the move syntax, whose `#` a `C#` would trip.
  case Patch.trimText body of
    t | Just _ <- String.stripPrefix (String.Pattern "odonusPatch") t ->
          if isJust (Patch.parsePatch t) then Right (Gestures [ RecallPatch t ])
          else Left "this odonusPatch does not read (the fields in their printed order?)"
      | Just _ <- String.stripPrefix (String.Pattern "odonusNow") t ->
          if isJust (Patch.parseNow t) then Right (Gestures [ RecallNow t ])
          else Left "this odonusNow does not read"
      | otherwise -> do
          tokens <- tokenize body
          { move, rest } <- hashChain tokens
          if null rest then Right move else Left "unexpected ')' or trailing words"

type P = Array Token -> Either String { move :: Move, rest :: Array Token }

-- | `a # b # c`, left to right; `#` binds loosest.
hashChain :: P
hashChain ts = do
  first <- dotChain ts
  go [ first.move ] first.rest
  where
  go acc rest = case uncons rest of
    Just { head: Hash, tail } -> dotChain tail >>= \r -> go (snoc acc r.move) r.rest
    _ -> Right { move: seqOf acc, rest }

-- | `a . b . c`, composed as functions are: c, then b, then a.
dotChain :: P
dotChain ts = do
  first <- term ts
  go [ first.move ] first.rest
  where
  go acc rest = case uncons rest of
    Just { head: Dot, tail } -> term tail >>= \r -> go (snoc acc r.move) r.rest
    _ -> Right { move: seqOf (reverse acc), rest }

-- | A verb with its words, `( MOVE )`, or `for BARS TERM`.
term :: P
term ts = case uncons ts of
  Just { head: Open, tail } -> do
    r <- hashChain tail
    case uncons r.rest of
      Just { head: Close, tail: after } -> Right { move: r.move, rest: after }
      _ -> Left "missing ')'"
  Just { head: Word "for", tail } -> case uncons tail of
    Just { head: Word n, tail: after } -> do
      bars <- int n
      when (bars < 1) (Left "for needs at least 1 bar")
      r <- term after
      Right { move: For bars r.move, rest: r.rest }
    _ -> Left "for BARS (MOVE)"
  Just { head: Word "heads", tail }
    | Just { head: Quoted p, tail: after } <- uncons tail -> Right { move: Gestures [ SetHeadsPattern (Just p) ], rest: after }
    | Just { head: Word "off", tail: after } <- uncons tail -> Right { move: Gestures [ SetHeadsPattern Nothing ], rest: after }
  Just { head: Word "harmony", tail } -> case uncons tail of
    Just { head: Quoted p, tail: after } -> Right { move: Gestures [ SetHarmony (Just p) ], rest: after }
    Just { head: Word "off", tail: after } -> Right { move: Gestures [ SetHarmony Nothing ], rest: after }
    _ -> Left "harmony takes a pattern in quotes (\"<c'maj7 a'min7>/2\") or off"
  Just { head: Word "scale", tail } -> case uncons tail of
    Just { head: Quoted p, tail: after } -> Right { move: Gestures [ SetScalePattern (Just p) ], rest: after }
    Just { head: Word "off", tail: after } -> Right { move: Gestures [ SetScalePattern Nothing ], rest: after }
    _ -> Left "scale takes a pattern of scale names in quotes (\"<dorian mixolydian>/4\") or off"
  Just { head: Word "outscale", tail } -> case uncons tail of
    Just { head: Quoted p, tail: after } -> case uncons after of
      Just { head: Word w, tail: after' } | Right pc <- pitchClass w ->
        Right { move: Gestures [ SetOutScale (Just p) pc ], rest: after' }
      _ -> Right { move: Gestures [ SetOutScale (Just p) 0 ], rest: after }
    Just { head: Word "off", tail: after } -> Right { move: Gestures [ SetOutScale Nothing 0 ], rest: after }
    _ -> Left "outscale takes a pattern of scale names in quotes and a root (\"<dorian lydian>/4\" d), or off"
  Just { head: Word name, tail } ->
    let
      args = wordsWhile tail
    in
      verb name args <#> \is -> { move: Gestures is, rest: drop (length args) tail }
  Just _ -> Left "expected a verb"
  Nothing -> Left ("expected a verb (try " <> String.joinWith ", " verbs <> ")")
  where
  wordsWhile xs = case uncons xs of
    Just { head: Word w, tail } | w /= "for" -> cons w (wordsWhile tail)
    _ -> []

seqOf :: Array Move -> Move
seqOf ms = case ms of
  [ m ] -> m
  _ -> Sequence (filter (const true) ms)
