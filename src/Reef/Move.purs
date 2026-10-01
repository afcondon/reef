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
module Reef.Move
  ( Move(..)
  , parse
  , inputsOf
  , Scheduled
  , schedule
  , verbs
  ) where

import Prelude

import Data.Array (catMaybes, concatMap, cons, drop, filter, find, head, length, mapWithIndex, null, reverse, snoc, takeWhile, uncons, (!!))
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Reef.Gen (GenKind(..))
import Reef.Input (Input(..), SimState)
import Reef.Odonus (knobMax)

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
    SetHeadPattern i _ -> catMaybes [ (\h -> SetHeadPattern i h.patternIx) <$> heads !! i ]
    SetHeadTransp i _ -> catMaybes [ (\h -> SetHeadTransp i h.transp) <$> heads !! i ]
    SetGatePct _ -> [ SetGatePct s.odo.gatePct ]
    SetHarmony _ -> [ SetHarmony s.odo.harmony ]
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
  , "offset N N N N"
  , "harmony \"PATTERN\" | off"
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
  "offset", ns | not (null ns) -> perHead SetHeadOffset ns
  _, _ ->
    if isVerb name then Left ("'" <> name <> "' takes: " <> usage name)
    else Left ("no verb '" <> name <> "' (try " <> String.joinWith ", " verbs <> ")")
  where
  one f v = [ f v ]
  perHead f ns = mapWithIndex f <$> traverseInts ns
  isVerb n = find (\v -> firstWord v == n) verbs /= Nothing
  usage n = fromMaybe n (find (\v -> firstWord v == n) verbs)
  firstWord v = fromMaybe v (head (String.split (String.Pattern " ") v))

int :: String -> Either String Int
int w = case Int.fromString w of
  Just n -> Right n
  Nothing -> Left ("'" <> w <> "' is not a whole number")

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
    body = case String.stripPrefix (String.Pattern "odonus") (String.trim src) of
      Just rest -> fromMaybe rest (String.stripPrefix (String.Pattern "$") (String.trim rest))
      Nothing -> src
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
  Just { head: Word "harmony", tail } -> case uncons tail of
    Just { head: Quoted p, tail: after } -> Right { move: Gestures [ SetHarmony (Just p) ], rest: after }
    Just { head: Word "off", tail: after } -> Right { move: Gestures [ SetHarmony Nothing ], rest: after }
    _ -> Left "harmony takes a pattern in quotes (\"<c'maj7 a'min7>/2\") or off"
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
