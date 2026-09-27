-- | **A Conspicillum scene as one line**, the way Tidal writes a pattern:
-- |
-- | ```
-- | s "fd-beat-bar" # walk 0.2 0.1 0.1 # grid 8 # swing 0.62
-- |   # fix snare 0.5 (pshift 1.5) # fix kick 0.5 (rsnpitch "<36 36 39 31>")
-- |   # every 4 0 (speed -1)
-- | s "chord-hits-0924-171929" # samples "<4 7 5 0>" # warp repitch
-- | ```
-- |
-- | Terms are separated by `#` and each sets one thing; anything not said is
-- | the default, which is Sector's: sixteen grains a bar following the tape.
-- | `print` writes the canonical line, leaving out every default, so a preset
-- | can be shown as the line that makes it and `parse (print l) == l`.
-- |
-- | What a line does NOT carry: the corpus (it names a set; whoever runs the
-- | line has the corpora), the query, and the warp's ratio, which is the rig's
-- | tempo and so belongs to the moment rather than to the scene.
-- |
-- | Vocabulary, in the order `print` writes it:
-- |
-- | - `s "set"`, `s "set:4"` — the set, and one sample of it (else the whole)
-- | - `grains 16`, `euclid 5 8`, `onsets "0 0.375 0.5"` — where grains land
-- | - `sustain`, `position`, `spray`, `follow` — the cloud
-- | - `walk jump hold home`, `grid n`, `reach n` — the walk
-- | - `swing play`, `tapeswing x`, `swinggrid n`
-- | - `bars n`, `order "0 1 1 0"`, `samples "<4 7 5 0>"`, `steps "~ ~ 5?0.4"`
-- | - `warp repitch|gap|leak`
-- | - `speed`, `gain`, `pan`, `accelerate`
-- | - any effect by its name (`lpf 800`, `rsnpitch 36`), any chain setting
-- |   (`room 0.3`, `delay 0.5`), `sendA 0.8`, `sendB 0.8`
-- | - rules: `every 4 0 (op x)`, `sometimesBy 0.2 (op x)`, `always (op x)`,
-- |   `fix snare 0.5 (op x)`; `x` may be `"a b c"` (one per firing) or
-- |   `"<a b c>"` (one per bar), as in Tidal
-- | - `seed n`
module Reef.Conspicillum.Notation
  ( Line
  , defaultLine
  , parse
  , print
  , opNames
  , OnsetLayout(..)
  , onsetLayout
  , stepsText
  ) where

import Prelude

import Data.Array (drop, elemIndex, filter, findIndex, foldM, index, length, mapWithIndex, null, range, snoc, take, uncons)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (intercalate)
import Data.Int (floor, round, toNumber)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.String as String
import Data.String.CodeUnits as SCU
import Reef.Conspicillum.Decimal (trimmed)
import Reef.Conspicillum.Cloud (Chain, Fx, Kind(..), Rule, Spec, WarpMode(..), When(..), noChain, noFx, noSteps, noSwing, noWalk, noWarp, oneBar)
import Reef.Conspicillum.Protocol (fromWireRule, toWireRule)

type Line = { set :: String, n :: Maybe Int, seed :: Int, spec :: Spec }

-- | Send A is orbit 10 (outputs 3/4), send B orbit 11 (5/6), both dry, both at
-- | 0.8: the rig's standard pair, so a line only mentions them to change them.
defaultLine :: Line
defaultLine =
  { set: ""
  , n: Nothing
  , seed: 1
  , spec:
      { onsets: even 16
      , cloud: { sustain: 0.125, position: 0.0, spray: 0.0, follow: 1.0 }
      , walk: noWalk
      , swing: noSwing
      , tape: oneBar
      , steps: noSteps
      , warp: noWarp
      , rules: []
      , speed: 1.0
      , gain: 1.0
      , pan: 0.5
      , accelerate: 0.0
      , fx: noFx
      , chain: noChain
      , sends:
          [ { chain: noChain { orbit = 10 }, level: 0.8 }
          , { chain: noChain { orbit = 11 }, level: 0.8 }
          ]
      }
  }

-- | The rule ops, by wire number: the name a line uses is the index here.
opNames :: Array String
opNames =
  [ "speed", "gain", "length", "pan", "accelerate", "shape", "crush", "coarse"
  , "lpf", "hpf", "bpf", "res", "vowel", "pshift", "tremolo", "phaser"
  , "genv", "gtilt", "gplat", "atk", "hold", "rel", "curve"
  , "rsnpitch", "rsndecay", "rsnbright", "rsnmix", "rsnmodel"
  , "shift", "ratchet", "send"
  ]

-- ── onsets ───────────────────────────────────────────────────────────────────

even :: Int -> Array Number
even k = map (\i -> toNumber i / toNumber k) (range 0 (k - 1))

-- | `k` hits spread over `n` steps, as the page lays them out.
euclid :: Int -> Int -> Array Number
euclid k n =
  map (\i -> toNumber i / toNumber n)
    (filter (\i -> floor (toNumber (i * k) / toNumber n) /= floor (toNumber ((i - 1) * k) / toNumber n))
       (range 0 (n - 1)))

-- ── tokens ───────────────────────────────────────────────────────────────────

data Tok = TWord String | TStr String | THash | TOpen | TClose | TGroup (Array Tok)

derive instance eqTok :: Eq Tok

-- | A term's arguments, with each `( … )` made one argument.
group :: Array Tok -> Either String (Array Tok)
group ts = go ts []
  where
  go xs acc = case uncons xs of
    Nothing -> Right acc
    Just { head: TOpen, tail } ->
      case Array.findIndex (_ == TClose) tail of
        Nothing -> Left "a ( is not closed"
        Just j -> go (drop (j + 1) tail) (snoc acc (TGroup (take j tail)))
    Just { head: TClose } -> Left "a ) with no ("
    Just { head, tail } -> go tail (snoc acc head)

tokenize :: String -> Either String (Array Tok)
tokenize src = go 0 []
  where
  len = SCU.length src
  at i = SCU.charAt i src
  go i acc
    | i >= len = Right acc
    | otherwise = case at i of
        Just c | c == ' ' || c == '\n' || c == '\t' || c == '\r' -> go (i + 1) acc
        Just '#' -> go (i + 1) (snoc acc THash)
        Just '(' -> go (i + 1) (snoc acc TOpen)
        Just ')' -> go (i + 1) (snoc acc TClose)
        Just '"' -> case String.indexOf (String.Pattern "\"") (SCU.drop (i + 1) src) of
          Nothing -> Left "a string is not closed"
          Just j -> go (i + j + 2) (snoc acc (TStr (SCU.take j (SCU.drop (i + 1) src))))
        _ -> let j = wordEnd i in go j (snoc acc (TWord (SCU.take (j - i) (SCU.drop i src))))
  wordEnd i = case at i of
    Just c | not (c == ' ' || c == '\n' || c == '\t' || c == '\r' || c == '#' || c == '(' || c == ')' || c == '"') -> wordEnd (i + 1)
    _ -> i

-- | The top level, split at `#`.
terms :: Array Tok -> Array (Array Tok)
terms = Array.foldl (\acc t -> if t == THash then snoc acc [] else addLast t acc) [ [] ]
  where
  addLast t acc = case Array.unsnoc acc of
    Just { init, last } -> snoc init (snoc last t)
    Nothing -> [ [ t ] ]

-- ── parsing ──────────────────────────────────────────────────────────────────

parse :: String -> Either String Line
parse src = do
  toks <- tokenize src
  foldM term defaultLine (filter (not <<< null) (terms toks))

num :: String -> Tok -> Either String Number
num what = case _ of
  TWord w -> note (what <> " wants a number, not " <> w) (Number.fromString w)
  _ -> Left (what <> " wants a number")

int :: String -> Tok -> Either String Int
int what t = do
  x <- num what t
  if toNumber (round x) == x then Right (round x) else Left (what <> " wants a whole number")

str :: String -> Tok -> Either String String
str what = case _ of
  TStr s -> Right s
  _ -> Left (what <> " wants a \"quoted\" pattern")

numbers :: String -> Either String (Array Number)
numbers s =
  let ws = filter (_ /= "") (String.split (String.Pattern " ") (strip s))
  in Array.foldM (\acc w -> maybe (Left ("not a number: " <> w)) (Right <<< snoc acc) (Number.fromString w)) [] ws
  where
  strip = String.replaceAll (String.Pattern "<") (String.Replacement " ")
    >>> String.replaceAll (String.Pattern ">") (String.Replacement " ")
    >>> String.replaceAll (String.Pattern ",") (String.Replacement " ")

ints :: String -> Either String (Array Int)
ints s = numbers s >>= Array.foldM (\acc x -> if toNumber (round x) == x then Right (snoc acc (round x)) else Left "whole numbers only") []

term :: Line -> Array Tok -> Either String Line
term l toks = case uncons toks of
  Nothing -> Right l
  Just { head: TWord w, tail } -> group tail >>= word w
  _ -> Left "a term starts with a word"
  where
  sp = l.spec
  withSpec f = Right l { spec = f sp }
  word w args = case w of
    "s" -> one w \a -> do
      s <- str w a
      case String.split (String.Pattern ":") s of
        [ name, k ] -> do
          n <- note ("s \"" <> s <> "\": the sample after : is a number") (Int.fromString k)
          Right l { set = name, n = Just n }
        _ -> Right l { set = s, n = Nothing }
    "seed" -> one w \a -> int w a <#> \k -> l { seed = k }
    "grains" -> one w \a -> int w a >>= \k -> withSpec _ { onsets = even k }
    "euclid" -> case args of
      [ a, b ] -> do
        k <- int w a
        n <- int w b
        withSpec _ { onsets = euclid k n }
      _ -> Left "euclid takes hits and steps"
    "onsets" -> one w \a -> str w a >>= numbers >>= \xs -> withSpec _ { onsets = xs }
    "sustain" -> one w \a -> num w a >>= \x -> withSpec _ { cloud { sustain = x } }
    "position" -> one w \a -> num w a >>= \x -> withSpec _ { cloud { position = x } }
    "spray" -> one w \a -> num w a >>= \x -> withSpec _ { cloud { spray = x } }
    "follow" -> one w \a -> num w a >>= \x -> withSpec _ { cloud { follow = x } }
    "walk" -> case args of
      [ a, b, c ] -> do
        j <- num w a
        h <- num w b
        o <- num w c
        withSpec _ { walk { jump = j, hold = h, home = o } }
      _ -> Left "walk takes jump, hold and home"
    "grid" -> one w \a -> int w a >>= \k -> withSpec _ { walk { grid = k } }
    "reach" -> one w \a -> int w a >>= \k -> withSpec _ { walk { reach = k } }
    "swing" -> one w \a -> num w a >>= \x -> withSpec _ { swing { play = x } }
    "tapeswing" -> one w \a -> num w a >>= \x -> withSpec _ { swing { tape = x } }
    "swinggrid" -> one w \a -> int w a >>= \k -> withSpec _ { swing { grid = k } }
    "bars" -> one w \a -> int w a >>= \k -> withSpec _ { tape { bars = k } }
    "order" -> one w \a -> str w a >>= ints >>= \xs -> withSpec _ { tape { order = xs } }
    "samples" -> one w \a -> str w a >>= ints >>= \xs -> withSpec _ { tape { samples = xs } }
    "steps" -> one w \a -> str w a >>= stepTable >>= \st -> withSpec _ { steps = st }
    "warp" -> one w \a -> case a of
      TWord "repitch" -> withSpec _ { warp { mode = Repitch } }
      TWord "gap" -> withSpec _ { warp { mode = Gap } }
      TWord "leak" -> withSpec _ { warp { mode = Leak } }
      _ -> Left "warp is repitch, gap or leak"
    "speed" -> one w \a -> num w a >>= \x -> withSpec _ { speed = x }
    "gain" -> one w \a -> num w a >>= \x -> withSpec _ { gain = x }
    "pan" -> one w \a -> num w a >>= \x -> withSpec _ { pan = x }
    "accelerate" -> one w \a -> num w a >>= \x -> withSpec _ { accelerate = x }
    "sendA" -> one w \a -> num w a >>= \x -> withSpec \s -> s { sends = setLevel 0 x s.sends }
    "sendB" -> one w \a -> num w a >>= \x -> withSpec \s -> s { sends = setLevel 1 x s.sends }
    "every" -> case args of
      [ a, b, o ] -> ruleOf o \_ -> Every <$> int w a <*> int w b
      _ -> Left "every takes n, k and an (op x)"
    "sometimesBy" -> case args of
      [ a, o ] -> ruleOf o \_ -> Chance <$> num w a
      _ -> Left "sometimesBy takes a chance and an (op x)"
    "always" -> case args of
      [ o ] -> ruleOf o \_ -> Right Always
      _ -> Left "always takes an (op x)"
    "fix" -> case args of
      [ TWord k, a, o ] -> ruleOf o \_ -> do
        kind <- case k of
          "kick" -> Right Kick
          "snare" -> Right Snare
          "hat" -> Right Hat
          _ -> Left "fix is for kick, snare or hat"
        Hit kind <$> num w a
      _ -> Left "fix takes kick|snare|hat, a threshold and an (op x)"
    _ -> case findIndex (\f -> f.name == w) fxFields of
      Just i -> one w \a -> num w a >>= \x -> withSpec \s -> s { fx = (unsafeIx i fxFields).set x s.fx }
      Nothing -> case findIndex (\f -> f.name == w) chainFields of
        Just i -> one w \a -> num w a >>= \x -> withSpec \s -> s { chain = (unsafeIx i chainFields).set x s.chain }
        Nothing -> Left ("unknown word: " <> w)
    where
    one what f = case args of
      [ a ] -> f a
      _ -> Left (what <> " takes one value")
    ruleOf o whenOf = do
      wh <- whenOf unit
      r <- opOf o
      Right l { spec = sp { rules = snoc sp.rules (r wh) } }

-- | `(pshift 1.5)`, `(pan "0.2 0.8")`, `(rsnpitch "<36 36 39 31>")`: an op by
-- | its name, with an amount or a sequence of them. A sequence's own amount is
-- | never read (`setAmount` replaces it), so it is 0.
opOf :: Tok -> Either String (When -> Rule)
opOf = case _ of
  TGroup [ TWord name, a ] -> do
    code <- note ("unknown op: " <> name) (elemIndex name opNames)
    seq <- case a of
      TStr s -> numbers s <#> \vs ->
        { amount: 0.0, values: vs, step: if String.contains (String.Pattern "<") s then 1 else 0 }
      TWord w -> note (name <> " wants a number, not " <> w) (Number.fromString w) <#> \x ->
        { amount: x, values: [], step: 0 }
      _ -> Left (name <> " wants an amount")
    Right \wh ->
      let r = fromWireRule { when: 0, everyN: 0, everyK: 0, chance: 0.0, op: code
                           , amount: seq.amount, values: seq.values, step: seq.step }
      in r { when = wh }
  _ -> Left "a rule's op is written (name amount)"

-- | `~ ~ 5?0.4 ~ 2` — one token a step; `~` stays.
stepTable :: String -> Either String { grid :: Int, to :: Array Int, p :: Array Number }
stepTable s = do
  let ws = filter (_ /= "") (String.split (String.Pattern " ") s)
  cells <- Array.foldM (\acc w -> cell w <#> snoc acc) [] ws
  pure if null cells then noSteps
       else { grid: length cells, to: map _.to cells, p: map _.p cells }
  where
  cell w = case String.split (String.Pattern "?") w of
    [ "~" ] -> Right { to: -1, p: 1.0 }
    [ t ] -> note ("step " <> w) (Int.fromString t) <#> \k -> { to: k, p: 1.0 }
    [ t, chance ] -> do
      k <- if t == "~" then Right (-1) else note ("step " <> w) (Int.fromString t)
      p <- note ("step " <> w) (Number.fromString chance)
      Right { to: k, p }
    _ -> Left ("step " <> w)

setLevel :: Int -> Number -> Array { chain :: Chain, level :: Number } -> Array { chain :: Chain, level :: Number }
setLevel i x ss = fromMaybe ss (Array.modifyAt i (_ { level = x }) ss)

unsafeIx :: forall a. Int -> Array a -> a
unsafeIx i xs = case index xs i of
  Just x -> x
  Nothing -> unsafeIx 0 xs

-- ── fields by name ───────────────────────────────────────────────────────────

type Field r = { name :: String, get :: r -> Number, set :: Number -> r -> r }

fxFields :: Array (Field Fx)
fxFields =
  [ { name: "shape", get: _.shape, set: \x r -> r { shape = x } }
  , { name: "crush", get: _.crush, set: \x r -> r { crush = x } }
  , { name: "coarse", get: _.coarse, set: \x r -> r { coarse = x } }
  , { name: "lpf", get: _.lpf, set: \x r -> r { lpf = x } }
  , { name: "hpf", get: _.hpf, set: \x r -> r { hpf = x } }
  , { name: "bpf", get: _.bpf, set: \x r -> r { bpf = x } }
  , { name: "res", get: _.res, set: \x r -> r { res = x } }
  , { name: "vowel", get: _.vowel, set: \x r -> r { vowel = x } }
  , { name: "pshift", get: _.pshift, set: \x r -> r { pshift = x } }
  , { name: "tremolo", get: _.tremolo, set: \x r -> r { tremolo = x } }
  , { name: "tremdepth", get: _.tremdepth, set: \x r -> r { tremdepth = x } }
  , { name: "phaser", get: _.phaser, set: \x r -> r { phaser = x } }
  , { name: "phdepth", get: _.phdepth, set: \x r -> r { phdepth = x } }
  , { name: "genv", get: _.genv, set: \x r -> r { genv = x } }
  , { name: "gtilt", get: _.gtilt, set: \x r -> r { gtilt = x } }
  , { name: "gplat", get: _.gplat, set: \x r -> r { gplat = x } }
  , { name: "atk", get: _.atk, set: \x r -> r { atk = x } }
  , { name: "hold", get: _.hold, set: \x r -> r { hold = x } }
  , { name: "rel", get: _.rel, set: \x r -> r { rel = x } }
  , { name: "curve", get: _.curve, set: \x r -> r { curve = x } }
  , { name: "rsnpitch", get: _.rsnpitch, set: \x r -> r { rsnpitch = x } }
  , { name: "rsndecay", get: _.rsndecay, set: \x r -> r { rsndecay = x } }
  , { name: "rsnbright", get: _.rsnbright, set: \x r -> r { rsnbright = x } }
  , { name: "rsnmix", get: _.rsnmix, set: \x r -> r { rsnmix = x } }
  , { name: "rsnmodel", get: _.rsnmodel, set: \x r -> r { rsnmodel = x } }
  ]

chainFields :: Array (Field Chain)
chainFields =
  [ { name: "orbit", get: _.orbit >>> toNumber, set: \x r -> r { orbit = round x } }
  , { name: "room", get: _.room, set: \x r -> r { room = x } }
  , { name: "size", get: _.size, set: \x r -> r { size = x } }
  , { name: "dry", get: _.dry, set: \x r -> r { dry = x } }
  , { name: "delay", get: _.delay, set: \x r -> r { delay = x } }
  , { name: "delaytime", get: _.delaytime, set: \x r -> r { delaytime = x } }
  , { name: "delayfeedback", get: _.delayfeedback, set: \x r -> r { delayfeedback = x } }
  , { name: "lock", get: _.lock, set: \x r -> r { lock = x } }
  , { name: "leslie", get: _.leslie, set: \x r -> r { leslie = x } }
  , { name: "lrate", get: _.lrate, set: \x r -> r { lrate = x } }
  , { name: "lsize", get: _.lsize, set: \x r -> r { lsize = x } }
  , { name: "grsn", get: _.grsn, set: \x r -> r { grsn = x } }
  , { name: "grsnpitch", get: _.grsnpitch, set: \x r -> r { grsnpitch = x } }
  , { name: "grsndecay", get: _.grsndecay, set: \x r -> r { grsndecay = x } }
  , { name: "grsnbright", get: _.grsnbright, set: \x r -> r { grsnbright = x } }
  ]

-- ── printing ─────────────────────────────────────────────────────────────────

-- | A number as a person would write it: `1`, not `1.0`; `0.62`, to six
-- | places at most, identically on both runtimes (see `Decimal`).
fmt :: Number -> String
fmt = trimmed 6

q :: String -> String
q s = "\"" <> s <> "\""

print :: Line -> String
print l = intercalate " # " (filter (_ /= "") parts)
  where
  sp = l.spec
  d = defaultLine.spec
  num1 name x x0 = if x == x0 then "" else name <> " " <> fmt x
  int1 name k k0 = if k == k0 then "" else name <> " " <> show k
  parts =
    [ "s " <> q (l.set <> maybe "" (\k -> ":" <> show k) l.n) ]
      <> [ onsetsTerm ]
      <> [ num1 "sustain" sp.cloud.sustain d.cloud.sustain
         , num1 "position" sp.cloud.position d.cloud.position
         , num1 "spray" sp.cloud.spray d.cloud.spray
         , num1 "follow" sp.cloud.follow d.cloud.follow
         , if sp.walk.jump == 0.0 && sp.walk.hold == 0.0 && sp.walk.home == 0.0 then ""
           else "walk " <> fmt sp.walk.jump <> " " <> fmt sp.walk.hold <> " " <> fmt sp.walk.home
         , int1 "grid" sp.walk.grid d.walk.grid
         , int1 "reach" sp.walk.reach d.walk.reach
         , num1 "swing" sp.swing.play d.swing.play
         , num1 "tapeswing" sp.swing.tape d.swing.tape
         , int1 "swinggrid" sp.swing.grid d.swing.grid
         , int1 "bars" sp.tape.bars d.tape.bars
         , if null sp.tape.order then "" else "order " <> q (intercalate " " (map show sp.tape.order))
         , if null sp.tape.samples then "" else "samples " <> q ("<" <> intercalate " " (map show sp.tape.samples) <> ">")
         , if null sp.steps.to then "" else "steps " <> q (stepsText sp.steps)
         , case sp.warp.mode of
             Gap -> ""
             Repitch -> "warp repitch"
             Leak -> "warp leak"
         , num1 "speed" sp.speed d.speed
         , num1 "gain" sp.gain d.gain
         , num1 "pan" sp.pan d.pan
         , num1 "accelerate" sp.accelerate d.accelerate
         ]
      <> map (\f -> num1 f.name (f.get sp.fx) (f.get d.fx)) fxFields
      <> map (\f -> num1 f.name (f.get sp.chain) (f.get d.chain)) chainFields
      <> [ num1 "sendA" (level 0 sp) (level 0 d)
         , num1 "sendB" (level 1 sp) (level 1 d)
         ]
      <> map ruleText sp.rules
      <> [ if l.seed == defaultLine.seed then "" else "seed " <> show l.seed ]
  level i s = maybe 0.0 _.level (index s.sends i)
  onsetsTerm
    | sp.onsets == d.onsets = ""
    | otherwise = case onsetLayout sp.onsets of
        EvenGrains k -> "grains " <> show k
        Euclidean k n -> "euclid " <> show k <> " " <> show n
        Placed os -> "onsets " <> q (intercalate " " (map fmt os))

-- | How a cycle's onsets are laid out, as the shortest thing that makes them:
-- | `k` even grains, `k` hits spread euclidean over `n` steps (tried against
-- | every grid up to 64), or placed by hand.
data OnsetLayout = EvenGrains Int | Euclidean Int Int | Placed (Array Number)

derive instance eqOnsetLayout :: Eq OnsetLayout

onsetLayout :: Array Number -> OnsetLayout
onsetLayout os
  | os == even (length os) = EvenGrains (length os)
  | otherwise = case euclidOf of
      Just { k, n } -> Euclidean k n
      Nothing -> Placed os
  where
  euclidOf = Array.head do
    n <- range 1 64
    k <- range 1 n
    if euclid k n == os then [ { k, n } ] else []

stepsText :: { grid :: Int, to :: Array Int, p :: Array Number } -> String
stepsText st = intercalate " " (mapWithIndex cell st.to)
  where
  cell i t =
    let p = fromMaybe 1.0 (index st.p i)
        base = if t < 0 then "~" else show t
    in if p == 1.0 then base else base <> "?" <> fmt p

ruleText :: Rule -> String
ruleText r =
  let w = toWireRule r
      name = fromMaybe "?" (index opNames w.op)
      amount
        | null w.values = fmt w.amount
        | w.step == 1 = q ("<" <> intercalate " " (map fmt w.values) <> ">")
        | otherwise = q (intercalate " " (map fmt w.values))
      op = "(" <> name <> " " <> amount <> ")"
  in case r.when of
    Always -> "always " <> op
    Every n k -> "every " <> show n <> " " <> show k <> " " <> op
    Chance p -> "sometimesBy " <> fmt p <> " " <> op
    Hit kind t -> "fix " <> kindName kind <> " " <> fmt t <> " " <> op
  where
  kindName = case _ of
    Kick -> "kick"
    Snare -> "snare"
    Hat -> "hat"
