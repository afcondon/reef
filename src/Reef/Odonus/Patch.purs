-- | **Odonus as text, for both runtimes**: the patch (its own settings) and
-- | `odonusNow` (what a patch leaves out at one instant: playhead phases, the
-- | generators' seed, what it was quantising to and why). One source, so the
-- | rig reads what the page writes: a mark's code evaluated in Limulus
-- | (`odonus $ odonusPatch "live" { … }`, `odonus $ odonusNow { … }`)
-- | applies on the BEAM voice as a move and the page follows, as for any move
-- | (docs/kb/plans/the-deck.md, step 2).
-- |
-- | Moved from Triggerfish's `Triggerfish.Odonus.Lepidoptera` (2026-10-03),
-- | which now re-exports it. The parser is written by hand over a small token
-- | list rather than with `parsing`, because the purerl package set has only
-- | the old `Text.Parsing.Parser` API, so no one parser compiles on both.
-- | Whitespace, newlines included, is insignificant, so a block Limulus sends
-- | whole, or `bodyOf` joins onto one line, reads the same.
-- |
-- | Format and history as before the move: print order is canonical and
-- | fixed, and the parser reads that order. `span` (added 2026-10-03) is
-- | optional on read. A legacy quantize (`chords pcs [...] every N`, `vetula
-- | N`) still reads, as the pattern meaning the same, or as the scale.
module Reef.Odonus.Patch
  ( OdonusPatch
  , printPatch
  , parsePatch
  , Phase
  , Sounding
  , OdonusNow
  , printNow
  , parseNow
  , reconcileGen
  , withPhases
  , quote
  , trimText
  ) where

import Prelude

import Data.Array (concatMap, find, findIndex, length, mapWithIndex, range, snoc, uncons, unsnoc, (!!))
import Data.Either (Either(..), hush)
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.Ord (abs)
import Data.String.CodeUnits as CU
import Data.String.Common (joinWith, toLower)
import Reef.Gen (GenKind(..), GenSource, genDefaultAmt, genDefaultRate, genKinds)
import Reef.Odonus (Head, Odonus, defaultOdonus, patternLibrary, setHarmony, setScalePattern, speedOf, speedTable, unitRateIx, clockOf)
import Reef.Scale (Distribution(..), normaliseIvls, rootName, rootNames, scaleTypes)
import Reef.Vetula.Harmony (clockHarmony)

-- | The whole authored setup: the model, plus the generators and feel that
-- | live beside it on the page.
type OdonusPatch =
  { name :: String
  , odo :: Odonus
  , gen :: Array GenSource
  , genSpread :: Number
  , genBias :: Number
  , swing :: Number          -- 0..0.6 (fraction of a step the off-beats lag)
  , velHumanize :: Int
  }

-- ---------------------------------------------------------------------------
-- Print
-- ---------------------------------------------------------------------------

printPatch :: OdonusPatch -> String
printPatch p =
  let
    o = p.odo
    cellInts f = ints (map f o.cells)
    cellBools f = bools (map f o.cells)
    pct x = show (Int.round (x * 100.0))
    -- by name when reef's table has one (Reef.Scale.scaleTypes), else as steps
    scaleText ivls = maybe (ints ivls) _.name
      (find (\t -> normaliseIvls t.intervals == normaliseIvls ivls) scaleTypes)
  in
    joinWith "\n"
      [ "odonusPatch " <> quote p.name
      , "  { scale: " <> rootName o.rootPc <> " " <> scaleText (fromMaybe o.scaleIvls o.scaleHeld)
          <> maybe "" (\sp -> " " <> quote sp) o.scalePattern
      , "  , distribution: " <> show o.dist
      , "  , octave: " <> show o.octaveShift
      , "  , span: " <> show o.span
      , "  , scalarTransp: " <> show o.degShift
      , "  , gate: " <> show o.gatePct
      , "  , quantize: " <> maybe "scale" (\h -> "harmony " <> quote h) o.harmony
      , "  , swing: " <> pct p.swing
      , "  , velHumanize: " <> show p.velHumanize
      , "  , clock: " <> decimal (clockOf o)
      , "  , marbles: { spread: " <> pct p.genSpread <> ", bias: " <> pct p.genBias <> " }"
      , "  , notes: " <> cellInts _.note
      , "  , gates: " <> cellBools _.gate
      , "  , skips: " <> cellBools _.skip
      , "  , glides: " <> cellBools _.glide
      , "  , durs: " <> cellInts _.dur
      , "  , ratchets: " <> cellInts _.ratchet
      , "  , vels: " <> cellInts _.vel
      , "  , heads:"
      , "      [ " <> joinWith "\n      , " (map printHead o.heads) <> " ]"
      , "  , gen:"
      , "      [ " <> joinWith "\n      , " (map printGen p.gen) <> " ]"
      , "  }"
      ]

printHead :: Head -> String
printHead hd =
  "head " <> patternSlug hd.patternIx <> " " <> decimal (speedOf hd) <> " " <> dirSlug hd.direction
    <> " transp " <> show hd.transp <> " off " <> show hd.offset <> " len " <> show hd.len
    <> " euclid " <> show hd.pulses <> " " <> show hd.esteps
    -- clocked by its notes' lengths; left out on the step clock, so older
    -- patches read and print as they did
    <> (if hd.clock == 1 then " clock dur" else "")
    <> (if hd.mute then " mute" else "")

printGen :: GenSource -> String
printGen g =
  "source " <> genSlug g.kind <> " rate " <> show g.rate <> " amt " <> show g.amt
    <> (if g.on then " on" else " off")

-- | A string in double quotes, `\\`, `"` and newlines escaped: written by
-- | hand rather than by `show`, which escapes differently on the BEAM
-- | (purerl's escapes `'`), so both runtimes print the same text.
quote :: String -> String
quote t = "\"" <> CU.fromCharArray (concatMap esc (CU.toCharArray t)) <> "\""
  where
  esc c
    | c == '\\' = [ '\\', '\\' ]
    | c == '"' = [ '\\', '"' ]
    | c == '\n' = [ '\\', 'n' ]
    | otherwise = [ c ]

-- | A speed as a plain decimal (`1.0`, `0.5`, `0.125`), to three places:
-- | purerl's `show` on a Number prints `1.00000000000000000000e+00`.
decimal :: Number -> String
decimal x =
  let
    m = Int.round (x * 1000.0)
    whole = m / 1000
    frac = m `mod` 1000
    digits = CU.drop 1 (show (1000 + frac))   -- three digits, zero-padded
    trimmed = dropZeros digits
  in
    show whole <> "." <> (if trimmed == "" then "0" else trimmed)
  where
  dropZeros d = if CU.takeRight 1 d == "0" then dropZeros (CU.dropRight 1 d) else d

-- | Whitespace off both ends, newlines included: purerl's `trim` is a regex
-- | whose `.` stops at a newline, so it leaves a multi-line string untrimmed.
trimText :: String -> String
trimText = CU.fromCharArray <<< dropEnd <<< dropStart <<< CU.toCharArray
  where
  ws c = c == ' ' || c == '\n' || c == '\t' || c == '\r'
  dropStart cs = case uncons cs of
    Just { head, tail } | ws head -> dropStart tail
    _ -> cs
  dropEnd cs = case unsnoc cs of
    Just { init, last } | ws last -> dropEnd init
    _ -> cs

ints :: Array Int -> String
ints xs = "[ " <> joinWith ", " (map show xs) <> " ]"

bools :: Array Boolean -> String
bools xs = "[ " <> joinWith ", " (map (\b -> if b then "T" else "F") xs) <> " ]"

-- ---------------------------------------------------------------------------
-- The instant: odonusNow
-- ---------------------------------------------------------------------------

-- | Where one head's playhead was.
-- | `hold`: on the duration clock, the ticks left on its cell (0 otherwise).
type Phase = { cursor :: Int, seqPos :: Int, accumulator :: Int, pendStep :: Int, etick :: Int, hold :: Int }

-- | What Odonus was quantising to, as pitch classes.
type Sounding = { root :: Int, scale :: Array Int, chord :: Maybe (Array Int) }

-- | What a patch leaves out, at one instant. `step`, `tempo`, `sounding`,
-- | `routes` and `feeds` are context, recorded to be read; recalling applies
-- | the phases, the seed and the freeze.
type OdonusNow =
  { step :: Int
  , tempo :: Int
  , seed :: Number
  , frozen :: Boolean
  , phases :: Array Phase
  , sounding :: Sounding
  , routes :: Maybe String
  , feeds :: Maybe String
  }

printNow :: OdonusNow -> String
printNow n =
  joinWith "\n"
    [ "odonusNow"
    , "  { step: " <> show n.step
    , "  , tempo: " <> show n.tempo
    , "  , seed: " <> show (Int.round n.seed)
    , "  , frozen: " <> (if n.frozen then "T" else "F")
    , "  , phases:"
    , "      [ " <> joinWith "\n      , " (map phase n.phases) <> " ]"
    , "  , sounding: { root: " <> rootName n.sounding.root
        <> ", scale: " <> ints n.sounding.scale
        <> ", chord: " <> maybe "none" ints n.sounding.chord <> " }"
    , "  , routes: " <> maybe "none" quote n.routes
    , "  , feeds: " <> maybe "none" quote n.feeds
    , "  }"
    ]
  where
  phase h = "{ cursor: " <> show h.cursor <> ", seqPos: " <> show h.seqPos
    <> ", acc: " <> show h.accumulator <> ", pend: " <> show h.pendStep
    <> ", etick: " <> show h.etick
    -- only on the duration clock, so earlier instants print as they did
    <> (if h.hold > 0 then ", hold: " <> show h.hold else "") <> " }"

-- | The heads with these phases, head by head; a head with none keeps its own.
withPhases :: Array Phase -> Odonus -> Odonus
withPhases ps o = o { heads = mapWithIndex set o.heads }
  where
  set i h = case ps !! i of
    Just p -> h { cursor = p.cursor, seqPos = p.seqPos, accumulator = p.accumulator, pendStep = p.pendStep, etick = p.etick, hold = p.hold }
    Nothing -> h

-- | Keep every source a patch saved and fill in any it predates with its
-- | default, off, in the canonical order.
reconcileGen :: Array GenSource -> Array GenSource
reconcileGen loaded =
  genKinds <#> \k -> case find (\g -> g.kind == k) loaded of
    Just g -> g
    Nothing -> { kind: k, on: false, rate: genDefaultRate k, amt: genDefaultAmt k }

-- ---------------------------------------------------------------------------
-- Tokens
-- ---------------------------------------------------------------------------

data Tok = Punct Char | Str String | Word String

derive instance Eq Tok

-- | `{ } [ ] , :` stand alone; a quoted string is one token (`\"` and `\\`
-- | escaped, as `show` writes them); anything else up to a space or one of
-- | those is a word (`C#`, `-12`, `0.75`, `fwd`, `T`).
tokenize :: String -> Either String (Array Tok)
tokenize src = go [] (CU.toCharArray src)
  where
  go acc cs = case uncons cs of
    Nothing -> Right acc
    Just { head: c, tail }
      | space c -> go acc tail
      | punct c -> go (snoc acc (Punct c)) tail
      | c == '"' -> do
          r <- quoted [] tail
          go (snoc acc (Str (CU.fromCharArray r.chars))) r.rest
      | otherwise ->
          let r = wordOf [ c ] tail
          in go (snoc acc (Word (CU.fromCharArray r.chars))) r.rest
  quoted chars cs = case uncons cs of
    Nothing -> Left "a string with no closing quote"
    Just { head: '\\', tail } -> case uncons tail of
      Just { head: 'n', tail: t } -> quoted (snoc chars '\n') t
      Just { head: e, tail: t } -> quoted (snoc chars e) t
      Nothing -> Left "a string with no closing quote"
    Just { head: '"', tail } -> Right { chars, rest: tail }
    Just { head: c, tail } -> quoted (snoc chars c) tail
  wordOf chars cs = case uncons cs of
    Just { head: c, tail } | not (space c || punct c || c == '"') -> wordOf (snoc chars c) tail
    _ -> { chars, rest: cs }
  space c = c == ' ' || c == '\n' || c == '\t' || c == '\r'
  punct c = c == '{' || c == '}' || c == '[' || c == ']' || c == ',' || c == ':'

-- ---------------------------------------------------------------------------
-- A small parser over tokens
-- ---------------------------------------------------------------------------

newtype P a = P (Array Tok -> Either String { val :: a, rest :: Array Tok })

runP :: forall a. P a -> Array Tok -> Either String { val :: a, rest :: Array Tok }
runP (P f) = f

instance Functor P where
  map f (P p) = P \ts -> map (\r -> r { val = f r.val }) (p ts)

instance Apply P where
  apply = ap

instance Applicative P where
  pure a = P \ts -> Right { val: a, rest: ts }

instance Bind P where
  bind (P p) k = P \ts -> case p ts of
    Left e -> Left e
    Right r -> runP (k r.val) r.rest

instance Monad P

failP :: forall a. String -> P a
failP e = P \_ -> Left e

-- | `a`, or if it fails, `b` from the same place.
orElse :: forall a. P a -> P a -> P a
orElse (P a) (P b) = P \ts -> case a ts of
  Left _ -> b ts
  r -> r

optionalP :: forall a. P a -> P (Maybe a)
optionalP p = orElse (Just <$> p) (pure Nothing)

next :: P Tok
next = P \ts -> case uncons ts of
  Just { head, tail } -> Right { val: head, rest: tail }
  Nothing -> Left "the text ended early"

peek :: P (Maybe Tok)
peek = P \ts -> Right { val: map _.head (uncons ts), rest: ts }

punctP :: Char -> P Unit
punctP c = next >>= case _ of
  Punct d | d == c -> pure unit
  _ -> failP ("expected '" <> CU.singleton c <> "'")

word :: P String
word = next >>= case _ of
  Word w -> pure w
  _ -> failP "expected a word"

kw :: String -> P Unit
kw k = word >>= \w -> if w == k then pure unit else failP ("expected '" <> k <> "', found '" <> w <> "'")

strP :: P String
strP = next >>= case _ of
  Str s -> pure s
  _ -> failP "expected a quoted string"

intP :: P Int
intP = word >>= \w -> maybe (failP ("expected a whole number, found '" <> w <> "'")) pure (Int.fromString w)

numP :: P Number
numP = word >>= \w -> maybe (failP ("expected a number, found '" <> w <> "'")) pure (Number.fromString w)

boolP :: P Boolean
boolP = word >>= case _ of
  "T" -> pure true
  "F" -> pure false
  w -> failP ("expected T or F, found '" <> w <> "'")

-- | `key: value`, then the comma between fields if there is one.
field :: forall a. String -> P a -> P a
field k p = do
  kw k
  punctP ':'
  v <- p
  _ <- optionalP (punctP ',')
  pure v

-- | `[ a, b, … ]`, possibly empty.
list :: forall a. P a -> P (Array a)
list p = do
  punctP '['
  peek >>= case _ of
    Just (Punct ']') -> punctP ']' *> pure []
    _ -> do
      first <- p
      more [ first ]
  where
  more acc = next >>= case _ of
    Punct ',' -> p >>= \x -> more (snoc acc x)
    Punct ']' -> pure acc
    _ -> failP "expected ',' or ']'"

done :: P Unit
done = P \ts -> if length ts == 0 then Right { val: unit, rest: ts } else Left "unexpected text after the closing '}'"

-- ---------------------------------------------------------------------------
-- Parse
-- ---------------------------------------------------------------------------

parsePatch :: String -> Maybe OdonusPatch
parsePatch input = hush do
  ts <- tokenize input
  r <- runP (patchP <* done) ts
  pure r.val

patchP :: P OdonusPatch
patchP = do
  kw "odonusPatch"
  name <- strP
  punctP '{'
  sc <- field "scale" scaleP
  dist <- field "distribution" distP
  octave <- field "octave" intP
  span <- optionalP (field "span" intP)
  scalarT <- field "scalarTransp" intP
  gatePct <- field "gate" intP
  quant <- field "quantize" sourceP
  swing <- field "swing" intP
  velH <- field "velHumanize" intP
  -- a step length, from before the Odonus clock (2026-10-09), is read past
  stepDiv <- optionalP (field "stepDiv" intP)
  clockV <- optionalP (field "clock" numP)
  marbles <- field "marbles" marblesP
  notes <- field "notes" (list intP)
  gates <- field "gates" (list boolP)
  skips <- field "skips" (list boolP)
  glides <- field "glides" (list boolP)
  durs <- field "durs" (list intP)
  ratchets <- field "ratchets" (list intP)
  vels <- field "vels" (list intP)
  heads0 <- field "heads" (list headP)
  gen <- field "gen" (list genP)
  punctP '}'
  let
    cells = map
      ( \i ->
          { note: at notes i 60, gate: at gates i true, skip: at skips i false
          , glide: at glides i false, dur: at durs i 1, ratchet: at ratchets i 1
          , vel: at vels i 100 }
      )
      (range 0 15)
    base = defaultOdonus
      { rootPc = sc.root, scaleIvls = sc.ivls, dist = dist
      , octaveShift = octave, degShift = scalarT, gatePct = gatePct
      , span = fromMaybe defaultOdonus.span span
      , cells = cells, heads = heads }
    heads = map (\h -> h.head { speedIx = rateIxOf h.speed }) heads0
    clockIx = maybe defaultOdonus.clockIx rateIxOf clockV
    odo = setScalePattern sc.pattern (setHarmony (quant (fromMaybe 1 stepDiv)) base { clockIx = clockIx })
  pure
    { name, odo
    , gen, genSpread: Int.toNumber marbles.spread / 100.0, genBias: Int.toNumber marbles.bias / 100.0
    , swing: Int.toNumber swing / 100.0, velHumanize: velH }
  where
  at :: forall a. Array a -> Int -> a -> a
  at arr i d = fromMaybe d (arr !! i)

-- | A root, a scale as steps or by name, and perhaps a pattern.
scaleP :: P { root :: Int, ivls :: Array Int, pattern :: Maybe String }
scaleP = do
  root <- rootP
  ivls <- orElse (list intP) named
  pattern <- optionalP strP
  pure { root, ivls, pattern }
  where
  named = word >>= \w -> case find (\t -> t.name == w) scaleTypes of
    Just t -> pure t.intervals
    Nothing -> failP ("no scale named " <> w)

rootP :: P Int
rootP = word >>= \w -> maybe (failP ("no root " <> w)) pure (findIndex (_ == w) rootNames)

distP :: P Distribution
distP = word >>= case _ of
  "Natural" -> pure Natural
  "Equal" -> pure Equal
  w -> failP ("no distribution " <> w)

marblesP :: P { spread :: Int, bias :: Int }
marblesP = do
  punctP '{'
  spread <- field "spread" intP
  bias <- field "bias" intP
  punctP '}'
  pure { spread, bias }

-- | The harmony it quantises to, given the patch's `stepDiv` (a legacy chord
-- | clock counted model steps; its pattern counts pulses, `stepDiv` to the
-- | step).
sourceP :: P (Int -> Maybe String)
sourceP = word >>= case _ of
  "scale" -> pure (const Nothing)
  "harmony" -> const <<< Just <$> strP
  -- retired 2026-10-01: Vetula sets the harmony itself now
  "vetula" -> intP *> pure (const Nothing)
  -- retired 2026-10-01: the chord clock, as the pattern that means the same
  "chords" -> do
    kw "pcs"
    sets <- list (list intP)
    kw "every"
    per <- intP
    pure \stepDiv ->
      let
        len = max 1 per * max 1 stepDiv
        clock = { segs: mapWithIndex (\i _ -> { ix: i, start: i * len, len }) sets, loopLen: len * length sets }
      in
        clockHarmony sets clock 0
  w -> failP ("no quantize source " <> w)

headP :: P { head :: Head, speed :: Number }
headP = do
  kw "head"
  pat <- patternIxOf <$> word
  spd <- numP
  dir <- dirOf <$> word
  kw "transp"
  tr <- intP
  kw "off"
  off <- intP
  kw "len"
  ln <- intP
  kw "euclid"
  pul <- intP
  est <- intP
  clk <- peek >>= case _ of
    Just (Word "clock") -> do
      _ <- word
      word >>= case _ of
        "dur" -> pure 1
        "step" -> pure 0
        w -> failP ("a head's clock is dur or step, not " <> w)
    _ -> pure 0
  mute <- peek >>= case _ of
    Just (Word "mute") -> word *> pure true
    _ -> pure false
  pure
    { head:
        { cursor: 0, seqPos: 0, accumulator: 0, pendStep: 1, etick: 0
        , speedIx: 0, direction: dir, transp: tr, mute, patternIx: pat
        , offset: off, len: ln, pulses: pul, esteps: est, clock: clk, hold: 0 }
    , speed: spd }

genP :: P GenSource
genP = do
  kw "source"
  k <- genOf <$> word
  kw "rate"
  r <- intP
  kw "amt"
  a <- intP
  on <- word >>= case _ of
    "on" -> pure true
    "off" -> pure false
    w -> failP ("expected on or off, found " <> w)
  pure { kind: k, on, rate: r, amt: a }

parseNow :: String -> Maybe OdonusNow
parseNow input = hush do
  ts <- tokenize input
  r <- runP (nowP <* done) ts
  pure r.val

nowP :: P OdonusNow
nowP = do
  kw "odonusNow"
  punctP '{'
  step <- field "step" intP
  tempo <- field "tempo" intP
  seed <- field "seed" intP
  frozen <- field "frozen" boolP
  phases <- field "phases" (list phaseP)
  sounding <- field "sounding" soundingP
  routes <- field "routes" noneOrStr
  feeds <- field "feeds" noneOrStr
  punctP '}'
  pure { step, tempo, seed: Int.toNumber seed, frozen, phases, sounding, routes, feeds }
  where
  noneOrStr = orElse (kw "none" *> pure Nothing) (Just <$> strP)

phaseP :: P Phase
phaseP = do
  punctP '{'
  cursor <- field "cursor" intP
  seqPos <- field "seqPos" intP
  accumulator <- field "acc" intP
  pendStep <- field "pend" intP
  etick <- field "etick" intP
  hold <- peek >>= case _ of
    Just (Word "hold") -> field "hold" intP
    _ -> pure 0
  punctP '}'
  pure { cursor, seqPos, accumulator, pendStep, etick, hold }

soundingP :: P Sounding
soundingP = do
  punctP '{'
  root <- field "root" rootP
  scale <- field "scale" (list intP)
  chord <- field "chord" (orElse (kw "none" *> pure Nothing) (Just <$> list intP))
  punctP '}'
  pure { root, scale, chord }

-- ---------------------------------------------------------------------------
-- Names
-- ---------------------------------------------------------------------------

patternSlug :: Int -> String
patternSlug i = toLower (maybe "rows" _.name (patternLibrary !! i))

patternIxOf :: String -> Int
patternIxOf slug = fromMaybe 0 (findIndex (\p -> toLower p.name == slug) patternLibrary)

-- | The nearest ratio in `rateTable` to a printed one (`0.667` is ÷1.5).
rateIxOf :: Number -> Int
rateIxOf v = fromMaybe unitRateIx (map _.ix (foldl closer Nothing (mapWithIndex (\ix r -> { ix, r }) speedTable)))
  where
  closer acc c = case acc of
    Just b | abs (b.r - v) <= abs (c.r - v) -> acc
    _ -> Just c

dirSlug :: Int -> String
dirSlug = case _ of
  1 -> "back"
  2 -> "pend"
  _ -> "fwd"

dirOf :: String -> Int
dirOf = case _ of
  "back" -> 1
  "pend" -> 2
  _ -> 0

genSlug :: GenKind -> String
genSlug = case _ of
  GNotes -> "notes"
  GGate -> "gate"
  GSkip -> "skip"
  GGlide -> "glide"
  GLen -> "len"
  GRatchet -> "ratchet"
  GHeads -> "heads"
  GTransp -> "transp"
  GPattern -> "pattern"
  GSpeed -> "speed"
  GKey -> "key"
  GVel -> "vel"

genOf :: String -> GenKind
genOf slug = fromMaybe GNotes (find (\k -> genSlug k == slug) genKinds)
