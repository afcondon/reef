-- | Triggerfish.Selene.Source — the round-trip between the rack and an editable
-- | text document. Line-oriented (not the bracketed eDSL) because the document
-- | is the instrument: you edit the numbers here and the visualisations follow.
-- |
-- | Grammar (POLYTRIG relocated to Balistes' TIDAL tab; Selene is CV/gate only):
-- |   <kind> <target>          opens a destination — a group of 8. kind ∈
-- |                            lfo | euclid | clock | note; target is a wire
-- |                            token (es9main, es9gt0, es98cv0, fh2_0, midi1,
-- |                            virtual:bus).
-- |   <slot>                   one line per slot, following the header; the
-- |                            block ends at the next blank line.
-- |   -- <slot>                a muted slot (that output goes silent); the
-- |                            position still counts, so mutes don't shift the
-- |                            bank. A blank line ends a block, so comments in
-- |                            the gaps between blocks are free notes.
-- |
-- | Slot syntax per kind:
-- |   lfo     <rate> @<phase> [lvl <v>] [sin <a>] [sqr <a>] [tri <a>] [saw <a>] [rnd <a>] [nse <a>]
-- |   euclid  <beats> <steps> [@<rate>] [acc <n>]
-- |   clock   <base> x<mult> [<pw>%] [ph<deg>]      (base = 1/4, 1/8T, …)
-- |   note    <name-or-midi>                        (C4 or 60)
-- |
-- | `parseRack` is total + lenient: bad fields fall back to defaults and short
-- | blocks pad to eight with silent slots, so the live viz never blanks while
-- | you type. The printer matches the parser, so print∘parse round-trips.
module Reef.Selene.Rack
  ( printRack
  , printDest
  , parseRack
  , parseTarget
  , kindKeyword
  -- for the line form (Reef.Selene.Line), which reads the same tokens
  , kindOf
  , parseBase
  , noteToken
  , fmt
  , silence
  ) where

import Prelude

import Data.Array (drop, filter, foldl, mapWithIndex, range, snoc, take, (!!))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.String as Str
import Data.String.Common (joinWith, toLower)
import Reef.Selene.Model as M
import Reef.Conspicillum.Decimal (trimmed)

-- ---------------------------------------------------------------------------
-- Print
-- ---------------------------------------------------------------------------

printRack :: M.Selene -> String
printRack sel =
  legend <> "\n\n" <> joinWith "\n\n" (map printDest sel.destinations)
  where
  legend =
    joinWith "\n"
      [ "-- SELENE · edit the numbers; the rack follows."
      , "-- <kind> <target> [range] opens a group of 8 · one slot per line · -- mutes a slot."
      , "-- range is optional (unipolar5v / unipolar8v / bipolar5v …); omitted = the rig's default for that kind." ]

-- | One destination: a `<kind> <target>` header then eight tagged slots.
printDest :: M.Destination -> String
printDest d =
  (kindKeyword d.bank <> " " <> M.targetWire d.target <> rangeTok) <> "\n"
    <> joinWith "\n" (mapWithIndex slotLine (bankSlots d.bank))
  where
  -- Printed only when the user pinned one, so an unpinned block round-trips
  -- byte-identically and a plain header stays a plain header.
  rangeTok = maybe "" (\r -> " " <> M.rangeToWire r) d.range
  slotLine i row = "  " <> row <> "   -- " <> show (i + 1)
  bankSlots = case _ of
    M.GLfo slots -> map lfoLine slots
    M.GEuclid slots -> map euclidLine slots
    M.GClock slots -> map clockLine slots
    M.GNote slots -> map noteLine slots
    M.GEnv slots -> map envLine slots

kindKeyword :: M.GenBank -> String
kindKeyword = case _ of
  M.GLfo _ -> "lfo"
  M.GEuclid _ -> "euclid"
  M.GClock _ -> "clock"
  M.GNote _ -> "note"
  M.GEnv _ -> "env"

-- ADSR always; the rest only when moved off its default, so a plain envelope
-- reads as four numbers and a shaped one shows exactly what was changed.
envLine :: M.EnvSlot -> String
envLine sl =
  "a " <> show sl.attack <> " d " <> show sl.decay
    <> " s " <> show sl.sustain <> " r " <> show sl.release
    <> opt "depth" sl.depth M.defaultEnvSlot.depth
    -- `vel` and `time` are printed ALWAYS, not elided at their defaults, because
    -- their defaults are musically ACTIVE — which is not true of the others.
    --
    -- The elision rule is "hide it when it says nothing", and that only holds
    -- when the default IS the inert value. `depth` 127 is the normal full-scale
    -- case and its inert value is 64, so eliding it says nothing. But `vel` 96
    -- is a real velocity response (inert is 64), and `time` 2 is a 1s scale that
    -- decides whether a shape reads as a click or a swell. Eliding those hid two
    -- live facts: the drawing shows a velocity band and a colour while the text
    -- stayed silent, which is the two surfaces disagreeing about what matters.
    --
    -- Additive only: the parser still defaults an absent key, so older docs
    -- parse unchanged and simply print fuller from now on.
    <> " vel " <> show sl.velDepth
    <> " time " <> show sl.timeRange
    <> opt "rnd" sl.randomDepth M.defaultEnvSlot.randomDepth
    <> opt "ashape" sl.attackShape M.defaultEnvSlot.attackShape
    <> opt "dshape" sl.decayShape M.defaultEnvSlot.decayShape
    <> opt "rshape" sl.releaseShape M.defaultEnvSlot.releaseShape
  where
  -- Elided against the SAME base `parseEnv` folds onto (`M.defaultEnvSlot`).
  -- These were literals, and drifted from the parser's — see that definition.
  opt k v def = if v == def then "" else " " <> k <> " " <> show v

lfoLine :: M.ModSlot -> String
lfoLine sl =
  fmt sl.rate <> " @" <> fmt sl.phase
    <> amp "lvl" sl.level
    <> amp "sin" sl.sin
    <> amp "sqr" sl.sqr
    <> amp "tri" sl.tri
    <> amp "saw" sl.saw
    <> amp "rnd" sl.rnd
    <> amp "nse" sl.nse
  where
  amp k v = if v == 0.0 then "" else " " <> k <> " " <> fmt v

euclidLine :: M.EuclidSlot -> String
euclidLine sl =
  show sl.beats <> " " <> show sl.steps <> " @" <> show sl.rate
    <> (if sl.accentRate == 0 then "" else " acc " <> show sl.accentRate)

clockLine :: M.ClockSlot -> String
clockLine sl =
  M.clockBaseLabel sl.base <> " x" <> show sl.multiplier <> " " <> show sl.pulseWidth <> "%"
    <> (if sl.phase == 0 then "" else " ph" <> show sl.phase)

noteLine :: M.PresetNoteSlot -> String
noteLine sl = M.noteName sl.note

-- ---------------------------------------------------------------------------
-- Parse — total, lenient
-- ---------------------------------------------------------------------------

-- | Accumulator: the destinations closed so far + the block currently open.
-- | `rows` are slot lines for the four bank kinds (Nothing = muted).
type PState =
  { dests :: Array M.Destination
  , open :: Maybe
      { target :: M.Target
      , kind :: M.GenKind
      , range :: Maybe M.OutputRange
      , rows :: Array (Maybe String)
      }
  }

parseRack :: String -> M.Selene
parseRack doc =
  let
    final = foldl step { dests: [], open: Nothing } (Str.split (Str.Pattern "\n") doc)
  in
    { destinations: (closeBlock final).dests }
  where
  step :: PState -> String -> PState
  step st line =
    let t = Str.trim line
    in
      if t == "" then closeBlock st
      else case headerOf t of
        Just hdr -> (closeBlock st) { open = Just { target: hdr.target, kind: hdr.kind, range: hdr.range, rows: [] } }
        Nothing -> case st.open of
          Just blk -> st { open = Just (addLine blk t) }
          Nothing -> st   -- a stray line outside any block: ignore

  -- A line inside an open block: every line is a slot row (muted → Nothing).
  addLine blk line = blk { rows = snoc blk.rows (slotText line) }

-- A slot's text, or Nothing if the line is muted (leading `--`). Trailing
-- `-- n` tags are stripped either way.
slotText :: String -> Maybe String
slotText t =
  if isMuted then Nothing else Just (stripTrailingComment t)
  where
  isMuted = maybe false (const true) (Str.stripPrefix (Str.Pattern "--") t)

stripTrailingComment :: String -> String
stripTrailingComment s = case Str.indexOf (Str.Pattern "--") s of
  Just i -> Str.trim (Str.take i s)
  Nothing -> Str.trim s

-- Close the open block (if any), padding/truncating it to eight slots.
closeBlock :: PState -> PState
closeBlock st = case st.open of
  Nothing -> st
  Just blk ->
    let bank = buildBank blk.kind (fitTo M.slotCount blk.rows)
    in { dests: snoc st.dests { target: blk.target, range: blk.range, bank }, open: Nothing }

-- Pad/truncate the row list to n (missing positions are muted → silent).
fitTo :: Int -> Array (Maybe String) -> Array (Maybe String)
fitTo n rows = take n (rows <> map (const Nothing) (range 0 n))

buildBank :: M.GenKind -> Array (Maybe String) -> M.GenBank
buildBank kind rows = case kind of
  M.KLfo -> M.GLfo (map (maybe silentLfo parseLfo) rows)
  M.KEuclid -> M.GEuclid (map (maybe silentEuclid parseEuclid) rows)
  M.KClock -> M.GClock (map (maybe silentClock parseClock) rows)
  M.KNote -> M.GNote (map (maybe silentNote parseNote) rows)
  M.KEnv -> M.GEnv (map (maybe silentEnv parseEnv) rows)

-- ---------------------------------------------------------------------------
-- Header
-- ---------------------------------------------------------------------------

-- A block header — exactly `<kind> <target>`.
-- | `<kind> <target>` with an optional trailing output range. An unrecognised
-- | third token is NOT silently dropped — the whole line stops being a header, so
-- | a typo shows up as a missing block rather than as a range that quietly isn't
-- | the one you typed.
headerOf :: String -> Maybe { kind :: M.GenKind, target :: M.Target, range :: Maybe M.OutputRange }
headerOf t = case words t of
  [] -> Nothing
  toks -> case toks !! 0 >>= kindOf of
    Just k -> case toks of
      [ _, tgt ] -> Just { kind: k, target: parseTarget tgt, range: Nothing }
      [ _, tgt, rng ] -> map (\r -> { kind: k, target: parseTarget tgt, range: Just r }) (M.rangeOfWire rng)
      _ -> Nothing
    Nothing -> Nothing

kindOf :: String -> Maybe M.GenKind
kindOf = case _ of
  "lfo" -> Just M.KLfo
  "euclid" -> Just M.KEuclid
  "clock" -> Just M.KClock
  "note" -> Just M.KNote
  "env" -> Just M.KEnv
  _ -> Nothing

parseTarget :: String -> M.Target
parseTarget tok =
  fromMaybe (M.Virtual tok) (afterInt "es9gt" M.ES9Gt <|> afterInt "es98cv" M.ES9Cv <|> afterInt "fh2_" M.FH2 <|> afterInt "midi" M.Midi <|> exact)
  where
  exact = case tok of
    "es9main" -> Just M.ES9Main
    _ -> case Str.stripPrefix (Str.Pattern "virtual:") tok of
      Just s -> Just (M.Virtual s)
      Nothing -> Nothing
  afterInt pfx ctor = case Str.stripPrefix (Str.Pattern pfx) tok of
    Just rest -> map ctor (Int.fromString rest)
    Nothing -> Nothing

-- ---------------------------------------------------------------------------
-- Per-kind slot parsers + silent defaults
-- ---------------------------------------------------------------------------

-- | A bank of its kind with every slot silent: what `selene $ off` sends, so
-- | the outputs stop rather than run on in the daemon.
silence :: M.GenBank -> M.GenBank
silence = case _ of
  M.GLfo s -> M.GLfo (map (const silentLfo) s)
  M.GEuclid s -> M.GEuclid (map (const silentEuclid) s)
  M.GClock s -> M.GClock (map (const silentClock) s)
  M.GNote s -> M.GNote (map (const silentNote) s)
  M.GEnv s -> M.GEnv (map (const silentEnv) s)

silentLfo :: M.ModSlot
silentLfo = { rate: 0.0, phase: 0.0, level: 0.0, sin: 0.0, sqr: 0.0, tri: 0.0, saw: 0.0, rnd: 0.0, nse: 0.0 }

silentEuclid :: M.EuclidSlot
silentEuclid = { beats: 0, steps: 8, rate: 4, accentRate: 0 }

silentClock :: M.ClockSlot
silentClock = { base: M.ClockQuarter, multiplier: 1, pulseWidth: 0, phase: 0 }

silentNote :: M.PresetNoteSlot
silentNote = { note: 0 }

-- Silent is `depth 0` — the envelope still fires, it just moves nothing. There is
-- no "off" for an envelope the way pulseWidth 0 mutes a clock.
silentEnv :: M.EnvSlot
-- No true "off" for an envelope — depth is not on the wire — so silent is the
-- shortest possible shape rather than a muted one.
silentEnv =
  { attack: 0, decay: 0, sustain: 0, release: 0
  , depth: 0, velDepth: 64, timeRange: 2
  , randomDepth: 0
  , attackShape: 64, decayShape: 64, releaseShape: 64
  }

-- lfo  <rate> @<phase> [lvl v] [sin a] [sqr a] [tri a] [saw a] [rnd a] [nse a]
parseLfo :: String -> M.ModSlot
parseLfo s = foldl apply (silentLfo { rate = rate0 }) (pairs rest)
  where
  toks = words s
  rate0 = num (fromMaybe "0" (toks !! 0))
  rest = drop 1 toks
  -- consume `@x` as phase; consume `key val` shape pairs
  pairs = collectPairs
  apply sl = case _ of
    Phase p -> sl { phase = p }
    Shape "lvl" v -> sl { level = v }
    Shape "sin" v -> sl { sin = v }
    Shape "sqr" v -> sl { sqr = v }
    Shape "tri" v -> sl { tri = v }
    Shape "saw" v -> sl { saw = v }
    Shape "rnd" v -> sl { rnd = v }
    Shape "nse" v -> sl { nse = v }
    _ -> sl

data Tok = Phase Number | Shape String Number

collectPairs :: Array String -> Array Tok
collectPairs = go
  where
  go toks = case toks !! 0 of
    Nothing -> []
    Just h -> case Str.stripPrefix (Str.Pattern "@") h of
      Just p -> snocTok (Phase (num p)) (go (drop 1 toks))
      Nothing -> case toks !! 1 of
        Just v -> snocTok (Shape (toLower h) (num v)) (go (drop 2 toks))
        Nothing -> []
  snocTok x xs = [ x ] <> xs

-- euclid <beats> <steps> [@rate] [acc n]
parseEuclid :: String -> M.EuclidSlot
parseEuclid s =
  { beats: int (idx 0) silentEuclid.beats
  , steps: max 1 (int (idx 1) silentEuclid.steps)
  , rate: fromMaybe 4 atRate
  , accentRate: fromMaybe 0 accVal
  }
  where
  toks = words s
  idx i = fromMaybe "" (toks !! i)
  atRate = firstJust (map (\tk -> map (\r -> r) (intMaybe =<< Str.stripPrefix (Str.Pattern "@") tk)) toks)
  accVal = afterKey "acc" toks

-- clock <base> x<mult> [<pw>%] [ph<deg>]
parseClock :: String -> M.ClockSlot
parseClock s =
  { base: parseBase (idx 0)
  , multiplier: fromMaybe 1 mult
  , pulseWidth: fromMaybe 50 pw
  , phase: fromMaybe 0 ph
  }
  where
  toks = words s
  idx i = fromMaybe "" (toks !! i)
  mult = firstJust (map (\tk -> intMaybe =<< Str.stripPrefix (Str.Pattern "x") tk) toks)
  pw = firstJust (map (\tk -> intMaybe =<< Str.stripSuffix (Str.Pattern "%") tk) toks)
  ph = firstJust (map (\tk -> intMaybe =<< Str.stripPrefix (Str.Pattern "ph") tk) toks)

-- note <name-or-midi>
-- env  a <n> d <n> s <n> r <n> [depth n] [vel n] [time n] [rnd n]
--                                [ashape n] [dshape n] [rshape n]
-- Every field is a `key value` pair, so the parser is one fold over pairs and
-- an unknown key is ignored rather than shifting everything after it.
parseEnv :: String -> M.EnvSlot
parseEnv str = foldl apply M.defaultEnvSlot (keyVals (words str))
  where
  apply sl kv = case kv.key of
    "a" -> sl { attack = b kv.val }
    "d" -> sl { decay = b kv.val }
    "s" -> sl { sustain = b kv.val }
    "r" -> sl { release = b kv.val }
    "depth" -> sl { depth = b kv.val }
    "vel" -> sl { velDepth = b kv.val }
    "time" -> sl { timeRange = b kv.val }
    "rnd" -> sl { randomDepth = b kv.val }
    "ashape" -> sl { attackShape = b kv.val }
    "dshape" -> sl { decayShape = b kv.val }
    "rshape" -> sl { releaseShape = b kv.val }
    _ -> sl
  b v = clamp 0 127 (fromMaybe 0 (Int.fromString v))

-- Adjacent tokens as key/value pairs; a trailing odd token is dropped.
keyVals :: Array String -> Array { key :: String, val :: String }
keyVals toks = case toks !! 0, toks !! 1 of
  Just k, Just v -> [ { key: k, val: v } ] <> keyVals (drop 2 toks)
  _, _ -> []

parseNote :: String -> M.PresetNoteSlot
parseNote s = { note: clamp 0 127 (fromMaybe 60 (noteToken (fromMaybe "" (words s !! 0)))) }


noteToken :: String -> Maybe Int
noteToken tok = case Int.fromString tok of
  Just n -> Just n
  Nothing -> parseNoteName tok

-- C4 / C#3 / Eb… (just sharps) → MIDI
parseNoteName :: String -> Maybe Int
parseNoteName tok =
  case Str.uncons tok of
    Just { head, tail } -> do
      pc0 <- pcOf (Str.toUpper (Str.singleton head))
      let sharp = isJustPrefix "#" tail
          octStr = if sharp then Str.drop 1 tail else tail
      oct <- Int.fromString octStr
      Just ((oct + 1) * 12 + pc0 + (if sharp then 1 else 0))
    Nothing -> Nothing
  where
  isJustPrefix p s = maybe false (const true) (Str.stripPrefix (Str.Pattern p) s)
  pcOf = case _ of
    "C" -> Just 0
    "D" -> Just 2
    "E" -> Just 4
    "F" -> Just 5
    "G" -> Just 7
    "A" -> Just 9
    "B" -> Just 11
    _ -> Nothing

parseBase :: String -> M.ClockBase
parseBase lbl = fromMaybe M.ClockQuarter (find (Str.toUpper lbl))
  where
  find l = case l of
    "1/1" -> Just M.ClockWhole
    "1/2" -> Just M.ClockHalf
    "1/4" -> Just M.ClockQuarter
    "1/4T" -> Just M.ClockQuarterT
    "1/8" -> Just M.ClockEighth
    "1/8T" -> Just M.ClockEighthT
    "1/16" -> Just M.ClockSixteenth
    "1/16T" -> Just M.ClockSixteenthT
    "1/32" -> Just M.ClockThirtySecond
    "1/32T" -> Just M.ClockThirtySecondT
    "1/64T" -> Just M.ClockSixtyFourthT
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

words :: String -> Array String
words = filter (_ /= "") <<< Str.split (Str.Pattern " ") <<< normalizeWs

-- collapse tabs to spaces (split only on space)
normalizeWs :: String -> String
normalizeWs = Str.replaceAll (Str.Pattern "\t") (Str.Replacement " ")

num :: String -> Number
num s = fromMaybe 0.0 (Number.fromString s)

int :: String -> Int -> Int
int s d = fromMaybe d (Int.fromString s)

intMaybe :: String -> Maybe Int
intMaybe = Int.fromString

afterKey :: String -> Array String -> Maybe Int
afterKey key toks = go 0
  where
  go i = case toks !! i of
    Just k | k == key -> intMaybe =<< toks !! (i + 1)
    Just _ -> go (i + 1)
    Nothing -> Nothing

firstJust :: forall a. Array (Maybe a) -> Maybe a
firstJust = foldl (\acc x -> acc <|> x) Nothing

fmt :: Number -> String
fmt x = trimmed 3 x

-- local <|> for Maybe so we needn't pull Control.Alt's name into scope twice
infixl 3 alt as <|>

alt :: forall a. Maybe a -> Maybe a -> Maybe a
alt a b = case a of
  Just _ -> a
  Nothing -> b
