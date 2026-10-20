-- | Triggerfish.Selene.Source — the round-trip between the rack and an editable
-- | text document. Line-oriented (not the bracketed eDSL) because the document
-- | is the instrument: you edit the numbers here and the visualisations follow.
-- |
-- | Grammar (POLYTRIG relocated to Balistes' TIDAL tab; Selene is CV/gate only):
-- |   <kind> <target>          opens a destination — a group of 8. kind ∈
-- |                            lfo | euclid | clock | note; target is a wire
-- |                            token (es9main, es9gt0, es98cv0, fh2_0, midi1,
-- |                            virtual:bus).
-- |   <kind> <target>:<a>-<b>  a destination on jacks a..b of the target's
-- |                            eight (`fh2_0:5-8`, or one jack, `fh2cv0:3`),
-- |                            one slot per jack; two on one target may not
-- |                            share a jack (docs/kb/plans/selene-split-banks.md).
-- |   <slot>                   one line per slot, following the header; the
-- |                            block ends at the next blank line.
-- |   -- <slot>                a muted slot: that output goes silent and keeps
-- |                            its settings, so unmuting brings it back; the
-- |                            position still counts, so mutes don't shift the
-- |                            bank. A bare `--` is a silent slot. A blank line ends a block, so comments in
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
  , parsePlace
  , placeOf
  , bankOf
  , kindKeyword
  -- for the line form (Reef.Selene.Line), which reads the same tokens
  , kindOf
  , parseBase
  , noteToken
  , fmt
  , silence
  , silenced
  , slotsSilenced
  ) where

import Prelude

import Data.Array as Array
import Data.Array (drop, filter, foldl, mapWithIndex, range, snoc, take, (!!))
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number as Number
import Data.String as Str
import Data.String.CodeUnits (charAt) as Str
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
      , "-- <kind> <target> [range] opens a group of 8 · <target>:5-8 takes jacks 5-8 · one slot per line · -- mutes a slot."
      , "-- range is optional (unipolar5v / unipolar8v / bipolar5v …); omitted = the rig's default for that kind." ]

-- | One destination: a `<kind> <target>` header then eight tagged slots.
printDest :: M.Destination -> String
printDest d =
  (kindKeyword d.bank <> " " <> M.placeWire d.target d.jacks <> layoutTok <> rangeTok) <> "\n"
    <> joinWith "\n" (mapWithIndex slotLine (bankSlots d.bank))
  where
  -- Printed only when the user pinned one, so an unpinned block round-trips
  -- byte-identically and a plain header stays a plain header.
  rangeTok = maybe "" (\r -> " " <> M.rangeToWire r) d.range
    <> (if d.muted then " muted" else "")
    <> maybe "" (\p -> " retrig \"" <> p <> "\"") d.retrig
  -- tagged with its jack, so a run on 5-8 reads 5, 6, 7, 8
  -- a voice layout's outputs and where its gates are gathered
  layoutTok = case d.bank of
    M.GVoice vb -> " " <> M.voiceOutsWord vb.outputs <> maybe "" (\g -> " gates " <> M.placeWire g.target g.jacks) vb.gates
    _ -> ""
  -- tagged with its jack (a voice, with its pitch's), so a run on 5-8 reads 5, 6, 7, 8
  slotLine i row = (if Array.elem i d.mutedSlots then "  -- " else "  ") <> row <> "   -- " <> show (d.jacks.first + i * stride)
  stride = case d.bank of
    M.GVoice vb -> M.voiceStride vb
    _ -> 1
  bankSlots = case _ of
    M.GLfo slots -> map lfoLine slots
    M.GLfoT slots -> map lfoTLine slots
    M.GEuclid slots -> map euclidLine slots
    M.GClock slots -> map clockLine slots
    M.GNote slots -> map noteLine slots
    M.GEnv slots -> map envLine slots
    M.GVoice vb -> map (voiceLine vb) vb.voices

kindKeyword :: M.GenBank -> String
kindKeyword = case _ of
  M.GLfo _ -> "lfo"
  M.GLfoT _ -> "lfot"
  M.GEuclid _ -> "euclid"
  M.GClock _ -> "clock"
  M.GNote _ -> "note"
  M.GEnv _ -> "env"
  M.GVoice _ -> "voices"

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

-- | A voice: its channel, and its envelope when the layout has one.
voiceLine :: M.VoiceBank -> M.VoiceSlot -> String
voiceLine vb v = "ch " <> show v.channel <> (if Array.elem M.VEnvelope vb.outputs then " " <> envLine v.env else "")

-- | `ch <n> [envelope]`; a voice with no channel given takes `def`'s.
parseVoice :: M.VoiceSlot -> String -> M.VoiceSlot
parseVoice def s = case words s of
  [] -> def
  toks -> case toks !! 0, toks !! 1 >>= Int.fromString of
    Just "ch", Just ch -> { channel: clamp 1 16 ch, env: if Array.length toks > 2 then parseEnv (joinWith " " (drop 2 toks)) else def.env }
    _, _ -> def { env = parseEnv s }

lfoLine :: M.ModSlot -> String
lfoLine sl = fmt sl.rate <> lfoShape sl

-- | A POLYLFOT slot: its period as a ratio to the beat (`÷16`), then the
-- | phase and shape as a POLYLFO's.
lfoTLine :: M.ModSlot -> String
lfoTLine sl = M.lfoRatio sl <> lfoShape sl

lfoShape :: M.ModSlot -> String
lfoShape sl =
  " @" <> fmt sl.phase
    <> amp "lvl" sl.level
    <> amp "sin" sl.sin
    <> amp "sqr" sl.sqr
    <> amp "tri" sl.tri
    <> amp "saw" sl.saw
    <> amp "rnd" sl.rnd
    <> amp "nse" sl.nse
    <> flat
  where
  amp k v = if v == 0.0 then "" else " " <> k <> " " <> fmt v
  -- A slot turned all the way down still says what shape it is (`sqr 0`), so
  -- the shape survives the text; sine, the parser's default, goes unsaid.
  flat =
    if sl.sin == 0.0 && sl.sqr == 0.0 && sl.tri == 0.0 && sl.saw == 0.0 && sl.rnd == 0.0 && sl.nse == 0.0 && sl.wave /= M.WaveSin
      then " " <> M.waveName sl.wave <> " 0"
      else ""

euclidLine :: M.EuclidSlot -> String
euclidLine sl =
  show sl.beats <> " " <> show sl.steps <> " @" <> show sl.rate
    <> (if sl.rotation == 0 then "" else " r" <> show sl.rotation)
    <> (if sl.accentRate == 0 then "" else " acc " <> show sl.accentRate)

clockLine :: M.ClockSlot -> String
clockLine sl =
  M.clockRatio sl <> " " <> show sl.pulseWidth <> "%"
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
      , jacks :: M.Jacks
      , kind :: M.GenKind
      , range :: Maybe M.OutputRange
      , retrig :: Maybe String
      , muted :: Boolean
      , layout :: { outputs :: Array M.VoiceOut, gates :: Maybe { target :: M.Target, jacks :: M.Jacks } }
      , rows :: Array { text :: Maybe String, muted :: Boolean }
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
        Just hdr -> (closeBlock st) { open = Just { target: hdr.target, jacks: hdr.jacks, kind: hdr.kind, range: hdr.range, retrig: hdr.retrig, muted: hdr.muted, layout: hdr.layout, rows: [] } }
        Nothing -> case st.open of
          Just blk -> st { open = Just (addLine blk t) }
          Nothing -> st   -- a stray line outside any block: ignore

  -- A line inside an open block: every line is a slot row.
  addLine blk line = blk { rows = snoc blk.rows (slotText line) }

-- A slot's text and whether it is muted (leading `--`), its settings kept.
-- Trailing `-- n` tags are stripped either way; a bare `--` (no settings) is
-- a silent slot, as it always was.
slotText :: String -> { text :: Maybe String, muted :: Boolean }
slotText t = case Str.stripPrefix (Str.Pattern "--") t of
  Just rest ->
    let body = stripTrailingComment (Str.trim rest)
    in { text: if body == "" || not (startsSlot body) then Nothing else Just body, muted: true }
  Nothing -> { text: Just (stripTrailingComment t), muted: false }
  where
  -- a muted slot starts as a slot does (a number, a note or clock name, or an
  -- envelope's `a`); anything else after `--` is a note, not settings
  startsSlot b = case Str.charAt 0 b of
    Just c -> c >= '0' && c <= '9' || c >= 'A' && c <= 'G' || c == 'a'
    Nothing -> false

stripTrailingComment :: String -> String
stripTrailingComment s = case Str.indexOf (Str.Pattern "--") s of
  Just i -> Str.trim (Str.take i s)
  Nothing -> Str.trim s

-- Close the open block (if any), padding/truncating it to eight slots.
closeBlock :: PState -> PState
closeBlock st = case st.open of
  Nothing -> st
  Just blk ->
    let stride = 1 + Array.length blk.layout.outputs
        rows = fitTo (if blk.kind == M.KVoice then blk.jacks.count / stride else blk.jacks.count) blk.rows
        bank = case blk.kind of
          M.KVoice -> buildVoices blk.layout (map _.text rows)
          k -> buildBank k (map _.text rows)
        mutedSlots = Array.catMaybes (mapWithIndex (\i r -> if r.muted && isJust r.text then Just i else Nothing) rows)
    in { dests: snoc st.dests { target: blk.target, jacks: blk.jacks, range: blk.range, bank, retrig: blk.retrig, muted: blk.muted, mutedSlots }, open: Nothing }

-- Pad/truncate the row list to n (missing positions are silent).
fitTo :: Int -> Array { text :: Maybe String, muted :: Boolean } -> Array { text :: Maybe String, muted :: Boolean }
fitTo n rows = take n (rows <> map (const { text: Nothing, muted: false }) (range 0 n))

-- | A voice layout's voices, each read onto the fresh voice at its place.
buildVoices :: { outputs :: Array M.VoiceOut, gates :: Maybe { target :: M.Target, jacks :: M.Jacks } } -> Array (Maybe String) -> M.GenBank
buildVoices layout rows =
  M.GVoice { outputs: layout.outputs, gates: layout.gates, voices: mapWithIndex voice rows }
  where
  n = Array.length rows
  voice k row = let def = { channel: M.voiceChannel n k, env: M.defaultEnvSlot } in maybe def (parseVoice def) row

buildBank :: M.GenKind -> Array (Maybe String) -> M.GenBank
buildBank kind rows = case kind of
  M.KLfo -> M.GLfo (map (maybe silentLfo parseLfo) rows)
  M.KLfoT -> M.GLfoT (map (maybe silentLfo parseLfoT) rows)
  M.KEuclid -> M.GEuclid (map (maybe silentEuclid parseEuclid) rows)
  M.KClock -> M.GClock (map (maybe silentClock parseClock) rows)
  M.KNote -> M.GNote (map (maybe silentNote parseNote) rows)
  M.KEnv -> M.GEnv (map (maybe silentEnv parseEnv) rows)
  M.KVoice -> buildVoices { outputs: [], gates: Nothing } rows

-- ---------------------------------------------------------------------------
-- Header
-- ---------------------------------------------------------------------------

-- A block header — exactly `<kind> <target>`.
-- | `<kind> <target>` with an optional trailing output range. An unrecognised
-- | third token is NOT silently dropped — the whole line stops being a header, so
-- | a typo shows up as a missing block rather than as a range that quietly isn't
-- | the one you typed.
-- | A `retrig "<pattern>"` may close the header (the pattern is quoted and may
-- | hold spaces, so it is taken off before the words are counted). Before it,
-- | the word `muted` marks a muted destination.
headerOf :: String -> Maybe { kind :: M.GenKind, target :: M.Target, jacks :: M.Jacks, range :: Maybe M.OutputRange, retrig :: Maybe String, muted :: Boolean, layout :: { outputs :: Array M.VoiceOut, gates :: Maybe { target :: M.Target, jacks :: M.Jacks } } }
headerOf t0 = case words t of
  [] -> Nothing
  toks0 -> case toks0 !! 0 >>= kindOf of
    Just k ->
      let
        muted = Array.last toks0 == Just "muted"
        toks1 = if muted then fromMaybe toks0 (Array.init toks0) else toks0
        -- a voice layout: `voices <place> <outputs> [gates <place>]`
        gatesAt = Array.elemIndex "gates" toks1
        gates = gatesAt >>= \i -> toks1 !! (i + 1) <#> parsePlace
        toks2 = maybe toks1 (\i -> Array.take i toks1 <> Array.drop (i + 2) toks1) gatesAt
        outputs = if k == M.KVoice then toks2 !! 2 >>= M.voiceOutsOfWord else Nothing
        toks = if isJust outputs then Array.take 2 toks2 <> Array.drop 3 toks2 else toks2
        layout = { outputs: fromMaybe [] outputs, gates }
        at tgt r = { kind: k, target: (parsePlace tgt).target, jacks: (parsePlace tgt).jacks, range: r, retrig, muted, layout }
      in
        case toks of
          [ _, tgt ] -> Just (at tgt Nothing)
          [ _, tgt, rng ] -> map (\r -> at tgt (Just r)) (M.rangeOfWire rng)
          _ -> Nothing
    Nothing -> Nothing
  where
  split = case Str.indexOf (Str.Pattern " retrig \"") t0 of
    Just i -> { t: Str.take i t0, retrig: unquote (Str.trim (Str.drop (i + 8) t0)) }
    Nothing -> { t: t0, retrig: Nothing }
  t = split.t
  retrig = split.retrig
  unquote s = Str.stripPrefix (Str.Pattern "\"") s >>= Str.stripSuffix (Str.Pattern "\"")

kindOf :: String -> Maybe M.GenKind
kindOf = case _ of
  "lfo" -> Just M.KLfo
  "lfot" -> Just M.KLfoT
  "euclid" -> Just M.KEuclid
  "clock" -> Just M.KClock
  "note" -> Just M.KNote
  "env" -> Just M.KEnv
  "voices" -> Just M.KVoice
  _ -> Nothing

parseTarget :: String -> M.Target
parseTarget tok =
  fromMaybe (M.Virtual tok) (afterInt "es9gt" M.ES9Gt <|> afterInt "es98cv" M.ES9Cv <|> afterInt "fh2_" M.FH2 <|> afterInt "fh2cv" M.FH2Cv <|> afterInt "midi" M.Midi <|> exact)
  where
  exact = case tok of
    "es9main" -> Just M.ES9Main
    _ -> case Str.stripPrefix (Str.Pattern "virtual:") tok of
      Just s -> Just (M.Virtual s)
      Nothing -> Nothing
  afterInt pfx ctor = case Str.stripPrefix (Str.Pattern pfx) tok of
    Just rest -> map ctor (Int.fromString rest)
    Nothing -> Nothing

-- | A target with an optional run of its jacks (`fh2_0:5-8`); lenient, as
-- | `parseTarget` is: a run that is not one, or a run on a target with no
-- | jacks (MIDI, virtual), leaves the whole bank.
parsePlace :: String -> { target :: M.Target, jacks :: M.Jacks }
parsePlace tok = case split tok of
  Just p | Just j <- M.jacksOfToken p.run, hasJacks (parseTarget p.target) -> { target: parseTarget p.target, jacks: j }
  _ -> { target: parseTarget tok, jacks: M.allJacks }
  where
  split t = case Str.stripPrefix (Str.Pattern "virtual:") t of
    Just _ -> Nothing
    Nothing -> case Str.lastIndexOf (Str.Pattern ":") t of
      Just at -> Just { target: Str.take at t, run: Str.drop (at + 1) t }
      Nothing -> Nothing
  hasJacks = case _ of
    M.Midi _ -> false
    M.Virtual _ -> false
    _ -> true

-- | A bank as a line names it: strict, where `parseTarget` (for saved racks)
-- | takes any unknown token as a virtual bank. On a line that turned a typo
-- | into a bank that is kept and never sent (`fh2gt0`, AC 2026-10-05), so a
-- | virtual bank must be asked for as `virtual:name`.
bankOf :: String -> Either String M.Target
bankOf tok = case parseTarget tok of
  M.Virtual v | Str.stripPrefix (Str.Pattern "virtual:") tok == Nothing ->
    Left ("no bank " <> v <> ": es9main, es9gt<n> or es98cv<n> (the ES-9 and its expanders), fh2_0 (the FH-2), fh2_<n> (its FHX-8GT expanders, from 1), midi<n>, or virtual:<name>")
  t -> Right t

-- | A place as a line names it, strict: a bank (`bankOf`) and, after a colon,
-- | a run of its jacks (`fh2_0:5-8`, `fh2cv0:3`).
placeOf :: String -> Either String { target :: M.Target, jacks :: M.Jacks }
placeOf tok = case Str.stripPrefix (Str.Pattern "virtual:") tok, Str.lastIndexOf (Str.Pattern ":") tok of
  Nothing, Just at -> do
    let run = Str.drop (at + 1) tok
    target <- bankOf (Str.take at tok)
    jacks <- case M.jacksOfToken run of
      Just j -> Right j
      Nothing -> Left ("a run of jacks is a-b or one jack, inside 1-8, not " <> run)
    case target of
      M.Midi _ -> Left "a MIDI channel has no jacks to take a run of"
      _ -> Right { target, jacks }
  _, _ -> map (\target -> { target, jacks: M.allJacks }) (bankOf tok)

-- ---------------------------------------------------------------------------
-- Per-kind slot parsers + silent defaults
-- ---------------------------------------------------------------------------

-- | A bank of its kind with every slot silent: what `selene $ off` sends, so
-- | the outputs stop rather than run on in the daemon.
silence :: M.GenBank -> M.GenBank
silence = case _ of
  M.GLfo s -> M.GLfo (map (const silentLfo) s)
  M.GLfoT s -> M.GLfoT (map (const silentLfo) s)
  M.GEuclid s -> M.GEuclid (map (const silentEuclid) s)
  M.GClock s -> M.GClock (map (const silentClock) s)
  M.GNote s -> M.GNote (map (const silentNote) s)
  M.GEnv s -> M.GEnv (map (const silentEnv) s)
  -- a voice is silenced where its notes come from (the playhead, the
  -- routing), not by the jacks: its converter only follows notes
  M.GVoice vb -> M.GVoice vb

-- | A destination as it plays while muted: every slot silent, on a range that
-- | puts silence at 0 V. A slot's level is a position in its range (both
-- | daemons: out = level × scale + offset), so level 0 is 0 V only on a
-- | bipolar range; on 0–10 V it is 5 V, which is what `selene $ off` left on
-- | a unipolar bank before this pinned the range. Gates rest at the bottom of
-- | their range, so a gate bank keeps a unipolar one. A note bank keeps its
-- | own: pitch is V/oct, not range-scaled, and its silent note is 0 V.
silenced :: M.Destination -> M.Destination
silenced d = d { bank = silence d.bank, range = rangeFor d.bank }
  where
  rangeFor = case _ of
    M.GLfo _ -> Just M.Bipolar5V
    M.GLfoT _ -> Just M.Bipolar5V
    M.GEnv _ -> Just M.Bipolar5V
    M.GEuclid _ -> Just M.Unipolar5V
    M.GClock _ -> Just M.Unipolar5V
    M.GNote _ -> d.range
    M.GVoice _ -> d.range

-- | A destination as it plays with its muted slots (`mutedSlots`) silent and
-- | the rest as they are. An LFO slot's level is a position in its range, so on
-- | a unipolar range a muted one sits at the bottom (level -1, 0 V), not at
-- | level 0 (mid-range); on a bipolar range level 0 is 0 V.
slotsSilenced :: M.Destination -> M.Destination
slotsSilenced d
  | Array.null d.mutedSlots = d
  | otherwise = d { bank = quiet d.bank }
  where
  muted i = Array.elem i d.mutedSlots
  over :: forall a. a -> Array a -> Array a
  over silent = mapWithIndex (\i s -> if muted i then silent else s)
  unipolar = case d.range of
    Just M.Bipolar5V -> false
    Just _ -> true
    Nothing -> false
  quiet = case _ of
    M.GLfo s -> M.GLfo (over (silentLfo { level = if unipolar then -1.0 else 0.0 }) s)
    M.GLfoT s -> M.GLfoT (over (silentLfo { level = if unipolar then -1.0 else 0.0 }) s)
    M.GEuclid s -> M.GEuclid (over silentEuclid s)
    M.GClock s -> M.GClock (over silentClock s)
    M.GNote s -> M.GNote (over silentNote s)
    M.GEnv s -> M.GEnv (over silentEnv s)
    M.GVoice vb -> M.GVoice vb

silentLfo :: M.ModSlot
silentLfo = { rate: 0.0, phase: 0.0, level: 0.0, sin: 0.0, sqr: 0.0, tri: 0.0, saw: 0.0, rnd: 0.0, nse: 0.0, wave: M.WaveSin }

silentEuclid :: M.EuclidSlot
silentEuclid = { beats: 0, steps: 8, rotation: 0, rate: 4, accentRate: 0 }

silentClock :: M.ClockSlot
silentClock = { base: M.ClockQuarter, multiplier: 1, pulseWidth: 0, phase: 0 }

-- | Note 12 (C0) is 0 V on V/oct (es9-daemon `note_to_cpal_level`); note 0
-- | was −1 V.
silentNote :: M.PresetNoteSlot
silentNote = { note: 12 }

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
parseLfo s = lfoShaped (num (fromMaybe "0" (words s !! 0))) s

-- lfot <ratio> @<phase> [shapes as lfo]: `÷16` a cycle every 16 beats; a
-- ratio that isn't whole ticks doesn't move
parseLfoT :: String -> M.ModSlot
parseLfoT s = lfoShaped (maybe 0.0 M.rateOfTicks (words s !! 0 >>= M.ticksOfRatio)) s

lfoShaped :: Number -> String -> M.ModSlot
lfoShaped rate0 s = foldl apply (silentLfo { rate = rate0 }) (pairs rest)
  where
  toks = words s
  rest = drop 1 toks
  -- consume `@x` as phase; consume `key val` shape pairs
  pairs = collectPairs
  apply sl = case _ of
    Phase p -> sl { phase = p }
    Shape "lvl" v -> sl { level = v }
    Shape "sin" v -> sl { sin = v, wave = M.WaveSin }
    Shape "sqr" v -> sl { sqr = v, wave = M.WaveSqr }
    Shape "tri" v -> sl { tri = v, wave = M.WaveTri }
    Shape "saw" v -> sl { saw = v, wave = M.WaveSaw }
    Shape "rnd" v -> sl { rnd = v, wave = M.WaveRnd }
    Shape "nse" v -> sl { nse = v, wave = M.WaveNse }
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

-- euclid <beats> <steps> [@rate] [r<rotation>] [acc n]
parseEuclid :: String -> M.EuclidSlot
parseEuclid s =
  { beats: int (idx 0) silentEuclid.beats
  , steps
  , rotation: ((fromMaybe 0 rot `mod` steps) + steps) `mod` steps
  , rate: fromMaybe 4 atRate
  , accentRate: fromMaybe 0 accVal
  }
  where
  toks = words s
  idx i = fromMaybe "" (toks !! i)
  atRate = firstJust (map (\tk -> map (\r -> r) (intMaybe =<< Str.stripPrefix (Str.Pattern "@") tk)) toks)
  steps = clamp 1 M.maxEuclidSteps (int (idx 1) silentEuclid.steps)
  rot = firstJust (map (\tk -> intMaybe =<< Str.stripPrefix (Str.Pattern "r") tk) toks)
  accVal = afterKey "acc" toks

-- clock <ratio> [<pw>%] [ph<deg>]: `÷4 50%`, `×2 50%`, `×3/2 25% ph90`.
-- The form before 2026-10-19, `<base> x<mult>` (`1/4 x4`), still reads, with
-- the meaning it had last: a pulse every base × mult (so `1/4 x4` is `÷4`).
parseClock :: String -> M.ClockSlot
parseClock s =
  { base: period.base
  , multiplier: period.multiplier
  , pulseWidth: fromMaybe 50 pw
  , phase: fromMaybe 0 ph
  }
  where
  toks = words s
  idx i = fromMaybe "" (toks !! i)
  period = case baseMaybe (idx 0) of
    Just base -> { base, multiplier: fromMaybe 1 mult }
    Nothing -> fromMaybe { base: M.ClockQuarter, multiplier: 1 } (M.clockOfRatio (idx 0))
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
parseBase lbl = fromMaybe M.ClockQuarter (baseMaybe lbl)

baseMaybe :: String -> Maybe M.ClockBase
baseMaybe lbl = find (Str.toUpper lbl)
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
