-- | **Selene as a line**: one line addresses one bank (a destination's eight
-- | slots) and changes only what it names (docs/kb/plans/selene-in-tidal.md).
-- |
-- |     lfo es9main # rate "0.5 1 2 4" # phase "0 0.25"
-- |     euclid es9gt0 # hits 5
-- |     clock es9gt0 # ratio "1 ÷2 ÷3 ÷4 ÷5 ÷6 ÷7 ÷8"
-- |
-- | `<kind> <bank>`, then `# <parameter> <value>` terms. A value is one token
-- | (every slot) or a quoted list, which sets the slots in turn and wraps, as
-- | Tidal's lists do. Parameters are the model's own field names, with the
-- | rack's short names beside them (`lvl`, `hits`, `acc`, `pw`, `ph`,
-- | `a d s r`), so the line and the rack read alike.
-- |
-- | A line is atomic: `applyLine` returns the rack with that one bank
-- | changed, and the rig sends that bank to its daemon as one update. A bank
-- | not in the rack yet is added; one of another kind starts fresh as the
-- | line's kind. `printLine` writes a bank back as a line (eight equal values
-- | as one), so the page can hand Limulus what is playing.
-- |
-- | Pure, and compiled to both columns (the page, and the rig reading
-- | `selene $` lines); numbers print through `Rack.fmt`, so both agree.
module Reef.Selene.Line
  ( Line
  , parseLine
  , applyLine
  , printLine
  , paramsOf
  , RigLine
  , rigLine
  , moduleOf
  , onBank
  , acceptsAt
  , refusalAt
  , onPlace
  , moduleKind
  , accepts
  , refusal
  , moduleText
  , retrigsOf
  , banksOf
  , mutesOf
  , mutedApply
  , voltPatternsOf
  , heldApply
  ) where

import Prelude

import Data.Array (all, filter, findIndex, foldM, head, length, mapWithIndex, snoc, uncons, updateAt, (!!))
import Data.Array as Array
import Data.Either (Either(..), hush, note)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number as Number
import Data.String as Str
import Data.String.Common (joinWith, trim)
import Data.Foldable (for_)
import Data.Traversable (traverse)
import Reef.Selene.Model as M
import Reef.Selene.Rack as Rack
import Reef.Selene.Recipe as Recipe
import Reef.Selene.Wire as Wire

-- | A line, read: the kind, the bank it addresses, and its terms in order.
type Line =
  { kind :: M.GenKind
  -- | a recipe (Reef.Selene.Recipe) in place of the kind: it sets the whole bank
  , recipe :: Maybe String
  , target :: M.Target
  -- | the run of the target's jacks the line's bank sits on (`fh2_0:5-8`)
  , jacks :: M.Jacks
  , terms :: Array { param :: String, values :: Array String }
  }

-- | Read a line. Errors name what was not understood.
parseLine :: String -> Either String Line
parseLine src = do
  let parts = map trim (Str.split (Str.Pattern "#") src)
  { head: h, tail: rest } <- note "an empty line" (uncons parts)
  case words h of
    [ k, t ] -> do
      let named = Recipe.recipeNamed k
      kind <- note ("not a kind of polysignal or a recipe: " <> k <> " (lfo, lfot, euclid, clock, note, volt, env, voices, throw; or a recipe: " <> joinWith ", " (map _.name Recipe.recipes) <> ")")
        (case named of
          Just b -> Just b.kind
          Nothing -> Rack.kindOf k)
      place <- Rack.placeOf t
      terms <- foldM (\acc part -> snoc acc <$> term part) [] (filter (_ /= "") rest)
      pure { kind, recipe: map _.name named, target: place.target, jacks: place.jacks, terms }
    _ -> Left ("a line starts with a kind and a bank, as in lfo es9main: " <> h)
  where
  term part = case Str.indexOf (Str.Pattern " ") part of
    Nothing | part == "fresh" -> Right { param: "fresh", values: [] }
    Nothing -> Left ("# " <> part <> " wants a value")
    Just at ->
      let
        param = Str.take at part
        v = trim (Str.drop (at + 1) part)
        values = case Str.stripPrefix (Str.Pattern "\"") v >>= Str.stripSuffix (Str.Pattern "\"") of
          Just inner -> words inner
          Nothing -> [ v ]
      in if Array.null values then Left ("# " <> param <> " wants a value") else Right { param, values }

-- | The rack with the line applied to its bank: one bank changed, the rest
-- | untouched, and the target that changed (for the rig to send).
applyLine :: Line -> M.Selene -> Either String M.Selene
applyLine line sel = do
  let
    existing = findIndex (M.samePlace line) sel.destinations
    current = case existing >>= (sel.destinations !! _) of
      Just d | M.bankKind d.bank == kindOfLine -> d.bank
      _ -> M.freshBankOn line.jacks kindOfLine
    kindOfLine = line.kind
    -- another destination on jacks this line's run shares, a voice layout's
    -- gates included: refused, as two generators on one jack would sum in
    -- the daemon (a line once landed a Euclid on a voice's gate: CR-9)
    clash = holders sel (M.samePlace line) line.target line.jacks
  -- a MIDI row takes only what the rig can send as MIDI, and a throw goes
  -- nowhere else. (Other targets' refusals are the page's alone for now: a
  -- typed `lfo es9gt0` still passes where a dropped one is refused; raised
  -- with AC 2026-10-22.)
  when ((isMidiTarget line.target || kindOfLine == M.KThrow) && not (accepts line.target kindOfLine)) $
    Left (M.placeWire line.target line.jacks <> ": " <> refusal line.target kindOfLine)
  when (kindOfLine == M.KEnv && not (M.isWholeBank line.jacks)) $
    Left (M.placeWire line.target line.jacks <> ": " <> envelopeWhole)
  unless (Array.null clash) $ Left (M.placeWire line.target line.jacks <> " shares jacks" <> heldBy clash)
  -- a recipe sets the whole bank from its own parameters; the line's other
  -- terms then apply to that, as plain parameters
  -- `# fresh` starts the bank fresh first: a module dropped on a bank
  -- replaces what was there, rather than changing only what it names
  let fresh = Array.any (\t -> t.param == "fresh") line.terms
      -- a voice layout's `outputs` and `gates` are about the whole layout,
      -- not each voice, so they are taken aside like `retrig`
      layoutTerm k = map (\t -> joinWith " " t.values) (Array.last (filter (\t -> t.param == k) line.terms))
      -- a MIDI row's `ch` and `cc` are where each signal goes, not the
      -- signal, so they are taken aside too (only there: a voice's `ch` is
      -- its own)
      midiRow = case line.target of
        M.Midi _ -> true
        _ -> false
      midiTerm k = if midiRow then Array.last (filter (\t -> t.param == k) line.terms) else Nothing
      named = filter (\t -> not (Array.elem t.param ([ "fresh", "retrig", "outputs", "gates", "range", "mute" ] <> if midiRow then [ "ch", "cc", "lo", "hi" ] else []))) line.terms
      -- `# retrig "<pattern>"` sets the bank's reset pattern, `# retrig off`
      -- clears it; the last one named wins
      retrigSet = map (\t -> joinWith " " t.values) (Array.last (filter (\t -> t.param == "retrig") line.terms))
      -- `# range unipolar10v` pins the bank's output range, `# range default`
      -- unpins it; unnamed, the range is kept
      rangeSet = map (\t -> joinWith " " t.values) (Array.last (filter (\t -> t.param == "range") line.terms))
      -- `# mute "<~ 1 [1,2]>"` sets the bank's mute pattern, `# mute off`
      -- ends it; the last one named wins
      muteSet = map (\t -> joinWith " " t.values) (Array.last (filter (\t -> t.param == "mute") line.terms))
  { start, terms } <- case line.recipe >>= Recipe.recipeNamed of
    Just b -> (\x -> { start: M.takeSlots line.jacks.count x.bank, terms: x.rest }) <$> Recipe.expand b named
    Nothing -> pure { start: if fresh then M.freshBankOn line.jacks kindOfLine else current, terms: named }
  laid <- case start of
    M.GVoice vb -> M.GVoice <$> layoutOf line sel vb (layoutTerm "outputs") (layoutTerm "gates")
    b -> Right b
  bank <- foldM applyTerm laid terms
  retrig <- case retrigSet of
    Just v | v == "off" || v == "~" || v == "" -> Right Nothing
    Just v -> case M.retrigBase line.target of
      Just _ | kindOfLine == M.KLfo -> Right (Just (unquote v))
      Just _ -> Left ("retrig resets LFOs, and this line makes " <> M.kindLabel kindOfLine)
      Nothing -> Left ("retrig needs an FH-2 LFO bank (fh2_0 or an FHX-8CV, fh2cvN), not " <> M.targetWire line.target)
    -- unnamed: kept, unless the bank starts fresh or changes kind
    Nothing -> Right case existing >>= (sel.destinations !! _) of
      Just d | M.bankKind d.bank == kindOfLine && not fresh -> d.retrig
      _ -> Nothing
  mutes <- case muteSet of
    Just v | isMidiTarget line.target -> Left ("a MIDI row's signals are muted by hand; a mute pattern needs jacks, not " <> v)
    Just v | v == "off" || v == "" -> Right Nothing
    Just v -> Right (Just (unquote v))
    -- unnamed: kept, as a mute is (it is about the jacks, not the bank)
    Nothing -> Right (existing >>= (sel.destinations !! _) >>= _.mutes)
  rangeNamed <- case rangeSet of
    Nothing -> Right Nothing
    Just w | isMidiTarget line.target -> Left ("a MIDI row has no output range, so no range " <> w)
    Just w | w == "default" || w == "-" -> Right (Just Nothing)
    Just w -> case M.rangeOfWire w of
      Nothing -> Left ("range is bipolar5v, unipolar5v, unipolar10v, unipolar8v, unipolar1v or default, not " <> w)
      Just r -> case M.fixedRange line.target of
        Just fixed | fixed /= r -> Left (M.targetWire line.target <> " is " <> M.rangeLabel fixed <> " only (the ESX-8CV has one range)")
        _ -> Right (Just (Just r))
  -- a mute is about the jacks, not the bank, so every line keeps it, and
  -- keeps each muted output too
  let was = existing >>= (sel.destinations !! _)
      muted = maybe false _.muted was
      mutedSlots = maybe [] _.mutedSlots was
      before = maybe [] M.midiOutsOf was
      fallback k = fromMaybe (M.defaultMidiOut (midiNumber line.target)) (before !! k)
  chs <- traverse (traverse (\v -> inRange "ch" 1 16 v)) (map _.values (midiTerm "ch"))
  -- `-` is no CC: that signal sends nothing
  ccs <- traverse (traverse (\v -> if v == "-" then Right Nothing else Just <$> inRange "cc" 0 127 v)) (map _.values (midiTerm "cc"))
  los <- traverse (traverse (\v -> inRange "lo" 0 127 v)) (map _.values (midiTerm "lo"))
  his <- traverse (traverse (\v -> inRange "hi" 0 127 v)) (map _.values (midiTerm "hi"))
  let
      -- each named value in turn and wrapping, as every term is; the rest kept
      pick xs k def = maybe def (\vs -> fromMaybe def (vs !! (k `mod` max 1 (length vs)))) xs
      -- but a CC is an address, and two signals can't share one, so its
      -- values go to the slots in turn without wrapping (AC, 2026-10-25)
      pickOnce xs k def = maybe def (\vs -> fromMaybe def (vs !! k)) xs
      midi = if midiRow
        then mapWithIndex (\k _ -> { channel: pick chs k (fallback k).channel, cc: pickOnce ccs k (fallback k).cc
                                   , lo: pick los k (fallback k).lo, hi: pick his k (fallback k).hi, to: (fallback k).to }) (Array.range 1 (M.bankSize bank))
        else []
      dest = { target: line.target, jacks: line.jacks, range: Nothing, bank, retrig, mutes, muted, mutedSlots, midi }
      next = case existing of
        Just i -> sel { destinations = fromMaybe sel.destinations (updateAt i (withRange (keepRange i dest)) sel.destinations) }
        Nothing -> sel { destinations = snoc sel.destinations (withRange dest) }
      withRange d = maybe d (\r -> d { range = r }) rangeNamed
  case M.midiClash next of
    Just why | midiRow -> Left why
    _ -> pure next
  where
  keepRange i d = d { range = (sel.destinations !! i) >>= _.range }
  isMidiTarget = case _ of
    M.Midi _ -> true
    _ -> false
  midiNumber = case _ of
    M.Midi n -> n
    _ -> 1
  inRange k lo hi v = case Int.fromString v of
    Just n | n >= lo && n <= hi -> Right n
    _ -> Left (k <> " is a whole number from " <> show lo <> " to " <> show hi <> ", not " <> v)

-- | One term over a bank: the parameter set on every slot from the values,
-- | in turn and wrapping.
applyTerm :: M.GenBank -> { param :: String, values :: Array String } -> Either String M.GenBank
applyTerm bank t = case bank of
  M.GLfo slots -> M.GLfo <$> each slots \sl v -> case t.param of
    p | p == "rate" -> (\x -> sl { rate = x }) <$> number v
    _ -> shaped "lfo" "rate" sl v
  -- the same shapes, on the beat: the period as a ratio, as a clock's
  M.GLfoT slots -> M.GLfoT <$> each slots \sl v -> case t.param of
    p | p == "ratio" || p == "rate" ->
          note ("ratio wants ÷n (a cycle every n beats), ×n (n a beat) or ×p/q, whole 24ppqn ticks; not " <> v)
            ((\ticks -> sl { rate = M.rateOfTicks ticks }) <$> M.ticksOfRatio v)
    _ -> shaped "lfot" "ratio" sl v
  M.GEuclid slots -> M.GEuclid <$> each slots \sl v -> case t.param of
    p | p == "beats" || p == "hits" -> (\x -> sl { beats = x }) <$> integer v
      | p == "steps" -> (\x -> let n = clamp 1 M.maxEuclidSteps x in sl { steps = n, beats = min sl.beats n, rotation = sl.rotation `mod` n }) <$> integer v
      | p == "rot" || p == "rotation" -> (\x -> sl { rotation = ((x `mod` sl.steps) + sl.steps) `mod` sl.steps }) <$> integer v
      | p == "rate" -> (\x -> sl { rate = x }) <$> integer v
      | p == "acc" || p == "accentRate" -> (\x -> sl { accentRate = x }) <$> integer v
    _ -> unknown "euclid" "hits steps rot rate acc"
  M.GClock slots -> M.GClock <$> each slots \sl v -> case t.param of
    p | p == "ratio" || p == "clock" ->
          note ("ratio wants ×n (n a beat), ÷n (one every n beats) or ×p/q, whole 24ppqn ticks; not " <> v)
            ((\c -> sl { base = c.base, multiplier = c.multiplier }) <$> M.clockOfRatio v)
      -- the fields beneath the ratio, as the FH-2 names them
      | p == "div" || p == "base" -> Right (sl { base = Rack.parseBase v })
      | p == "mult" || p == "x" || p == "multiplier" -> (\x -> sl { multiplier = x }) <$> integer v
      | p == "pw" || p == "pulseWidth" -> (\x -> sl { pulseWidth = x }) <$> integer v
      | p == "phase" || p == "ph" -> (\x -> sl { phase = x }) <$> integer v
    _ -> unknown "clock" "ratio pw phase"
  M.GNote slots -> M.GNote <$> each slots \sl v -> case t.param of
    p | p == "note" || p == "n" -> (\x -> sl { note = clamp 0 127 x }) <$> note ("not a note: " <> v) (Rack.noteToken v)
    _ -> unknown "note" "note"
  M.GEnv slots -> M.GEnv <$> each slots \sl v -> case t.param of
    p | p == "attack" || p == "a" -> (\x -> sl { attack = x }) <$> integer v
      | p == "decay" || p == "d" -> (\x -> sl { decay = x }) <$> integer v
      | p == "sustain" || p == "s" -> (\x -> sl { sustain = x }) <$> integer v
      | p == "release" || p == "r" -> (\x -> sl { release = x }) <$> integer v
      | p == "depth" -> (\x -> sl { depth = x }) <$> integer v
      | p == "vel" || p == "velDepth" -> (\x -> sl { velDepth = x }) <$> integer v
      | p == "time" || p == "timeRange" -> (\x -> sl { timeRange = x }) <$> integer v
      | p == "rnd" || p == "randomDepth" -> (\x -> sl { randomDepth = x }) <$> integer v
      | p == "ashape" -> (\x -> sl { attackShape = x }) <$> integer v
      | p == "dshape" -> (\x -> sl { decayShape = x }) <$> integer v
      | p == "rshape" -> (\x -> sl { releaseShape = x }) <$> integer v
    _ -> unknown "env" "a d s r depth vel time rnd ashape dshape rshape"
  M.GVoice vb
    | t.param == "ch" || t.param == "channel" ->
        (\vs -> M.GVoice vb { voices = vs }) <$> each vb.voices \v x -> (\c -> v { channel = clamp 1 16 c }) <$> integer x
    | otherwise -> case applyTerm (M.GEnv (map _.env vb.voices)) t of
        Right (M.GEnv envs) -> Right (M.GVoice vb { voices = Array.zipWith (\v e -> v { env = e }) vb.voices envs })
        Right _ -> Right bank
        Left _ -> unknown "voices" "ch, and a d s r depth vel time rnd ashape dshape rshape for its envelopes"
  M.GThrow slots -> M.GThrow <$> each slots \sl v -> case t.param of
    p | p == "a" || p == "attack" -> (\l -> sl { attack = l }) <$> lengthIn v
      | p == "h" || p == "hold" -> (\l -> sl { hold = l }) <$> lengthIn v
      | p == "d" || p == "decay" -> (\l -> sl { decay = l }) <$> lengthIn v
      | p == "on" -> note ("on is now, beat or bar, not " <> v) (M.startOf v)
          <#> \o -> sl { start = o }
    _ -> unknown "throw" "a h d on"
  -- `# volts 3 "<0 1.2 2.5>"`: output 3 follows a pattern of voltages, the
  -- others kept; `# volts 3 2.5V` sets it alone
  M.GVolt slots | Just one <- slotForm -> M.GVolt <$> oneSlot slots one
  -- a voltage set is set: a pattern on the outputs it sets ends
  M.GVolt slots -> M.GVolt <$> each slots \sl v -> case t.param of
    p | p == "volts" || p == "v" -> (\x -> sl { volts = x, pattern = Nothing }) <$> note (t.param <> " wants a voltage, as 2.5V or -3V, not " <> v) (M.voltOf v)
    _ -> unknown "volt" "volts (and range)"
  where
  -- `<output> <voltage or quoted pattern>`, one value as the term reads it
  slotForm = case t.values of
    [ v ] | t.param == "volts" || t.param == "v" -> case Str.indexOf (Str.Pattern " ") v of
      Just at -> Just { slot: Str.take at v, value: trim (Str.drop (at + 1) v) }
      Nothing -> Nothing
    _ -> Nothing
  oneSlot :: Array M.VoltSlot -> { slot :: String, value :: String } -> Either String (Array M.VoltSlot)
  oneSlot slots one = do
    k <- case Int.fromString one.slot of
      Just k | k >= 1 && k <= length slots -> Right k
      _ -> Left ("volts names an output from 1 to " <> show (length slots) <> ", not " <> one.slot)
    sl <- case Str.stripPrefix (Str.Pattern "\"") one.value >>= Str.stripSuffix (Str.Pattern "\"") of
      Just inner
        | trim inner == "" -> Left "volts wants a pattern of voltages, as \"<0 1.2 2.5>\""
        | Just x <- M.voltOf (trim inner) -> Right { volts: x, pattern: Nothing }
        | otherwise -> Right { volts: 0.0, pattern: Just (trim inner) }
      Nothing -> (\x -> { volts: x, pattern: Nothing }) <$> note ("volts wants a voltage, as 2.5V, or a quoted pattern; not " <> one.value) (M.voltOf one.value)
    note "volts: no such output" (updateAt (k - 1) sl slots)
  -- an LFO slot's phase, level and shape: what POLYLFO and POLYLFOT share
  shaped :: String -> String -> M.ModSlot -> String -> Either String M.ModSlot
  shaped kind period sl v = case t.param of
    p | p == "phase" || p == "ph" -> (\x -> sl { phase = x }) <$> number v
      | p == "level" || p == "lvl" -> (\x -> sl { level = x }) <$> number v
      | p == "sin" -> (\x -> sl { sin = x }) <$> number v
      | p == "sqr" -> (\x -> sl { sqr = x }) <$> number v
      | p == "tri" -> (\x -> sl { tri = x }) <$> number v
      | p == "saw" -> (\x -> sl { saw = x }) <$> number v
      | p == "rnd" -> (\x -> sl { rnd = x }) <$> number v
      | p == "nse" -> (\x -> sl { nse = x }) <$> number v
    _ -> unknown kind (period <> " phase level sin sqr tri saw rnd nse")
  each :: forall s. Array s -> (s -> String -> Either String s) -> Either String (Array s)
  each slots f = foldM (\acc x -> snoc acc <$> x) [] (mapWithIndex (\i sl -> f sl (valueAt i)) slots)
  valueAt i = fromMaybe "" (t.values !! (i `mod` length t.values))
  unknown :: forall s. String -> String -> Either String s
  unknown kind known = Left (kind <> " has no " <> t.param <> " (it has " <> known <> ")")
  number v = note (t.param <> " wants a number, not " <> v) (Number.fromString v)
  integer v = note (t.param <> " wants a whole number, not " <> v) (Int.fromString v)
  lengthIn v = note (t.param <> " is a length, as 250ms, 1/2b (beats) or 8bar, or 0; not " <> v) (M.lengthOf v)

-- | The parameters a bank's kind takes, as `printLine` names them.
paramsOf :: M.GenKind -> Array String
paramsOf = case _ of
  M.KLfo -> [ "rate", "phase", "level", "sin", "sqr", "tri", "saw", "rnd", "nse" ]
  M.KLfoT -> [ "ratio", "phase", "level", "sin", "sqr", "tri", "saw", "rnd", "nse" ]
  M.KEuclid -> [ "hits", "steps", "rot", "rate", "acc" ]
  M.KClock -> [ "ratio", "pw", "phase" ]
  M.KNote -> [ "note" ]
  M.KEnv -> [ "a", "d", "s", "r", "depth", "vel", "time", "rnd", "ashape", "dshape", "rshape" ]
  M.KVoice -> [ "ch", "a", "d", "s", "r", "depth", "vel", "time", "rnd", "ashape", "dshape", "rshape" ]
  M.KThrow -> [ "a", "h", "d", "on" ]
  M.KVolt -> [ "volts" ]

-- | A voice layout's outputs and gates for a line: set from its terms, the
-- | voices sized to the run (as many whole voices as fit), and the gates on
-- | their own bank: as named, else where they were, else the first run of a
-- | gate bank nothing else holds.
layoutOf :: Line -> M.Selene -> M.VoiceBank -> Maybe String -> Maybe String -> Either String M.VoiceBank
layoutOf line sel vb0 outputsTerm gatesTerm = do
  outputs <- case outputsTerm of
    Just w -> note ("outputs is pitch, pitch+env, pitch+vel+env+pres …, not " <> w) (M.voiceOutsOfWord w)
    Nothing -> Right vb0.outputs
  let
    stride = 1 + Array.length outputs
    n = line.jacks.count / stride
    fresh = case M.freshBank M.KVoice of
      M.GVoice f -> f.voices
      _ -> []
    -- the voices this place already had keep their channels; new ones take
    -- channels no other layout plays (from 11 up, then down from 10), so
    -- two converters never answer one note
    kept = case Array.find (M.samePlace line) sel.destinations of
      Just { bank: M.GVoice old } -> old.voices
      _ -> []
    taken = Array.concatMap (\o -> case o.bank of
      M.GVoice ov | not (M.samePlace o line) -> map _.channel ov.voices
      _ -> []) sel.destinations <> map _.channel kept
    spare = filter (\c -> not (Array.elem c taken)) (Array.range 11 16 <> Array.reverse (Array.range 1 10))
    incoming = Array.drop (Array.length kept) (vb0.voices <> fresh <> fresh)
    voices = Array.take n (Array.take n kept <> mapWithIndex (\k v -> v { channel = fromMaybe v.channel (spare !! k) }) incoming)
  when (n < 1) $ Left (M.placeWire line.target line.jacks <> " is too narrow for one voice of " <> M.voiceOutsWord outputs)
  gateBank <- note ("voices need a gate bank beside " <> M.targetWire line.target <> " for their gates") (gateBankFor line.target)
  gates <- case gatesTerm of
    Just g -> do
      place <- Rack.placeOf g
      when (place.target /= gateBank) $ Left ("the gates go on " <> M.targetWire gateBank <> ", the gate bank beside " <> M.targetWire line.target)
      Right { target: place.target, jacks: place.jacks { count = n } }
    Nothing -> case vb0.gates of
      Just g | g.jacks.first + n - 1 <= M.slotCount && free gateBank (g.jacks { count = n }) -> Right (g { jacks = g.jacks { count = n } })
      -- the same jack numbers as the layout's own run first (pitches on 5-8,
      -- gates on 5-8: easy to patch and to remember), else the first free run
      _ -> note ("no " <> show n <> " free jacks on " <> M.targetWire gateBank <> " for these voices' gates" <> heldBy (holders sel (M.samePlace line) gateBank { first: 1, count: M.slotCount }))
             (Array.find (free gateBank) (map (\f -> { first: f, count: n }) (Array.nub ([ line.jacks.first ] <> Array.range 1 (M.slotCount - n + 1)) # filter (\f -> f + n - 1 <= M.slotCount))))
               <#> \j -> { target: gateBank, jacks: j }
  for_ (Array.find (\o -> not (M.samePlace o line) && Array.any (\p -> p.target == gates.target && M.jacksOverlap p.jacks gates.jacks) (M.placesOf o)) sel.destinations) \o ->
    Left ("the voices' gates on " <> M.placeWire gates.target gates.jacks <> " share jacks" <> heldBy (holders sel (\d -> not (M.samePlace d o)) gates.target gates.jacks))
  pure { outputs, gates: Just gates, voices }
  where
  -- jacks nothing else on the rack holds (this layout's own place aside)
  free target j = not (Array.any (\o -> not (M.samePlace o line) && Array.any (\p -> p.target == target && M.jacksOverlap p.jacks j) (M.placesOf o)) sel.destinations)

-- | What on the rack holds any of these jacks, each said with how to free
-- | it: "fh2_1 holds an empty euclid (off fh2_1)", or "fh2_1:1-2
-- | holds the gates of the voices on fh2_0:1-2 (off fh2_0:1-2)". A refusal
-- | over a taken jack names its holder, never just the fact (CR-9 in
-- | docs/kb/plans/atlantis-crs.md: an empty Euclid on the whole GT looked
-- | like a rig stuck with jacks both free and taken).
-- | `skip` leaves out the destination the line is about.
holders :: M.Selene -> (M.Destination -> Boolean) -> M.Target -> M.Jacks -> Array String
holders sel skip target jacks = Array.concatMap one (filter (not <<< skip) sel.destinations)
  where
  one d = Array.mapMaybe (\p ->
      if p.target == target && M.jacksOverlap p.jacks jacks then Just (describe d p) else Nothing) (M.placesOf d)
  describe d p =
    let
      off = " (off " <> M.placeWire d.target d.jacks <> ")"
      what
        | M.samePlace p d = article d <> Rack.kindKeyword d.bank
        | otherwise = "the gates of the " <> Rack.kindKeyword d.bank <> " on " <> M.placeWire d.target d.jacks
    in M.placeWire p.target p.jacks <> " holds " <> what <> off
  -- an empty Euclid still holds its jacks, and is the easiest holder to miss
  article d = case d.bank of
    M.GEuclid xs | all (\x -> x.beats == 0) xs -> "an empty "
    _ -> "the "

-- | The holders as the tail of a refusal.
heldBy :: Array String -> String
heldBy = case _ of
  [] -> ""
  hs -> ": " <> joinWith "; " hs

-- | The gate bank a layout on a CV bank gathers its gates on: the FHX-8GT for
-- | the FH-2 and its FHX-8CVs. (The ES-9's, the ES-5, when it has layouts.)
gateBankFor :: M.Target -> Maybe M.Target
gateBankFor = case _ of
  M.FH2 0 -> Just (M.FH2 1)
  M.FH2Cv _ -> Just (M.FH2 1)
  _ -> Nothing

-- | A bank as a line: the kind's main parameters always (`mainOf`), and any
-- | other whose slots are not all at the kind's fresh value; eight equal
-- | values printed as one. The main ones are said even at their fresh values,
-- | since a line changes only what it names and a printed bank is read as the
-- | whole of it.
printLine :: M.Destination -> String
printLine d =
  joinWith " # " ([ Rack.kindKeyword d.bank <> " " <> M.placeWire d.target d.jacks ] <> terms <> rangeTerm <> midiTerms <> retrigTerm)
  where
  -- a MIDI row's channel and CC per signal, always said (Rack's rule)
  midiTerms = case M.midiOutsOf d of
    [] -> []
    outs -> [ "ch " <> value (map (show <<< _.channel) outs), "cc " <> listed (map (maybe "-" show <<< _.cc) outs) ]
      <> (if Array.all (\o -> o.lo == 0 && o.hi == 127) outs then [] else [ "lo " <> value (map (show <<< _.lo) outs), "hi " <> value (map (show <<< _.hi) outs) ])
  -- a CC list said whole: one value would be slot 1's alone
  listed vs = if length vs == 1 then fromMaybe "" (head vs) else "\"" <> joinWith " " vs <> "\""
  fresh = case d.bank of
    -- a layout's voices against fresh voices like them (their envelopes)
    M.GVoice vb -> case M.freshBank M.KVoice of
      M.GVoice f -> M.GVoice vb { voices = map (\v -> v { env = fromMaybe v.env (map _.env (f.voices !! 0)) }) vb.voices }
      other -> other
    _ -> M.freshBankOn d.jacks (M.bankKind d.bank)
  terms = layoutTerms <> Array.catMaybes (map term (filter said (paramsOf (M.bankKind d.bank)))) <> patternTerms
  -- each output that follows a pattern, after the voltages it holds
  patternTerms = case d.bank of
    M.GVolt s -> Array.catMaybes (mapWithIndex (\k sl -> map (\p -> "volts " <> show (k + 1) <> " \"" <> p <> "\"") sl.pattern) s)
    _ -> []
  layoutTerms = case d.bank of
    M.GVoice vb -> [ "outputs " <> M.voiceOutsWord vb.outputs ] <> maybe [] (\g -> [ "gates " <> M.placeWire g.target g.jacks ]) vb.gates
    _ -> []
  -- a layout with no envelope has no envelope to say
  said p = case d.bank of
    M.GVoice vb -> p == "ch" || Array.elem M.VEnvelope vb.outputs
    _ -> true
  term p =
    let vs = valuesOf p d.bank
    in if vs == valuesOf p fresh && not (Array.elem p (mainOf (M.bankKind d.bank))) then Nothing else Just (p <> " " <> value vs)
  retrigTerm = maybe [] (\p -> [ "retrig \"" <> p <> "\"" ]) d.retrig
    <> maybe [] (\p -> [ "mute \"" <> p <> "\"" ]) d.mutes
  -- said when pinned: a POLYVOLT's volts, or an LFO's level, mean what they
  -- do on it
  rangeTerm = maybe [] (\r -> [ "range " <> M.rangeToWire r ]) d.range
  value vs = case head vs of
    Just v | all (_ == v) vs -> v
    _ -> "\"" <> joinWith " " vs <> "\""

-- | What a bank is, at a glance: printed whatever their values.
mainOf :: M.GenKind -> Array String
mainOf = case _ of
  M.KLfo -> [ "rate", "phase" ]
  M.KLfoT -> [ "ratio", "phase" ]
  M.KEuclid -> [ "hits", "steps" ]
  M.KClock -> [ "ratio" ]
  M.KNote -> [ "note" ]
  M.KEnv -> [ "a", "d", "s", "r" ]
  M.KVoice -> [ "ch" ]
  M.KThrow -> [ "a", "h", "d" ]
  M.KVolt -> [ "volts" ]

valuesOf :: String -> M.GenBank -> Array String
valuesOf p = case _ of
  M.GLfoT s
    | p == "ratio" -> map M.lfoRatio s
    | otherwise -> valuesOf p (M.GLfo s)
  M.GLfo s -> map (\sl -> Rack.fmt case p of
    "rate" -> sl.rate
    "phase" -> sl.phase
    "level" -> sl.level
    "sin" -> sl.sin
    "sqr" -> sl.sqr
    "tri" -> sl.tri
    "saw" -> sl.saw
    "rnd" -> sl.rnd
    _ -> sl.nse) s
  M.GEuclid s -> map (\sl -> show case p of
    "hits" -> sl.beats
    "steps" -> sl.steps
    "rot" -> sl.rotation
    "rate" -> sl.rate
    _ -> sl.accentRate) s
  M.GClock s -> map (\sl -> case p of
    "ratio" -> M.clockRatio sl
    "div" -> M.clockBaseLabel sl.base
    "mult" -> show sl.multiplier
    "pw" -> show sl.pulseWidth
    _ -> show sl.phase) s
  M.GNote s -> map (\sl -> M.noteName sl.note) s
  M.GEnv s -> map (\sl -> show case p of
    "a" -> sl.attack
    "d" -> sl.decay
    "s" -> sl.sustain
    "r" -> sl.release
    "depth" -> sl.depth
    "vel" -> sl.velDepth
    "time" -> sl.timeRange
    "rnd" -> sl.randomDepth
    "ashape" -> sl.attackShape
    "dshape" -> sl.decayShape
    _ -> sl.releaseShape) s
  M.GVoice vb
    | p == "ch" -> map (\v -> show v.channel) vb.voices
    | otherwise -> valuesOf p (M.GEnv (map _.env vb.voices))
  M.GThrow s -> map (\sl -> case p of
    "a" -> M.lengthWord sl.attack
    "h" -> M.lengthWord sl.hold
    "d" -> M.lengthWord sl.decay
    _ -> M.startWord sl.start) s
  M.GVolt s -> map (M.voltWord <<< _.volts) s

words :: String -> Array String
words = filter (_ /= "") <<< Str.split (Str.Pattern " ") <<< trim

unquote :: String -> String
unquote s = fromMaybe s (Str.stripPrefix (Str.Pattern "\"") s >>= Str.stripSuffix (Str.Pattern "\""))

-- | Each retrig pattern in a rack, with the FH-2 output its bank's slot 1 is
-- | on (Architeuthis plays them; slot k resets output `base + k - 1`). A muted
-- | bank's pattern is left out, so it stops with the mute.
retrigsOf :: String -> Array { base :: Int, pattern :: String }
retrigsOf rack = Array.mapMaybe one (Rack.parseRack rack).destinations
  where
  one d = case d.bank, d.retrig, M.retrigBase d.target of
    M.GLfo _, Just pattern, Just base | not d.muted -> Just { base: base + d.jacks.first - 1, pattern }
    _, _, _ -> Nothing

-- | The modular banks a rack holds: each one's daemon (socket), bank, and the
-- | name its claim is held under. What is not here has no business holding
-- | jacks, so the rig's keeper releases any bank it kept that the rack has lost
-- | (Architeuthis, selene_keeper), however it went. MIDI and virtual targets
-- | hold no jacks and are left out.
banksOf :: String -> Array { socket :: String, bank :: String, owner :: String }
banksOf rack = Array.mapMaybe one (Rack.parseRack rack).destinations
  where
  one d = map (\sb -> { socket: sb.socket, bank: Wire.placeOn sb d.jacks, owner: Wire.ownerOf sb d.jacks }) (Wire.targetSocketBank d.target)

-- | Each mute pattern in a rack, with the daemon and bank (its place token,
-- | `main` or `main:5-8`) of the bank it mutes. Architeuthis plays them
-- | (selene_mutes). A muted bank's pattern is left out: it is silent anyway.
mutesOf :: String -> Array { socket :: String, bank :: String, pattern :: String }
mutesOf rack = Array.mapMaybe one (Rack.parseRack rack).destinations
  where
  one d = case d.mutes, Wire.targetSocketBank d.target of
    Just pattern, Just sb | not d.muted -> Just { socket: sb.socket, bank: Wire.placeOn sb d.jacks, pattern }
    _, _ -> Nothing

-- | The apply for a bank with these slots (1-based) silent as well as its
-- | own muted ones: what the rig sends when a mute pattern changes. The rack
-- | is not changed, since a pattern's mute is an act, not a setting. Slots
-- | outside the bank are ignored; Nothing when the bank is not on the rack.
mutedApply :: String -> String -> String -> Array Int -> Maybe { json :: String, command :: String }
mutedApply rack socket bank slots = heldApply rack socket bank slots []

-- | Each POLYVOLT output in a rack that follows a pattern of voltages, with
-- | its bank (Architeuthis plays them, one stream a bank). A muted bank's, or
-- | a muted output's, is left out, so it stops with the mute.
voltPatternsOf :: String -> Array { socket :: String, bank :: String, slot :: Int, pattern :: String }
voltPatternsOf rack = Array.concatMap one (Rack.parseRack rack).destinations
  where
  one d = case d.bank, Wire.targetSocketBank d.target of
    M.GVolt slots, Just sb | not d.muted ->
      Array.catMaybes (mapWithIndex (\k sl -> case sl.pattern of
        Just pattern | not (Array.elem k d.mutedSlots) -> Just { socket: sb.socket, bank: Wire.placeOn sb d.jacks, slot: k + 1, pattern }
        _ -> Nothing) slots)
    _, _ -> []

-- | The apply for a bank with these slots (1-based) silent as well as its
-- | own muted ones, and these POLYVOLT outputs at these voltages: what the
-- | rig sends when a mute pattern or a pattern of voltages moves. The rack
-- | is not changed. Slots outside the bank are ignored; Nothing when the
-- | bank is not on the rack.
heldApply :: String -> String -> String -> Array Int -> Array { slot :: Int, volts :: Number } -> Maybe { json :: String, command :: String }
heldApply rack socket bank slots volts = do
  d <- Array.find (\x -> map (\sb -> sb.socket == socket && Wire.placeOn sb x.jacks == bank) (Wire.targetSocketBank x.target) == Just true) (Rack.parseRack rack).destinations
  let extra = Array.mapMaybe (\k -> if k >= 1 && k <= M.bankSize d.bank then Just (k - 1) else Nothing) slots
      at k sl = maybe sl (\v -> sl { volts = v.volts }) (Array.find (\v -> v.slot == k + 1) volts)
      bank' = case d.bank of
        M.GVolt s -> M.GVolt (mapWithIndex at s)
        b -> b
  w <- Wire.destinationEnvelope d { bank = bank', mutedSlots = Array.sort (Array.nub (d.mutedSlots <> extra)) }
  pure { json: w.json, command: "apply-polysignal " <> w.json }

-- | What the rig does with a `selene $` line (Architeuthis's Handler): the
-- | rack it keeps on the stage (`selene/rack`) with the line applied, and the
-- | one bank the line touched, ready for its daemon. `socket` is "" when that
-- | bank is not on the modular (a MIDI or virtual target): kept, not sent.
-- | What a line does on the rig: the rack after it, the line as written back,
-- | the daemon (socket) and bank it reaches, the apply envelope (`json`, empty
-- | when nothing is applied), the whole `command` for the daemon (empty
-- | when nothing is sent), and a `note` for the reply when the hardware will
-- | not do quite what the line says (empty when it will).
-- | `also`: more places the line applied, for the rig to keep beside
-- | `bank`/`json` (a `swap` moves two).
type RigLine = { rack :: String, line :: String, socket :: String, bank :: String, json :: String, command :: String, note :: String, also :: Array { bank :: String, json :: String } }

rigLine :: String -> String -> Either String RigLine
rigLine src rack = case Str.stripPrefix (Str.Pattern "reset ") (trim src) of
  Just rest -> resetOn rest rack
  Nothing -> rigCommand src rack

rigCommand :: String -> String -> Either String RigLine
rigCommand src rack = case words src of
  -- `off <bank>`: the bank leaves the rack, and its daemon releases the
  -- bank's claim, which silences its jacks, so the outputs stop and are free
  -- for something else (a dropped "free"). No json: there is nothing for the
  -- rig to keep and send again.
  [ "off", t ] -> do
    place <- Rack.placeOf t
    let
      sel = Rack.parseRack rack
      sb = Wire.targetSocketBank place.target
    -- not a bank on the rack: free, or part of what another bank holds,
    -- which the refusal names so the right one can be taken off
    _ <- note (case holders sel (const false) place.target place.jacks of
        [] -> t <> " is already free"
        hs -> t <> " is not a bank on the rack" <> heldBy hs) (Array.find (M.samePlace place) sel.destinations)
    pure
      { rack: Rack.printRack (sel { destinations = filter (not <<< M.samePlace place) sel.destinations })
      , line: "off " <> t
      , socket: maybe "" _.socket sb
      , bank: maybe "" (\b -> Wire.placeOn b place.jacks) sb
      , json: ""
      , command: maybe "" (\b -> "release-claim polysignal " <> Wire.ownerOf b place.jacks) sb
      , note: ""
      , also: []
      }
  -- `split <bank>`: a whole bank becomes its first four jacks, keeping their
  -- slots, and jacks 5-8 are free for something else (two rows of four on
  -- the page). `join <bank>:<run>`: a run grows back to the whole bank, its
  -- slots on its jacks and fresh ones on the rest, if nothing else is on
  -- the bank. Either is two daemon commands, in order: the old claim
  -- released (which silences its jacks), then the new one applied, since the
  -- daemon refuses a claim that shares jacks with another.
  [ verb, t ] | verb == "split" || verb == "join" -> do
    place <- Rack.placeOf t
    let sel = Rack.parseRack rack
    i <- note (t <> " has nothing on it to " <> verb) (findIndex (M.samePlace place) sel.destinations)
    d <- note (t <> " went missing") (sel.destinations !! i)
    d' <- if verb == "split" then splitDest d else joinDest sel d
    let
      sel' = sel { destinations = fromMaybe sel.destinations (updateAt i d' sel.destinations) }
      sb = Wire.targetSocketBank d.target
      sent = Wire.destinationEnvelope d'
    pure
      { rack: Rack.printRack sel'
      , line: verb <> " " <> t
      , socket: maybe "" _.socket sb
      , bank: maybe "" _.bank sent
      , json: maybe "" _.json sent
      , command: case sb, sent of
          Just b, Just w -> "release-claim polysignal " <> Wire.ownerOf b d.jacks <> "\n" <> "apply-polysignal " <> w.json
          _, _ -> ""
      , note: ""
      , also: []
      }
  -- `swap <bank>`: what is on the bank's jacks 1-4 and what is on 5-8
  -- change places, a voice layout's gates there included (AC: "pitches 1-4
  -- go with gates 1-4"). Every place that moves is released, then every one
  -- applied again where it now is, in that order, so no claim meets one it
  -- is about to replace.
  [ "swap", t ] -> do
    target <- Rack.bankOf t
    let sel = Rack.parseRack rack
    sb <- note (t <> " is not a modular bank") (Wire.targetSocketBank target)
    moved <- traverse (swapOne target) sel.destinations
    let
      -- what moved: its run, or a voice layout's gates
      changed = Array.catMaybes (Array.zipWith (\d d' -> if M.placesOf d == M.placesOf d' then Nothing else Just { was: d, now: d' }) sel.destinations moved)
      sel' = sel { destinations = moved }
      releases = map (\c -> maybe "" (\b -> "release-claim polysignal " <> Wire.ownerOf b c.was.jacks) (Wire.targetSocketBank c.was.target)) changed
      applies = Array.mapMaybe (\c -> Wire.destinationEnvelope c.now) changed
    when (Array.null changed) $ Left (t <> " has nothing on either half to swap")
    for_ (Array.find (\a -> a.socket /= sb.socket) applies) \_ -> Left "a swap stays on one device"
    pure
      { rack: Rack.printRack sel'
      , line: "swap " <> t
      , socket: sb.socket
      , bank: maybe "" _.bank (head applies)
      , json: maybe "" _.json (head applies)
      , command: joinWith "\n" (filter (_ /= "") releases <> map (\a -> "apply-polysignal " <> a.json) applies)
      , note: ""
      , also: map (\a -> { bank: a.bank, json: a.json }) (Array.drop 1 applies)
      }
  -- `mute <bank>` / `unmute <bank>`: the bank stays as it is and its jacks go
  -- silent, or play it again
  [ verb, t ] | verb == "mute" || verb == "unmute" -> do
    place <- Rack.placeOf t
    let
      sel = Rack.parseRack rack
      muted = verb == "mute"
    i <- note (t <> " has nothing on it to " <> verb) (findIndex (M.samePlace place) sel.destinations)
    dest <- note (t <> " went missing") (sel.destinations !! i)
    let
      dest' = dest { muted = muted }
      sent = Wire.destinationEnvelope dest'
    pure
      { rack: Rack.printRack (sel { destinations = fromMaybe sel.destinations (updateAt i dest' sel.destinations) })
      , line: verb <> " " <> t
      , socket: maybe "" _.socket sent
      , bank: maybe "" _.bank sent
      , json: maybe "" _.json sent
      , command: maybe "" (\w -> "apply-polysignal " <> w.json) sent
      , note: if muted then "" else rateNote dest'
      , also: []
      }
  -- `convert <bank> lfo` (or `lfot`): a POLYVOLT bank becomes LFOs that
  -- start where its voltages are, every shape at 0, so it is modulated from
  -- the CV dialled in (AC, 2026-10-26). The jacks do not move.
  [ "convert", t, k ] -> do
    place <- Rack.placeOf t
    kind <- case Rack.kindOf k of
      Just kd | kd == M.KLfo || kd == M.KLfoT -> Right kd
      _ -> Left ("convert makes lfo or lfot, not " <> k)
    let sel = Rack.parseRack rack
    i <- note (t <> " has nothing on it to convert") (findIndex (M.samePlace place) sel.destinations)
    d <- note (t <> " went missing") (sel.destinations !! i)
    unless (M.bankKind d.bank == M.KVolt) $ Left ("convert turns volts into LFOs, and " <> t <> " holds " <> Rack.kindKeyword d.bank)
    let
      d' = M.voltToLfo kind d
      sent = Wire.destinationEnvelope d'
    pure
      { rack: Rack.printRack (sel { destinations = fromMaybe sel.destinations (updateAt i d' sel.destinations) })
      , line: printLine d'
      , socket: maybe "" _.socket sent
      , bank: maybe "" _.bank sent
      , json: maybe "" _.json sent
      , command: maybe "" (\w -> "apply-polysignal " <> w.json) sent
      , note: ""
      , also: []
      }
  _ -> rigLine' src rack

-- | A destination with the halves of `target` exchanged: its run if it is on
-- | one half (or refused, if it straddles them), and a voice layout's gates if
-- | they are on one half of `target`.
swapOne :: M.Target -> M.Destination -> Either String M.Destination
swapOne target d = do
  jacks <- if d.target == target then flip d.jacks else Right d.jacks
  bank <- case d.bank of
    M.GVoice vb -> case vb.gates of
      Just g | g.target == target -> (\j -> M.GVoice vb { gates = Just g { jacks = j } }) <$> flip g.jacks
      _ -> Right d.bank
    b -> Right b
  pure d { jacks = jacks, bank = bank }
  where
  flip j
    | j.first + j.count - 1 <= 4 = Right j { first = j.first + 4 }
    | j.first >= 5 = Right j { first = j.first - 4 }
    | otherwise = Left (M.placeWire target j <> " is across both halves; a swap needs the bank in halves (split it first)")

-- | A whole bank as its first four jacks (`split`).
splitDest :: M.Destination -> Either String M.Destination
splitDest d
  | not (M.isWholeBank d.jacks) = Left (M.placeWire d.target d.jacks <> " is already a run of its bank; split takes a whole bank")
  | M.bankKind d.bank == M.KEnv = Left (M.placeWire d.target d.jacks <> ": " <> envelopeWhole)
  | otherwise = case d.target of
      M.Midi _ -> Left "a MIDI channel has no jacks to split"
      M.Virtual _ -> Left "a virtual bank has no jacks to split"
      _ -> Right d
        { jacks = { first: 1, count: 4 }
        , bank = M.takeSlots 4 d.bank
        , mutedSlots = filter (_ < 4) d.mutedSlots
        }

-- | A run as the whole of its bank (`join`), if nothing else is on it.
joinDest :: M.Selene -> M.Destination -> Either String M.Destination
joinDest sel d
  | M.isWholeBank d.jacks = Left (M.placeWire d.target d.jacks <> " is already the whole bank")
  | otherwise = case Array.find (\o -> o.target == d.target && not (M.samePlace o d)) sel.destinations of
      Just o -> Left (M.placeWire o.target o.jacks <> " is on the same bank; free it first (off " <> M.placeWire o.target o.jacks <> ")")
      Nothing -> Right d
        { jacks = M.allJacks
        , bank = M.widenBank d.jacks d.bank
        , mutedSlots = map (\k -> k + d.jacks.first - 1) d.mutedSlots
        }

rigLine' :: String -> String -> Either String RigLine
rigLine' src rack = do
  line <- parseLine src
  sel <- applyLine line (Rack.parseRack rack)
  dest <- note "the line's bank went missing" (Array.find (M.samePlace line) sel.destinations)
  let sent = Wire.destinationEnvelope dest
  pure
    { rack: Rack.printRack sel
    , line: printLine dest
    , socket: maybe "" _.socket sent
    , bank: maybe "" _.bank sent
    , json: maybe "" _.json sent
    , command: maybe "" (\w -> "apply-polysignal " <> w.json) sent
    , note: if dest.muted then "muted: the jacks stay silent until unmute " <> M.placeWire dest.target dest.jacks else rateNote dest
    , also: []
    }

-- | What an FH-2 LFO bank's rates will really be, when some clamp: said once
-- | per distinct rate, as "0.05 Hz runs at 0.1 Hz". For an FH-2 clock or
-- | Euclid bank, why some slots will play silent.
rateNote :: M.Destination -> String
rateNote d = case d.bank of
  M.GLfo slots ->
    let
      clamps = Array.nub (Array.mapMaybe (\sl -> map (\c -> Rack.fmt sl.rate <> " Hz runs at " <> Rack.fmt c <> " Hz") (M.lfoRateOn d.target sl.rate)) slots)
    in
      if Array.null clamps then ""
      else joinWith ", " clamps <> " (the FH-2's LFOs span 0.1 to 10.36 Hz)"
  M.GLfoT slots ->
    let
      silent = Array.nub (Array.mapMaybe (M.lfoTOn d.target) slots)
    in
      if Array.null silent then ""
      else "still where " <> joinWith ", and " silent
  M.GClock slots ->
    let
      silent = Array.nub (Array.mapMaybe (M.clockOn d.target) slots)
    in
      if Array.null silent then ""
      else "silent where " <> joinWith ", and " silent
  M.GEuclid slots ->
    let
      silent = Array.nub (Array.mapMaybe (M.euclidOn d.target) slots)
    in
      if Array.null silent then ""
      else "silent where " <> joinWith ", and " silent
  _ -> ""

-- | `reset <bank>` or `reset <bank> "3 6"`: restart the bank's LFOs, or some
-- | of its slots, together, so each sits at exactly its phase offset. Only an
-- | FH-2 LFO bank has resets (FH-2 firmware per-output LFO reset, by CC; the
-- | daemon's `reset-lfos`). The rack is unchanged: a reset is an act, not a
-- | setting, so a page or a resumed daemon has nothing to replay.
resetOn :: String -> String -> Either String RigLine
resetOn rest rack = do
  let
    t = fromMaybe "" (head (words rest))
    after = trim (Str.drop (Str.length t) (trim rest))
  place <- Rack.placeOf t
  dest <- note (t <> " has nothing on it to reset") (Array.find (M.samePlace place) (Rack.parseRack rack).destinations)
  case dest.bank of
    M.GLfo _ -> pure unit
    other -> Left ("reset restarts a bank's LFOs, and " <> t <> " holds " <> Rack.kindKeyword other)
  wired <- note (t <> " is not a modular bank") (Wire.targetSocketBank place.target)
  when (wired.socket /= "fh2") (Left ("only the FH-2's LFOs can be reset, and " <> t <> " is an ES-9 bank"))
  slots <- slotsOf place.jacks.count after
  let
    listed = if Array.null slots then "" else " \"" <> joinWith " " (map show slots) <> "\""
    -- the daemon counts a bank's outputs 1..8; a run's slot k is its jack
    -- first + k - 1, and a reset of a whole run names every one of its jacks
    onBank' = if Array.null slots then (if M.isWholeBank place.jacks then [] else M.jackNumbers place.jacks)
              else map (\k -> place.jacks.first + k - 1) slots
    commaed = if Array.null onBank' then "" else " " <> joinWith "," (map show onBank')
  pure
    { rack
    , line: "reset " <> t <> listed
    , socket: wired.socket
    , bank: Wire.placeOn wired place.jacks
    , json: ""
    , command: "reset-lfos " <> wired.bank <> commaed
    , note: ""
    , also: []
    }
  where
  slotsOf n s
    | s == "" = Right []
    | otherwise = case Str.stripPrefix (Str.Pattern "\"") s >>= Str.stripSuffix (Str.Pattern "\"") of
        Just inner -> traverse (slot n) (words inner)
        Nothing -> Left ("slots are a quoted list, as in reset fh2cv0 \"1 3\": " <> s)
  slot n w = case Int.fromString w of
    Just k | k >= 1 && k <= n -> Right k
    _ -> Left ("a slot is 1 to " <> show n <> ", not " <> w)

-- ---------------------------------------------------------------------------
-- Modules: a bank's worth, free of any bank (docs/kb/plans/selene-in-tidal.md)
-- ---------------------------------------------------------------------------

-- | A bank as a module: its line without the bank (`euclid # hits 5 # …`),
-- | to be dropped on another bank.
moduleOf :: M.Destination -> String
moduleOf d = case Str.indexOf (Str.Pattern " # ") line of
  Just at -> Rack.kindKeyword d.bank <> Str.drop at line
  Nothing -> Rack.kindKeyword d.bank
  where
  line = printLine d

-- | A module (or a recipe's name, with its terms) put on a bank: the line to
-- | send, `<kind or recipe> <bank> # …`.
onBank :: String -> M.Target -> String
onBank m target = onPlace m target M.allJacks

-- | A module put on a run of a bank's jacks (`fh2_0:5-8`): it fills the run
-- | with the module's first slots.
onPlace :: String -> M.Target -> M.Jacks -> String
onPlace m target jacks = case Str.indexOf (Str.Pattern " ") (trim m) of
  Just at -> Str.take at (trim m) <> " " <> place <> Str.drop at (trim m)
  Nothing -> trim m <> " " <> place
  where
  place = M.placeWire target jacks

-- | The kind of bank a module makes: its first word, a kind or a recipe.
moduleKind :: String -> Maybe M.GenKind
moduleKind m = case head (words m) of
  Just w -> case Recipe.recipeNamed w of
    Just b -> Just b.kind
    Nothing -> Rack.kindOf w
  Nothing -> Nothing

-- | Whether a bank can take a kind: gate banks (the ES-5, an ESX-8GT, the
-- | FH-2's FHX-8GT) take rhythms and clocks; a MIDI channel takes notes; CV
-- | banks, the FH-2's own eight and virtual buses take any.
accepts :: M.Target -> M.GenKind -> Boolean
accepts target kind = case target of
  M.ES9Gt _ -> gates
  M.FH2 n | n > 0 -> gates
  M.Midi _ -> kind == M.KNote || kind == M.KLfo || kind == M.KLfoT || kind == M.KThrow
  -- a throw is played by the rig as a CC, so a MIDI row is its only home
  _ | kind == M.KThrow -> false
  _ | kind == M.KVoice -> isJust (gateBankFor target)
  _ -> true
  where
  gates = kind == M.KEuclid || kind == M.KClock

-- | Whether a run of a bank's jacks can take a kind: as the bank can, and
-- | an envelope bank only whole. Its eight envelopes are the FH-2's first
-- | eight MIDI converters, triggered on channels 1-8 by slot, so half a bank
-- | of them would answer channels nothing plays them on.
acceptsAt :: M.Target -> M.Jacks -> M.GenKind -> Boolean
acceptsAt target jacks kind = accepts target kind && (M.isWholeBank jacks || kind /= M.KEnv)

refusalAt :: M.Target -> M.Jacks -> M.GenKind -> String
refusalAt target jacks kind
  | not (accepts target kind) = refusal target kind
  | otherwise = envelopeWhole

envelopeWhole :: String
envelopeWhole = "an envelope bank takes a whole bank: its envelopes answer MIDI channels 1-8, one per slot"

-- | Why a bank cannot take a kind, for the page to say.
refusal :: M.Target -> M.GenKind -> String
refusal target kind = case target of
  M.Midi _ -> "a MIDI row takes LFOs and throws (sent as CCs) and notes, not " <> M.kindLabel kind
  _ | kind == M.KThrow -> "a throw is played by the rig as a CC, so it goes on a MIDI row (midi1 to midi16)"
  M.ES9Gt _ -> "a gate bank takes rhythms and clocks, not " <> M.kindLabel kind
  M.FH2 n | n > 0 -> "a gate bank takes rhythms and clocks, not " <> M.kindLabel kind
  _ | kind == M.KVoice -> "voices need the FH-2's converters: its own jacks or an FHX-8CV, with the FHX-8GT for their gates"
  _ -> "a gate bank takes rhythms and clocks, not " <> M.kindLabel kind

-- | A module as the setting it makes, in its module form: a recipe or a kind
-- | with terms, expanded on a fresh bank, then written back without the
-- | bank. The same setting so has the same text (and rebus) in the drawer,
-- | on a row, and wherever it is dropped. Nothing for what is not a module.
moduleText :: String -> Maybe String
moduleText m = do
  kind <- moduleKind m
  let
    bank = case kind of
      M.KEuclid -> M.ES9Gt 0
      M.KClock -> M.ES9Gt 0
      M.KNote -> M.ES9Cv 0
      -- voices need the FH-2's converters
      M.KVoice -> M.FH2 0
      _ -> M.ES9Main
    isRecipe = maybe false (\w -> Recipe.recipeNamed w /= Nothing) (head (words m))
    src = onBank m bank <> (if isRecipe then "" else " # fresh")
  line <- hush (parseLine src)
  sel <- hush (applyLine line { destinations: [] })
  moduleOf <$> Array.find (\d -> d.target == bank) sel.destinations
