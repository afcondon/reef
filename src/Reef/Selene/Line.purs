-- | **Selene as a line**: one line addresses one bank (a destination's eight
-- | slots) and changes only what it names (docs/kb/plans/selene-in-tidal.md).
-- |
-- |     lfo es9main # rate "0.5 1 2 4" # phase "0 0.25"
-- |     euclid es9gt0 # hits 5
-- |     clock es9gt1 # div 1/4 # mult "1 2 3 4 5 6 7 8"
-- |
-- | `<kind> <bank>`, then `# <parameter> <value>` terms. A value is one token
-- | (every slot) or a quoted list, which sets the slots in turn and wraps, as
-- | Tidal's lists do. Parameters are the model's own field names, with the
-- | rack's short names beside them (`lvl`, `hits`, `acc`, `x`, `pw`, `ph`,
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
  ) where

import Prelude

import Data.Array (all, filter, findIndex, foldM, head, length, mapWithIndex, snoc, uncons, updateAt, (!!))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.String as Str
import Data.String.Common (joinWith, trim)
import Reef.Selene.Model as M
import Reef.Selene.Rack as Rack
import Reef.Selene.Block as Block
import Reef.Selene.Wire as Wire

-- | A line, read: the kind, the bank it addresses, and its terms in order.
type Line =
  { kind :: M.GenKind
  -- | a block (Reef.Selene.Block) in place of the kind: it sets the whole bank
  , block :: Maybe String
  , target :: M.Target
  , terms :: Array { param :: String, values :: Array String }
  }

-- | Read a line. Errors name what was not understood.
parseLine :: String -> Either String Line
parseLine src = do
  let parts = map trim (Str.split (Str.Pattern "#") src)
  { head: h, tail: rest } <- note "an empty line" (uncons parts)
  case words h of
    [ k, t ] -> do
      let named = Block.blockNamed k
      kind <- note ("not a kind of polysignal or a block: " <> k <> " (lfo, euclid, clock, note, env; or a block: " <> joinWith ", " (map _.name Block.blocks) <> ")")
        (case named of
          Just b -> Just b.kind
          Nothing -> Rack.kindOf k)
      terms <- foldM (\acc part -> snoc acc <$> term part) [] (filter (_ /= "") rest)
      pure { kind, block: map _.name named, target: Rack.parseTarget t, terms }
    _ -> Left ("a line starts with a kind and a bank, as in lfo es9main: " <> h)
  where
  term part = case Str.indexOf (Str.Pattern " ") part of
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
    existing = findIndex (\d -> d.target == line.target) sel.destinations
    current = case existing >>= (sel.destinations !! _) of
      Just d | M.bankKind d.bank == kindOfLine -> d.bank
      _ -> M.freshBank kindOfLine
    kindOfLine = line.kind
  -- a block sets the whole bank from its own parameters; the line's other
  -- terms then apply to that, as plain parameters
  { start, terms } <- case line.block >>= Block.blockNamed of
    Just b -> (\x -> { start: x.bank, terms: x.rest }) <$> Block.expand b line.terms
    Nothing -> pure { start: current, terms: line.terms }
  bank <- foldM applyTerm start terms
  let dest = { target: line.target, range: Nothing, bank }
  pure case existing of
    Just i -> sel { destinations = fromMaybe sel.destinations (updateAt i (keepRange i dest) sel.destinations) }
    Nothing -> sel { destinations = snoc sel.destinations dest }
  where
  keepRange i d = d { range = (sel.destinations !! i) >>= _.range }

-- | One term over a bank: the parameter set on every slot from the values,
-- | in turn and wrapping.
applyTerm :: M.GenBank -> { param :: String, values :: Array String } -> Either String M.GenBank
applyTerm bank t = case bank of
  M.GLfo slots -> M.GLfo <$> each slots \sl v -> case t.param of
    p | p == "rate" -> (\x -> sl { rate = x }) <$> number v
      | p == "phase" || p == "ph" -> (\x -> sl { phase = x }) <$> number v
      | p == "level" || p == "lvl" -> (\x -> sl { level = x }) <$> number v
      | p == "sin" -> (\x -> sl { sin = x }) <$> number v
      | p == "sqr" -> (\x -> sl { sqr = x }) <$> number v
      | p == "tri" -> (\x -> sl { tri = x }) <$> number v
      | p == "saw" -> (\x -> sl { saw = x }) <$> number v
      | p == "rnd" -> (\x -> sl { rnd = x }) <$> number v
      | p == "nse" -> (\x -> sl { nse = x }) <$> number v
    _ -> unknown "lfo" "rate phase level sin sqr tri saw rnd nse"
  M.GEuclid slots -> M.GEuclid <$> each slots \sl v -> case t.param of
    p | p == "beats" || p == "hits" -> (\x -> sl { beats = x }) <$> integer v
      | p == "steps" -> (\x -> sl { steps = x }) <$> integer v
      | p == "rate" -> (\x -> sl { rate = x }) <$> integer v
      | p == "acc" || p == "accentRate" -> (\x -> sl { accentRate = x }) <$> integer v
    _ -> unknown "euclid" "hits steps rate acc"
  M.GClock slots -> M.GClock <$> each slots \sl v -> case t.param of
    p | p == "div" || p == "base" -> Right (sl { base = Rack.parseBase v })
      | p == "mult" || p == "x" || p == "multiplier" -> (\x -> sl { multiplier = x }) <$> integer v
      | p == "pw" || p == "pulseWidth" -> (\x -> sl { pulseWidth = x }) <$> integer v
      | p == "phase" || p == "ph" -> (\x -> sl { phase = x }) <$> integer v
    _ -> unknown "clock" "div mult pw phase"
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
  where
  each :: forall s. Array s -> (s -> String -> Either String s) -> Either String (Array s)
  each slots f = foldM (\acc x -> snoc acc <$> x) [] (mapWithIndex (\i sl -> f sl (valueAt i)) slots)
  valueAt i = fromMaybe "" (t.values !! (i `mod` length t.values))
  unknown :: forall s. String -> String -> Either String s
  unknown kind known = Left (kind <> " has no " <> t.param <> " (it has " <> known <> ")")
  number v = note (t.param <> " wants a number, not " <> v) (Number.fromString v)
  integer v = note (t.param <> " wants a whole number, not " <> v) (Int.fromString v)

-- | The parameters a bank's kind takes, as `printLine` names them.
paramsOf :: M.GenKind -> Array String
paramsOf = case _ of
  M.KLfo -> [ "rate", "phase", "level", "sin", "sqr", "tri", "saw", "rnd", "nse" ]
  M.KEuclid -> [ "hits", "steps", "rate", "acc" ]
  M.KClock -> [ "div", "mult", "pw", "phase" ]
  M.KNote -> [ "note" ]
  M.KEnv -> [ "a", "d", "s", "r", "depth", "vel", "time", "rnd", "ashape", "dshape", "rshape" ]

-- | A bank as a line: the kind's main parameters always (`mainOf`), and any
-- | other whose slots are not all at the kind's fresh value; eight equal
-- | values printed as one. The main ones are said even at their fresh values,
-- | since a line changes only what it names and a printed bank is read as the
-- | whole of it.
printLine :: M.Destination -> String
printLine d =
  joinWith " # " ([ Rack.kindKeyword d.bank <> " " <> M.targetWire d.target ] <> terms)
  where
  fresh = M.freshBank (M.bankKind d.bank)
  terms = Array.catMaybes (map term (paramsOf (M.bankKind d.bank)))
  term p =
    let vs = valuesOf p d.bank
    in if vs == valuesOf p fresh && not (Array.elem p (mainOf (M.bankKind d.bank))) then Nothing else Just (p <> " " <> value vs)
  value vs = case head vs of
    Just v | all (_ == v) vs -> v
    _ -> "\"" <> joinWith " " vs <> "\""

-- | What a bank is, at a glance: printed whatever their values.
mainOf :: M.GenKind -> Array String
mainOf = case _ of
  M.KLfo -> [ "rate", "phase" ]
  M.KEuclid -> [ "hits", "steps" ]
  M.KClock -> [ "div", "mult" ]
  M.KNote -> [ "note" ]
  M.KEnv -> [ "a", "d", "s", "r" ]

valuesOf :: String -> M.GenBank -> Array String
valuesOf p = case _ of
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
    "rate" -> sl.rate
    _ -> sl.accentRate) s
  M.GClock s -> map (\sl -> case p of
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

words :: String -> Array String
words = filter (_ /= "") <<< Str.split (Str.Pattern " ") <<< trim

-- | What the rig does with a `selene $` line (Architeuthis's Handler): the
-- | rack it keeps on the stage (`selene/rack`) with the line applied, and the
-- | one bank the line touched, ready for its daemon. `socket` is "" when that
-- | bank is not on the modular (a MIDI or virtual target): kept, not sent.
type RigLine = { rack :: String, line :: String, socket :: String, bank :: String, json :: String }

rigLine :: String -> String -> Either String RigLine
rigLine src rack = do
  line <- parseLine src
  sel <- applyLine line (Rack.parseRack rack)
  dest <- note "the line's bank went missing" (Array.find (\d -> d.target == line.target) sel.destinations)
  let sent = Wire.destinationEnvelope dest
  pure
    { rack: Rack.printRack sel
    , line: printLine dest
    , socket: maybe "" _.socket sent
    , bank: maybe "" _.bank sent
    , json: maybe "" _.json sent
    }
