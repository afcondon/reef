-- | `Reef.Vetula.Lepidoptera` — the Perform surface as one transferable document.
-- |
-- | Where `Reef.Vetula.NoteText` serialises a single *progression* (one caught-chord path
-- | as `note "<…>"`), this serialises the whole **Perform surface**: the set of
-- | parallel voices, each a source-reference + a mini-notation head + a transform
-- | stack + a terminal. That makes a live Perform configuration a named, saveable
-- | *scene* — the Amphora-storable "the file is the music" — and the atom a future
-- | macro/arrangement layer sequences (verse / chorus / head-fugue).
-- |
-- | The document shape (Tier 2 of the convergence — see the Lepidoptera memory):
-- |
-- |   -- vetula perform · C major · 3 voices
-- |   source A "<[c5,e5,g5] [a4,c5,e5] [d5,f5,a5] [g4,b4,d5]>"
-- |   source B "<[c5,e5,g5,b5,d6] …>"
-- |   ch1 A "0 1 2 3" # voice open # strum 14
-- |   ch2 B "0 1 2 3" # arpup 4 # every 4
-- |   ch5 - "" # transpose 12 # out rig
-- |
-- | Two principles from the design conversation are baked in:
-- |
-- |   * **Sources are plural and named; sharing is a coincidence of reference.**
-- |     A voice names a source; two voices that name the same one share harmony,
-- |     and re-harmonising = editing one table entry. Distinct/inline sources give
-- |     the "different chords per voice" cases (plain vs substitution sibling,
-- |     different keys). "Set all at once" is a repoint, never a baseline share.
-- |   * **One grammar across the ecosystem.** The `"head" # verb arg` lane is
-- |     parsed by `Reef.Macro.parseLane` verbatim (the arrangement layer's
-- |     grammar). Vetula owns only the *vocabulary* — the `(verb,arg) -> PerfFx`
-- |     table — which is the right seam. This forced the (b)-tasteful choice:
-- |     arp's direction fuses into the verb (`arpup`/`arpdown`/`arpupdown`) so
-- |     every layer is a single-arg mod, and `every N` is a trailing mod that
-- |     binds to the preceding layer. `out <term>` / `mute` ride the same grammar.
-- |
-- | Total + lenient (Selene `Source.purs` discipline): unknown mods drop, partial
-- | ones fall back to defaults, values clamp to the chip-nudge ranges. Aim:
-- |   parsePerform (performSource doc) == doc  (modulo trim + source-name canon).
-- |
-- | NOTE (follow-up): this imports the Perform types from `Vetula.App`. Wiring a
-- | save button *into* App will need App to import this — a cycle — so first
-- | extract the pure Perform types (PerfFx/Layer/PerfTerm/…) into a shared module.
-- | Deferred deliberately; tonight is additive and leaves App untouched.
-- |
-- | Moved from Triggerfish (`Vetula.Lepidoptera`) on 2026-10-02 so the rig can read Vetula's
-- | cards (docs/kb/plans/gpl-boundary-review.md, step 3); Triggerfish re-exports it.
module Reef.Vetula.Lepidoptera
  ( NamedSource
  , VoiceEntry
  , VoiceSpec
  , PerfDoc
  , performSource
  , printAsRecord
  , parsePerform
  , docFromVoices
  , roundTrips
  , printCard
  , parseCard
  ) where

import Prelude

import Data.Array (drop, filter, find, index, length, mapMaybe, mapWithIndex, null, snoc, updateAt, (!!))
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Number as Number
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), contains, stripPrefix)
import Data.String.Common (joinWith, split, trim)
import Reef.Macro (Form(..), parseLane, tokenize)
import Reef.PatternArg (PatternArg(..), argSrc, printArg)
import Reef.Vetula.PerformTypes (ArpDir(..), Layer, PerfFx(..), PerfSel(..), PerfTerm(..), When(..), mkLayer, printArpDir, termShort)
import Reef.Vetula.NoteText (parseProgression, tidalNoteName)

-- | A named chord set the voices reference. `chords` are note-lists in *stored
-- | order* (NOT sorted — voicing order drives arp direction, so it must survive).
type NamedSource = { name :: String, chords :: Array (Array Int) }

-- | One voice of the surface — the serialisable core of a `PerfBox` (drop the
-- | live-trace provenance / glyph; keep the harmonic content by reference).
type VoiceEntry =
  { channel :: Int
  , source  :: Maybe String   -- name into the source table; Nothing = sourceless
  , seqText :: String         -- the mini-notation head (indices into the source)
  , stack   :: Array Layer    -- the transform stack (fx + when)
  , term    :: PerfTerm       -- terminal sink
  , muted   :: Boolean
  }

-- | A whole Perform surface as one document.
type PerfDoc =
  { key     :: String
  , sources :: Array NamedSource
  , voices  :: Array VoiceEntry
  }

-- ============================================================================
-- Print
-- ============================================================================

performSource :: PerfDoc -> String
performSource doc = joinWith "\n" (header <> map printSource doc.sources <> map printVoice doc.voices)
  where
  header = [ "-- vetula perform · " <> doc.key <> " · " <> show (length doc.voices) <> " voices" ]

printSource :: NamedSource -> String
printSource s = "source " <> s.name <> " " <> quote ("<" <> joinWith " " (map bracket s.chords) <> ">")
  where
  bracket c = "[" <> joinWith "," (map tidalNoteName c) <> "]"

printVoice :: VoiceEntry -> String
printVoice v = joinWith " " ([ "ch" <> show v.channel, srcTok, quote v.seqText ] <> hashed directives)
  where
  srcTok = fromMaybe "-" v.source
  directives = bind v.stack layerDirs <> termDir <> muteDir
  termDir = case v.term of
    TMidi -> []
    t -> [ "out " <> termShort t ]
  muteDir = if v.muted then [ "mute" ] else []
  -- interleave a `#` before each directive: [d1,d2] -> ["#",d1,"#",d2]
  hashed ds = bind ds \d -> [ "#", d ]

-- | A layer prints to its fx directive, plus an `every N` directive when gated.
layerDirs :: Layer -> Array String
layerDirs lyr = [ fxDir lyr.fx ] <> whenDir lyr.when

whenDir :: When -> Array String
whenDir = case _ of
  Always -> []
  Every n -> [ "every " <> show n ]
  Prob p -> [ "prob " <> show p ]
  AfterBar n -> [ "afterbar " <> show n ]
  -- two args → one quoted token, since Macro's grammar is strictly one arg per mod.
  Whenmod n r -> [ "whenmod " <> quote (show n <> " " <> show r) ]

-- | The single-arg mod vocabulary. Arp's direction is FUSED into the verb so the
-- | mod stays `verb arg` (Macro's grammar is strictly one arg). Use plain `show`
-- | (a leading '+' breaks `Int.fromString`).
fxDir :: PerfFx -> String
fxDir = case _ of
  Transpose arg -> "transpose " <> printArg arg
  Octave arg -> "oct " <> printArg arg
  Slow n -> "slow " <> show (max 1 n)
  Fast n -> "fast " <> show (max 1 n)
  Voice arg -> "voice " <> printArg arg
  Select (High arg) -> "top " <> printArg arg
  Select (Low arg) -> "bottom " <> printArg arg
  Arpg dir r -> "arp" <> printArpDir dir <> " " <> show r
  ArpP src -> "arp " <> quote src   -- the figure is quoted so its spaces survive as one arg
  Strum ms -> "strum " <> show ms

quote :: String -> String
quote x = "\"" <> x <> "\""

-- | Tier 3 — the same document as a record literal, mirroring the ecosystem
-- | idiom (`Odonus.Lepidoptera` `odonusPatch <name> { … }`): an outer record
-- | shell with `field:` labels whose list ELEMENTS are the compact directive
-- | lines (`printSource` / `printVoice`) — the flat form of `performSource` is
-- | exactly the element grammar, so this is a shell over it, nothing re-derived.
-- | This is the shape the A5 cross-instrument library manager reads. Print-only
-- | for now; the elements already parse via `parsePerform`, so a `parseRecord`
-- | is just the shell. (Head keyword `vetulaScene` — a saved surface IS a scene.)
printAsRecord :: String -> PerfDoc -> String
printAsRecord name doc =
  joinWith "\n"
    [ "vetulaScene " <> show name
    , "  { key: " <> show doc.key
    , "  , sources:"
    , "      [ " <> joinWith "\n      , " (map printSource doc.sources) <> " ]"
    , "  , voices:"
    , "      [ " <> joinWith "\n      , " (map printVoice doc.voices) <> " ]"
    , "  }"
    ]

-- ============================================================================
-- Parse — total + lenient
-- ============================================================================

parsePerform :: String -> PerfDoc
parsePerform text =
  { key: findKey lines
  , sources: mapMaybe parseSourceLine kept
  , voices: mapMaybe parseVoiceLine kept
  }
  where
  lines = map trim (split (Pattern "\n") text)
  kept = filter (\l -> l /= "" && not (isComment l)) lines
  isComment l = stripPrefix (Pattern "--") l /= Nothing

-- | Pull the key out of the `-- vetula perform · KEY · N voices` header.
findKey :: Array String -> String
findKey lines = fromMaybe "" (find isHeader lines >>= keyOf)
  where
  isHeader l = stripPrefix (Pattern "-- vetula perform") l /= Nothing
  keyOf l = map trim (index (split (Pattern "·") l) 1)

-- Drop record-literal punctuation ("[", "]", ",") so the SAME line parsers handle
-- both the flat `performSource` form and the `printAsRecord` form (whose elements
-- are wrapped `[ … , … ]`). Bare brackets never occur in our grammar except as
-- record punctuation — mini-notation brackets live INSIDE quoted heads, which
-- tokenize keeps intact. This is what makes `parsePerform` recall a saved scene.
stripPunct :: Array String -> Array String
stripPunct = filter (\t -> t /= "[" && t /= "]" && t /= ",")

parseSourceLine :: String -> Maybe NamedSource
parseSourceLine l = case stripPunct (tokenize l) of
  toks | (toks !! 0) == Just "source" ->
    case toks !! 1 of
      Just name -> Just { name, chords: parseProgression l }
      Nothing -> Nothing
  _ -> Nothing

parseVoiceLine :: String -> Maybe VoiceEntry
parseVoiceLine l =
  let toks = stripPunct (tokenize l)
  in case toks !! 0 >>= parseCh of
       Nothing -> Nothing
       Just channel ->
         let src = case toks !! 1 of
               Just "-" -> Nothing
               Just s -> Just s
               Nothing -> Nothing
             laneStr = joinWith " " (drop 2 toks)
             folded = foldMods (firstStepMods laneStr)
         in Just
              { channel
              , source: src
              , seqText: firstStepHead laneStr
              , stack: folded.stack
              , term: folded.term
              , muted: folded.muted
              }

parseCh :: String -> Maybe Int
parseCh t = stripPrefix (Pattern "ch") t >>= Int.fromString

-- The head (mini-notation) of the lane's first (and, for a Perform voice, only)
-- step. Empty when the lane is empty or the head is a rest/alt (kept lenient).
firstStepHead :: String -> String
firstStepHead laneStr = case parseLane laneStr of
  steps -> case steps !! 0 of
    Just step -> formHead step.form
    Nothing -> ""

firstStepMods :: String -> Array { verb :: String, arg :: PatternArg }
firstStepMods laneStr = case (parseLane laneStr) !! 0 of
  Just step -> step.mods
  Nothing -> []

formHead :: Form -> String
formHead = case _ of
  FName n -> n
  FRest -> "~"
  FAlt inner -> "<" <> joinWith " " inner <> ">"

-- | Fold the step's mods into (stack, term, muted). `every` binds to the layer
-- | most recently pushed; `out`/`mute` set the terminal/mute; a recognised fx
-- | verb pushes a layer; anything else drops (lenient).
foldMods :: Array { verb :: String, arg :: PatternArg } -> { stack :: Array Layer, term :: PerfTerm, muted :: Boolean }
foldMods = foldl step { stack: [], term: TMidi, muted: false }
  where
  step acc m =
    let arg = argSrc m.arg
    in case m.verb of
         "every" -> acc { stack = setLastWhen (Every (max 1 (argInt 2 arg))) acc.stack }
         "prob" -> acc { stack = setLastWhen (Prob (argNum 0.5 arg)) acc.stack }
         "afterbar" -> acc { stack = setLastWhen (AfterBar (max 0 (argInt 8 arg))) acc.stack }
         "whenmod" -> acc { stack = setLastWhen (whenmodOf arg) acc.stack }
         "out" -> acc { term = parseTerm arg }
         "mute" -> acc { muted = true }
         _ -> case fxOfVerb m.verb arg of
                Just fx -> acc { stack = snoc acc.stack (mkLayer fx) }
                Nothing -> acc

setLastWhen :: When -> Array Layer -> Array Layer
setLastWhen w stack =
  let n = length stack
  in case stack !! (n - 1) of
       Just lyr -> fromMaybe stack (updateAt (n - 1) (lyr { when = w }) stack)
       Nothing -> stack

fxOfVerb :: String -> String -> Maybe PerfFx
fxOfVerb verb arg = case verb of
  "transpose" -> Just (Transpose (mkArgL arg))
  "trans" -> Just (Transpose (mkArgL arg))
  "oct" -> Just (Octave (mkArgL arg))
  "octave" -> Just (Octave (mkArgL arg))
  "8ve" -> Just (Octave (mkArgL arg))
  "slow" -> Just (Slow (max 1 (argInt 4 arg)))
  "fast" -> Just (Fast (max 1 (argInt 2 arg)))
  "voice" -> Just (Voice (mkArgL arg))
  "top" -> Just (Select (High (mkArgL arg)))
  "bottom" -> Just (Select (Low (mkArgL arg)))
  "arp" -> Just (ArpP arg)   -- explicit index figure (dir-less); Macro un-quotes the arg
  "arpup" -> Just (Arpg ArpUp (clamp 1 16 (argInt 4 arg)))
  "arpdown" -> Just (Arpg ArpDown (clamp 1 16 (argInt 4 arg)))
  "arpupdown" -> Just (Arpg ArpUpDown (clamp 1 16 (argInt 4 arg)))
  "strum" -> Just (Strum (clamp 0 80 (argInt 14 arg)))
  _ -> Nothing

parseTerm :: String -> PerfTerm
parseTerm = case _ of
  "rig" -> TRig
  "odo" -> TOdo
  _ -> TMidi

-- | Reconstruct a `PatternArg` from a Macro arg's SOURCE string. `firstStepMods` now
-- | yields a real `PatternArg` (`Reef.Macro` shares the type), but the doc-layer
-- | fold flattens everything to a source string first (`argSrc`) and re-derives the
-- | literal/pattern split here: an arg carrying a space or mini-notation punctuation is
-- | a pattern, anything else a bare literal. (The LIVE text hatch keeps quotes and is
-- | exact; this is the document layer's best effort.)
mkArgL :: String -> PatternArg
mkArgL s = if patterned then Pat s else Lit s
  where
  patterned = contains (Pattern " ") s
    || contains (Pattern "<") s || contains (Pattern "[") s
    || contains (Pattern "~") s || contains (Pattern "*") s || contains (Pattern "(") s

-- | One integer token, lenient: strip a leading '+' (which `fromString` rejects),
-- | fall back to `def` on anything non-numeric.
argInt :: Int -> String -> Int
argInt def s = fromMaybe def (Int.fromString (fromMaybe s (stripPrefix (Pattern "+") s)))

-- | One number token (for `prob`), lenient.
argNum :: Number -> String -> Number
argNum def s = fromMaybe def (Number.fromString s)

-- | `whenmod`'s two ints, packed into one quoted arg ("8 1") by `whenDir`. Lenient.
whenmodOf :: String -> When
whenmodOf s = case filter (_ /= "") (split (Pattern " ") (trim s)) of
  [ a, b ] -> Whenmod (argInt 8 a) (argInt 1 b)
  _ -> Whenmod 8 1

-- ============================================================================
-- Build a document from live voices. Takes a NEUTRAL spec (raw chords, not a
-- PerfBox) so this module stays free of App / SavedSeq — App extracts
-- `map _.notes s.events` from each box at the call site. The inverse (minting
-- SavedSeqs with synthesised events/glyphs from a parsed doc) lives App-side.
-- ============================================================================

-- | The serialisable core of a live voice, decoupled from `PerfBox` — App maps
-- | its boxes to these (a box's chords = `map _.notes s.events`).
type VoiceSpec =
  { channel :: Int
  , chords  :: Array (Array Int)   -- source material; empty = sourceless
  , seqText :: String
  , stack   :: Array Layer
  , term    :: PerfTerm
  , muted   :: Boolean
  }

-- | **One card as one line**, the form a card takes on the stage and in
-- | Limulus (`v3 $ …`, docs/kb/plans/text-on-the-stage.md): a voice line whose
-- | source is the card's own chords, quoted, where a scene names a shared
-- | source:
-- |
-- |     ch3 "<[c4,e4,g4] [a3,c4,e4]>" "0 1 2 3" # arp up 4
-- |
-- | `-` for a card with no chords. It reads with the scene's own line parser,
-- | the quoted chords landing where a source name would.
printCard :: VoiceSpec -> String
printCard spec = printVoice
  { channel: spec.channel
  , source: if null spec.chords then Nothing else Just (quote ("<" <> joinWith " " (map bracket spec.chords) <> ">"))
  , seqText: trim spec.seqText
  , stack: spec.stack
  , term: spec.term
  , muted: spec.muted
  }
  where
  bracket c = "[" <> joinWith "," (map tidalNoteName c) <> "]"

parseCard :: String -> Maybe VoiceSpec
parseCard line = parseVoiceLine (trim line) <#> \v ->
  { channel: v.channel
  , chords: maybe [] parseProgression v.source
  , seqText: v.seqText
  , stack: v.stack
  , term: v.term
  , muted: v.muted
  }

docFromVoices :: String -> Array VoiceSpec -> PerfDoc
docFromVoices key specs =
  { key
  , sources: named
  , voices: map toVoice specs
  }
  where
  -- distinct non-empty chord sets across all voices, in first-seen order → named;
  -- two voices with the same chords dedup to ONE source (sharing = co-reference)
  distinct = foldl (\acc c -> if null c || isJustArr (find (_ == c) acc) then acc else snoc acc c) [] (map _.chords specs)
  named = mapWithIndex (\i c -> { name: srcName i, chords: c }) distinct
  toVoice spec =
    { channel: spec.channel
    , source: if null spec.chords then Nothing else map _.name (find (\ns -> ns.chords == spec.chords) named)
    , seqText: trim spec.seqText
    , stack: spec.stack
    , term: spec.term
    , muted: spec.muted
    }

isJustArr :: forall a. Maybe a -> Boolean
isJustArr = case _ of
  Just _ -> true
  Nothing -> false

-- Source names: A…Z, then S27, S28, … (rarely reached — few voices).
srcName :: Int -> String
srcName i = fromMaybe ("S" <> show i) (index letters i)
  where
  letters =
    [ "A","B","C","D","E","F","G","H","I","J","K","L","M"
    , "N","O","P","Q","R","S","T","U","V","W","X","Y","Z" ]

-- ============================================================================
-- Round-trip self-check (type-checks that Eq is available; run in a harness).
-- ============================================================================

-- | The reconciliation invariant, as a Boolean over a document. `performSource`
-- | then `parsePerform` should return the same document (source names are already
-- | canonical A…Z; seqText is trimmed on the way in). Not run at build time —
-- | drop tokens on the surface and eyeball, or wire a test harness.
roundTrips :: PerfDoc -> Boolean
roundTrips doc = parsePerform (performSource doc) == doc
