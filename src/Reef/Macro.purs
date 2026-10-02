-- | macro-tidal — the arrangement layer.
-- |
-- | Where the instruments each play ONE saved form at a time, the macro layer
-- | SEQUENCES those forms over macro-time AND transforms them in place. A lane
-- | is a mini-notation string whose atoms are *form names* (Amphora labels),
-- | each optionally carrying a stack of `#`-modifiers:
-- |
-- |   "contemplative" # scale <"F# lydian dominant" "G major">
-- |   "bopping along" ~ <midnight drift>
-- |
-- | The shell resolves each name to a saved setup, loads it into the instrument
-- | at a bar-quantized step boundary, and applies the resolved modifiers. This
-- | unifies two ideas: sequencing forms (the arrangement) and transforming them
-- | (the workbench transforms) — a transform is just a modifier on a lane atom.
-- |
-- | Subset (Slice: transforms):
-- |
-- |   * space-separated STEPS divide the macro-cycle equally
-- |   * a form name with spaces is double-quoted ("bopping along")
-- |   * `~` is a rest — the instrument falls silent for that step
-- |   * `<a b c>` alternates a form (or a modifier ARG) once per macro-CYCLE
-- |   * `# verb arg` attaches a transform; the arg may itself be `<…>`
-- |
-- | The language is DOMAIN-AGNOSTIC: it yields `(verb, arg)` string pairs and
-- | does not know what `scale`/`fast`/`bass` mean — the shell interprets each
-- | verb for the target instrument. Weights (`@`), replication (`*`) and the
-- | other lanes come later. The parser is one-level (no nested `<>`).
-- |
-- | Moved from Triggerfish (`Triggerfish.Macro`) on 2026-10-02 so the rig can read Vetula's
-- | cards (docs/kb/plans/gpl-boundary-review.md, step 3); Triggerfish re-exports it.
module Reef.Macro
  ( module ReExport
  , Form(..)
  , Mod
  , Step
  , Cell(..)
  , ResolvedMod
  , parseLane
  , resolveStep
  , laneFormNames
  , stepLabel
  ) where

import Prelude

import Data.Array (concatMap, filter, index, length, uncons)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.String.Common (joinWith)
import Reef.PatternArg (PatternArg(..), tokenize) as ReExport
import Reef.PatternArg (PatternArg(..), mkArg, printArg, sampleArg, stripAngle, tokenize, unq, unquote)

-- Which form a step plays.
data Form
  = FName String        -- a form name (an Amphora label)
  | FRest               -- `~` — silent this step
  | FAlt (Array String) -- `<a b c>` — a different form each macro-cycle ("~" = rest)

derive instance Eq Form

-- One `# verb arg` modifier (an unresolved transform). The argument is a shared
-- `PatternArg` (`Reef.PatternArg`) — the same arg type the Vetula card uses,
-- so both scales parse one grammar. A bare `Lit`, or a `Pat` (a `<…>` alternation, or
-- a quoted multi-word / mini-notation source).
type Mod = { verb :: String, arg :: PatternArg }

-- A step: which form, plus its transform stack.
type Step = { form :: Form, mods :: Array Mod }

-- A modifier once its cycle-dependent argument has been chosen.
type ResolvedMod = { verb :: String, arg :: String }

-- What a step resolves to on a given cycle: silence, or a named form to load
-- with its resolved transforms.
data Cell = Quiet | Load String (Array ResolvedMod)

derive instance Eq Cell

-- Parse a lane into ordered steps. Grammar: `step := form (# verb arg)*`.
parseLane :: String -> Array Step
parseLane = go <<< tokenize
  where
  go toks = case uncons toks of
    Nothing -> []
    Just { head: "#", tail } -> go tail  -- stray leading #, skip
    Just { head, tail } ->
      let r = collectMods tail
      in [ { form: parseForm head, mods: r.mods } ] <> go r.rest

-- Consume `(# verb arg)*` from the front of the token stream; stop at the next
-- form token (or the end). Returns the mods and the remaining tokens.
collectMods :: Array String -> { mods :: Array Mod, rest :: Array String }
collectMods toks = case uncons toks of
  Just { head: "#", tail } -> case uncons tail of
    Just { head: verb, tail: afterVerb } -> case uncons afterVerb of
      Just { head: argTok, tail: afterArg } ->
        let r = collectMods afterArg
        in r { mods = [ { verb, arg: mkArg argTok } ] <> r.mods }
      Nothing -> { mods: [ { verb, arg: Lit "" } ], rest: [] }  -- `# verb` (no arg)
    Nothing -> { mods: [], rest: [] }  -- trailing `#`
  _ -> { mods: [], rest: toks }

parseForm :: String -> Form
parseForm t = case unquote t of
  Just name -> FName name
  Nothing ->
    if t == "~" then FRest
    else case stripAngle t of
      Just inner -> FAlt (map unq (tokenize inner))
      Nothing -> FName t

-- Resolve a step at `stepIdx` (reduced mod the step count) on macro-cycle
-- `cycleIdx`. Alternation of forms and of modifier args both pick by the cycle.
resolveStep :: Array Step -> Int -> Int -> Cell
resolveStep steps stepIdx cycleIdx = case index steps stepIdx of
  Nothing -> Quiet
  Just step -> case resolveForm step.form cycleIdx of
    Nothing -> Quiet
    Just name -> Load name (map (resolveMod cycleIdx) step.mods)

resolveForm :: Form -> Int -> Maybe String
resolveForm form cycleIdx = case form of
  FName n -> Just n
  FRest -> Nothing
  FAlt inner ->
    if length inner == 0 then Nothing
    else case index inner (cycleIdx `mod` length inner) of
      Just s -> if s == "~" then Nothing else Just s
      Nothing -> Nothing

resolveMod :: Int -> Mod -> ResolvedMod
resolveMod cycleIdx m = { verb: m.verb, arg: sampleArg cycleIdx m.arg }

-- Every FORM name referenced (for unknown-name checking / the palette). Modifier
-- args are excluded — they are not library forms.
laneFormNames :: Array Step -> Array String
laneFormNames = concatMap (formNames <<< _.form)
  where
  formNames = case _ of
    FName n -> [ n ]
    FRest -> []
    FAlt inner -> filter (_ /= "~") inner

-- Render a step back to its source form (for the live readout).
stepLabel :: Step -> String
stepLabel step = formLabel step.form <> joinWith "" (map modLabel step.mods)
  where
  formLabel = case _ of
    FName n -> quoteIfSpace n
    FRest -> "~"
    FAlt inner -> "<" <> joinWith " " (map quoteIfSpace inner) <> ">"
  modLabel m = " # " <> m.verb <> " " <> printArg m.arg
  quoteIfSpace s = if contains (Pattern " ") s then "\"" <> s <> "\"" else s
