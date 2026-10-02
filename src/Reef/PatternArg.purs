-- | `Reef.PatternArg` — the shared verb-ARGUMENT vocabulary, one grammar
-- | across scales (see `docs/DESIGN-tidal-scaling.md` §3–6, step 6).
-- |
-- | Both scales of Tidal in Triggerfish take the SAME kind of argument on a
-- | `# verb arg` layer:
-- |
-- |   * MICRO — a Vetula Perform card: `transpose "0 7 <5 3>"`, `voice <open drop2>`.
-- |   * MACRO — a `Reef.Macro` arrangement lane: `# scale <"F# lydian" "G major">`.
-- |
-- | An argument is one uniform `PatternArg` (decision B, §4.4): a bare LITERAL
-- | (`7`, `open`, `up`) or a quoted / angle-delimited PATTERN (`"0 7"`, `<open drop2>`).
-- | Both carry their SOURCE text; the verb interprets the sampled atom at apply-time
-- | (Selene-lenient — a bad token falls back to the verb's default, never a dropped
-- | layer). This is exactly the domain-agnostic string-arg model `Macro.purs` already
-- | used, which is what lets the two scales share one parser, one tokenizer, and one
-- | printer instead of two divergent dialects.
-- |
-- | Two SAMPLING projections read a `PatternArg`, at different depths — the module is
-- | the honest home of both:
-- |
-- |   * `sampleArg` — the MACRO projection: one value per macro-cycle, one-level `<>`
-- |     alternation, multi-word-safe (quoted alternatives survive). This is verbatim
-- |     Macro's old `resolveArg`, so the arrangement lane behaves identically.
-- |   * (the MICRO projection — full mini-notation sampled per chord EVENT — lives in
-- |     `Vetula.App` as `argEval`, since it pulls in the `src/Tidal` engine. Keeping
-- |     that dependency out of here keeps `Reef.Macro` engine-free.)
-- |
-- | Total + lenient throughout (Selene `Source.purs` discipline).
-- |
-- | Moved from Triggerfish (`Triggerfish.PatternArg`) on 2026-10-02 so the rig can read Vetula's
-- | cards (docs/kb/plans/gpl-boundary-review.md, step 3); Triggerfish re-exports it.
module Reef.PatternArg
  ( PatternArg(..)
  , argSrc
  , printArg
  , glyphArg
  , mkArg
  , tokenize
  , unq
  , unquote
  , stripAngle
  , isAngle
  , sampleArg
  ) where

import Prelude

import Data.Array (foldl, index, length, null, snoc)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String.CodeUnits (singleton, stripPrefix, stripSuffix, toCharArray)

-- | A verb's ARGUMENT — one uniform string model. `Lit` is a bare literal (prints
-- | bare); `Pat` is a pattern whose source is either a mini-notation (`0 7 <5 3>`,
-- | prints quoted) or an angle-alternation (`<open drop2>`, prints bare — the angles
-- | are its own delimiters). The verb interprets the sampled atom; invalid → default.
data PatternArg = Lit String | Pat String

derive instance Eq PatternArg

-- | The source text of an arg (quotes already stripped; angles, if any, kept).
argSrc :: PatternArg -> String
argSrc = case _ of
  Lit s -> s
  Pat s -> s

-- | Canonical, round-trippable text: a literal bare; a pattern quoted, UNLESS it is
-- | an angle-alternation (which carries its own `<…>` delimiters and prints bare, so
-- | `voice <open drop2>` round-trips without redundant quotes — §4.4). `mkArg` is the
-- | exact inverse.
printArg :: PatternArg -> String
printArg = case _ of
  Lit s -> s
  Pat s -> if isAngle s then s else "\"" <> s <> "\""

-- | A compact chip glyph: a literal bare, a pattern wrapped in ⟨…⟩ so the eye reads
-- | "this arg is patterned" without the quote noise.
glyphArg :: PatternArg -> String
glyphArg = case _ of
  Lit s -> s
  Pat s -> "⟨" <> s <> "⟩"

-- | A raw token → a `PatternArg`. A `"…"` span is a pattern (quotes stripped); a
-- | `<…>` span is a pattern (angles kept — they are its delimiters); anything else is
-- | a bare literal. Inverse of `printArg`.
mkArg :: String -> PatternArg
mkArg t = case unquote t of
  Just inner -> Pat inner
  Nothing -> if isAngle t then Pat t else Lit t

-- | Split a string into a flat token stream. A `<…>` group and a `"…"` quoted span
-- | each stay intact (their internal whitespace does NOT break a token); a bare `#`
-- | (space-delimited) becomes its own token. Quotes and angles are preserved in the
-- | raw token — the arg/form parsers strip them. (Moved verbatim from `Macro.purs`;
-- | it is the quote/angle-aware tokenizer both scales need.)
tokenize :: String -> Array String
tokenize s = (flush final).out
  where
  final = foldl step { out: [], cur: "", depth: 0, quoted: false } (toCharArray s)
  flush acc = if acc.cur == "" then acc else acc { out = snoc acc.out acc.cur, cur = "" }
  step acc ch =
    if acc.quoted then
      if ch == '"' then acc { cur = acc.cur <> "\"", quoted = false }
      else acc { cur = acc.cur <> singleton ch }
    else if ch == '"' then acc { cur = acc.cur <> "\"", quoted = true }
    else if (ch == ' ' || ch == '\n' || ch == '\t') && acc.depth == 0 then flush acc
    else if ch == '<' then acc { cur = acc.cur <> "<", depth = acc.depth + 1 }
    else if ch == '>' then acc { cur = acc.cur <> ">", depth = if acc.depth > 0 then acc.depth - 1 else 0 }
    else acc { cur = acc.cur <> singleton ch }

-- | The MACRO projection: resolve an arg to ONE value on macro-cycle `cyc`. A literal
-- | is constant; a `<…>` alternation picks its `cyc`-th element (mod length), the
-- | inner tokenizer keeping quoted multi-word alternatives whole (`<"F# lydian" "G
-- | major">`); a non-angle pattern resolves to its source verbatim. Verbatim Macro's
-- | old `resolveArg`, so the arrangement lane is behaviour-identical.
sampleArg :: Int -> PatternArg -> String
sampleArg cyc = case _ of
  Lit s -> s
  Pat src -> case stripAngle src of
    Just inner ->
      let xs = map unq (tokenize inner)
      in if null xs then "" else fromMaybe "" (index xs (cyc `mod` length xs))
    Nothing -> src

-- | Strip surrounding quotes if present, else return as-is.
unq :: String -> String
unq x = fromMaybe x (unquote x)

-- | `"name"` → `Just name` (surrounding double quotes removed), else `Nothing`.
unquote :: String -> Maybe String
unquote t = case stripPrefix (Pattern "\"") t of
  Just rest -> stripSuffix (Pattern "\"") rest
  Nothing -> Nothing

-- | `<inner>` → `Just inner`, otherwise `Nothing`.
stripAngle :: String -> Maybe String
stripAngle t = case stripPrefix (Pattern "<") t of
  Just rest -> stripSuffix (Pattern ">") rest
  Nothing -> Nothing

-- | Is this an angle-alternation span (`<…>`)?
isAngle :: String -> Boolean
isAngle s = case stripAngle s of
  Just _ -> true
  Nothing -> false
