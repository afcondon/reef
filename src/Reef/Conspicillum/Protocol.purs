-- | `Reef.Conspicillum.Protocol` — the wire format for the Conspicillum scene
-- | handoff. The frontend resolves a corpus, a query and a cloud, encodes them
-- | to JSON and pushes them to the rig ONCE; the BEAM decodes and runs the
-- | shared `Reef.Conspicillum.Corpus` selector from exactly that definition.
-- | Because a cloud is a pure function of the scene and the seed, that one push
-- | is the whole cross-runtime wire — no per-grain stream, which at the
-- | densities C1 measured would be the wrong shape entirely.
-- |
-- | `Axis`, `Cmp` and `Toward` are closed ADTs, so — as with Vetula's
-- | `VDest`/`VRenderer` and Balistes' `BInput` — they are projected to small
-- | ints and an unknown int decodes to a defined default rather than dropping
-- | the clause. `Corpus` itself needs no projection: it is already records of
-- | Int/Number/String/Array, which simple-json's generic `writeJSON`/`readJSON`
-- | cover directly on both runtimes (the BEAM defers to `jsx`).
-- |
-- | **The axis numbering is append-only.** Renumbering it would not break a
-- | decode; it would silently change which measurement a saved query filters
-- | on, and the failure would be "this cloud is made of the wrong material"
-- | rather than an error. Add at the end, never reorder.
module Reef.Conspicillum.Protocol
  ( WireAxis
  , WireClause
  , WireWeighting
  , WireQuery
  , WireRule
  , WireSpec
  , WireScene
  , Scene
  , axisToWire
  , axisFromWire
  , cmpToInt
  , cmpFromInt
  , towardToInt
  , towardFromInt
  , toWireRule
  , fromWireRule
  , toWireScene
  , fromWireScene
  , encodeScene
  , decodeScene
  ) where

import Prelude

import Data.Array (head, length)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Foreign (MultipleErrors)
import Reef.Conspicillum.Corpus
  (Axis(..), Cloud, Cmp(..), Corpus, Query, Toward(..), Weighting)
import Reef.Conspicillum.Cloud (Chain, Fx, Op(..), Rule, Spec, When(..))
import Reef.Conspicillum.Harmonic (Harmonic)
import Simple.JSON (readJSON, writeJSON)

-- | Everything the rig needs to run a cloud, pushed once.
type Scene =
  { corpus :: Corpus
  , query :: Query
  , spec :: Spec
  -- | The base seed, as an Int rather than a `Seed`.
  -- |
  -- | `Cloud.cycleOf` derives each cycle's `Seed` from this and the cycle
  -- | number — that derivation is what makes any cycle directly addressable —
  -- | so the Int IS the scene's seed and a `Seed` would be a already-consumed
  -- | form of it. A scene has ONE seed; anything reaching for `pick` directly
  -- | calls `seedFrom` on this.
  , seed :: Int
  }

-- ── axis ─────────────────────────────────────────────────────────────────────

-- | An axis, wire-flat. `at` is meaningful only for `cell`, `param` only for
-- | `param`; both are always present so the record shape never varies, which
-- | keeps the JSON uniform for jsx.
type WireAxis = { kind :: Int, at :: Int, param :: String }

axisToWire :: Axis -> WireAxis
axisToWire = case _ of
  APeak -> { kind: 0, at: 0, param: "" }
  ARms -> { kind: 1, at: 0, param: "" }
  AZcr -> { kind: 2, at: 0, param: "" }
  ATilt -> { kind: 3, at: 0, param: "" }
  ADecay -> { kind: 4, at: 0, param: "" }
  ASecs -> { kind: 5, at: 0, param: "" }
  ACell k -> { kind: 6, at: k, param: "" }
  AParam nm -> { kind: 7, at: 0, param: nm }

-- | Total, per the house convention. An unrecognised kind reads as `ARms`,
-- | which is the most neutral loudness-ish axis and is always answerable — see
-- | the append-only note in the module header for why this is a weaker
-- | protection than it looks.
axisFromWire :: WireAxis -> Axis
axisFromWire w = case w.kind of
  0 -> APeak
  1 -> ARms
  2 -> AZcr
  3 -> ATilt
  4 -> ADecay
  5 -> ASecs
  6 -> ACell w.at
  7 -> AParam w.param
  _ -> ARms

-- ── comparison and direction ─────────────────────────────────────────────────

cmpToInt :: Cmp -> Int
cmpToInt = case _ of
  Lt -> 0
  Lte -> 1
  Gt -> 2
  Gte -> 3

cmpFromInt :: Int -> Cmp
cmpFromInt = case _ of
  0 -> Lt
  1 -> Lte
  2 -> Gt
  _ -> Gte

towardToInt :: Toward -> Int
towardToInt = case _ of
  High -> 0
  Low -> 1

towardFromInt :: Int -> Toward
towardFromInt = case _ of
  1 -> Low
  _ -> High

-- ── clauses, weighting, query ────────────────────────────────────────────────

type WireClause = { axis :: WireAxis, cmp :: Int, value :: Number }
type WireWeighting = { axis :: WireAxis, toward :: Int, strength :: Number }

-- | `weighting` is an array of AT MOST ONE rather than a nullable field.
-- | simple-json maps `Maybe` to null, and null is exactly the sort of thing
-- | that round-trips through V8 and through jsx in ways that agree until one
-- | day they do not. An empty-or-singleton array is unambiguous on both.
-- | `Harmonic` needs no projection at all: it is `{ target :: { pcs, root,
-- | bass }, minFit, strength }`, which is Int / Array Int / Number the whole
-- | way down. It rides as an at-most-one array for the same reason `weighting`
-- | does.
type WireQuery =
  { clauses :: Array WireClause
  , weighting :: Array WireWeighting
  , harmonic :: Array Harmonic
  }

-- | A rule, wire-flat. `when` and `op` are the two closed ADTs as small ints;
-- | their payloads ride in fixed fields so the record shape never varies,
-- | which keeps the JSON uniform for jsx.
-- |
-- | Append-only, like the axis numbering, and for the same reason: renumbering
-- | would not fail a decode, it would silently turn "reverse every third
-- | grain" into something else.
type WireRule =
  { when :: Int      -- 0 always, 1 every, 2 chance
  , everyN :: Int
  , everyK :: Int
  , chance :: Number
  , op :: Int        -- see `opToWire`; 0-4 are the voice, 5 up are the effects
  , amount :: Number
  }

-- | `Fx` and `Chain` need no projection: both are flat records of Number (plus
-- | one Int), which simple-json's generic codec covers directly on both
-- | runtimes. They ride at full width — every field always present — for the
-- | reason the module header gives about `weighting`: a nullable or optional
-- | field is exactly the sort of thing that round-trips through V8 and through
-- | jsx in ways that agree until one day they do not.
type WireSpec =
  { onsets :: Array Number
  , cloud :: Cloud
  , rules :: Array WireRule
  , speed :: Number
  , gain :: Number
  , pan :: Number
  , accelerate :: Number
  , fx :: Fx
  , chain :: Chain
  }

type WireScene =
  { corpus :: Corpus
  , query :: WireQuery
  , spec :: WireSpec
  , seed :: Int
  }

whenToWire :: When -> { when :: Int, everyN :: Int, everyK :: Int, chance :: Number }
whenToWire = case _ of
  Always -> { when: 0, everyN: 0, everyK: 0, chance: 0.0 }
  Every n k -> { when: 1, everyN: n, everyK: k, chance: 0.0 }
  Chance p -> { when: 2, everyN: 0, everyK: 0, chance: p }

-- | Total. An unrecognised kind reads as `Always`, which applies the rule to
-- | every grain — deliberately the LOUD failure rather than the quiet one. A
-- | version skew that silently stopped applying a rule would be heard as
-- | "this preset sounds flat" and blamed on the material.
whenFromWire :: WireRule -> When
whenFromWire w = case w.when of
  1 -> Every w.everyN w.everyK
  2 -> Chance w.chance
  _ -> Always

-- | **Append-only from 5.** 0-4 were frozen the day the first scene went over
-- | the wire; the effects take 5 up and nothing already on it changes meaning.
opToWire :: Op -> { op :: Int, amount :: Number }
opToWire = case _ of
  OpSpeed x -> { op: 0, amount: x }
  OpGain x -> { op: 1, amount: x }
  OpLength x -> { op: 2, amount: x }
  OpPan x -> { op: 3, amount: x }
  OpAccelerate x -> { op: 4, amount: x }
  OpShape x -> { op: 5, amount: x }
  OpCrush x -> { op: 6, amount: x }
  OpCoarse x -> { op: 7, amount: x }
  OpLpf x -> { op: 8, amount: x }
  OpHpf x -> { op: 9, amount: x }
  OpBpf x -> { op: 10, amount: x }
  OpRes x -> { op: 11, amount: x }
  OpVowel x -> { op: 12, amount: x }
  OpPshift x -> { op: 13, amount: x }
  OpTremolo x -> { op: 14, amount: x }
  OpPhaser x -> { op: 15, amount: x }
  OpGenv x -> { op: 16, amount: x }
  OpGtilt x -> { op: 17, amount: x }
  OpGplat x -> { op: 18, amount: x }
  OpAtk x -> { op: 19, amount: x }
  OpHold x -> { op: 20, amount: x }
  OpRel x -> { op: 21, amount: x }
  OpCurve x -> { op: 22, amount: x }
  OpRsnPitch x -> { op: 23, amount: x }
  OpRsnDecay x -> { op: 24, amount: x }
  OpRsnBright x -> { op: 25, amount: x }
  OpRsnMix x -> { op: 26, amount: x }
  OpRsnModel x -> { op: 27, amount: x }

-- | Total, and the fallthrough is now `OpAccelerate` only because it always
-- | was. An unknown op reads as the LAST of the original five rather than the
-- | last of all of them, so a page built against a newer numbering than the rig
-- | misbehaves in a way that existed before rather than in a new way.
opFromWire :: WireRule -> Op
opFromWire w = case w.op of
  0 -> OpSpeed w.amount
  1 -> OpGain w.amount
  2 -> OpLength w.amount
  3 -> OpPan w.amount
  5 -> OpShape w.amount
  6 -> OpCrush w.amount
  7 -> OpCoarse w.amount
  8 -> OpLpf w.amount
  9 -> OpHpf w.amount
  10 -> OpBpf w.amount
  11 -> OpRes w.amount
  12 -> OpVowel w.amount
  13 -> OpPshift w.amount
  14 -> OpTremolo w.amount
  15 -> OpPhaser w.amount
  16 -> OpGenv w.amount
  17 -> OpGtilt w.amount
  18 -> OpGplat w.amount
  19 -> OpAtk w.amount
  20 -> OpHold w.amount
  21 -> OpRel w.amount
  22 -> OpCurve w.amount
  23 -> OpRsnPitch w.amount
  24 -> OpRsnDecay w.amount
  25 -> OpRsnBright w.amount
  26 -> OpRsnMix w.amount
  27 -> OpRsnModel w.amount
  _ -> OpAccelerate w.amount

toWireRule :: Rule -> WireRule
toWireRule r =
  let wh = whenToWire r.when
      o = opToWire r.op
  in { when: wh.when, everyN: wh.everyN, everyK: wh.everyK, chance: wh.chance
     , op: o.op, amount: o.amount }

fromWireRule :: WireRule -> Rule
fromWireRule w = { when: whenFromWire w, op: opFromWire w }

toWireSpec :: Spec -> WireSpec
toWireSpec sp = sp { rules = map toWireRule sp.rules }

fromWireSpec :: WireSpec -> Spec
fromWireSpec sp = sp { rules = map fromWireRule sp.rules }

toWireQuery :: Query -> WireQuery
toWireQuery q =
  { clauses: map (\c -> { axis: axisToWire c.axis, cmp: cmpToInt c.cmp, value: c.value }) q.clauses
  , weighting: case q.weighting of
      Nothing -> []
      Just w -> [ { axis: axisToWire w.axis, toward: towardToInt w.toward, strength: w.strength } ]
  , harmonic: case q.harmonic of
      Nothing -> []
      Just h -> [ h ]
  }

fromWireQuery :: WireQuery -> Query
fromWireQuery w =
  { clauses: map (\c -> { axis: axisFromWire c.axis, cmp: cmpFromInt c.cmp, value: c.value }) w.clauses
  , weighting: if length w.weighting == 0 then Nothing else map toW (head w.weighting)
  , harmonic: head w.harmonic
  }
  where
  toW :: WireWeighting -> Weighting
  toW x = { axis: axisFromWire x.axis, toward: towardFromInt x.toward, strength: x.strength }

toWireScene :: Scene -> WireScene
toWireScene s = { corpus: s.corpus, query: toWireQuery s.query, spec: toWireSpec s.spec, seed: s.seed }

fromWireScene :: WireScene -> Scene
fromWireScene s = { corpus: s.corpus, query: fromWireQuery s.query, spec: fromWireSpec s.spec, seed: s.seed }

encodeScene :: Scene -> String
encodeScene = writeJSON <<< toWireScene

decodeScene :: String -> Either MultipleErrors Scene
decodeScene = map fromWireScene <<< readJSON
