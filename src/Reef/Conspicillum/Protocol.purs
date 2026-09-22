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
  , WireScene
  , Scene
  , axisToWire
  , axisFromWire
  , cmpToInt
  , cmpFromInt
  , towardToInt
  , towardFromInt
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
import Reef.Conspicillum.Harmonic (Harmonic)
import Simple.JSON (readJSON, writeJSON)

-- | Everything the rig needs to run a cloud, pushed once.
type Scene =
  { corpus :: Corpus
  , query :: Query
  , cloud :: Cloud
  , seed :: Number
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

type WireScene =
  { corpus :: Corpus
  , query :: WireQuery
  , cloud :: Cloud
  , seed :: Number
  }

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
toWireScene s = { corpus: s.corpus, query: toWireQuery s.query, cloud: s.cloud, seed: s.seed }

fromWireScene :: WireScene -> Scene
fromWireScene s = { corpus: s.corpus, query: fromWireQuery s.query, cloud: s.cloud, seed: s.seed }

encodeScene :: Scene -> String
encodeScene = writeJSON <<< toWireScene

decodeScene :: String -> Either MultipleErrors Scene
decodeScene = map fromWireScene <<< readJSON
