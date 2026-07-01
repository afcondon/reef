-- | `Reef.Conformance` — the pure cross-runtime oracle for the Odonus engine.
-- |
-- | Three I/O-free renders, each run under BOTH the JS backend and purerl
-- | (Erlang); diffing the two proves the engine behaves identically on both
-- | runtimes, which is what makes wiring `reef_voice` onto `reef_*@ps` — and the
-- | lockstep co-simulation (reef/docs/PLAN-lockstep-cosimulation.md) — safe.
-- | Frozen as goldens in the test suite so any future drift is caught.
-- |
-- |   • `run`       — the original 32-step engine golden (scale-quantised, no gen).
-- |                   Proves `stepEmit` / `renderCell` identical.
-- |   • `genRun`    — a LONG generative run: the 10 non-transcendental gen sources
-- |                   enabled, threading `runGen` then `stepEmit` for 2000 steps
-- |                   from a fixed seed, with the full evolving state sampled as a
-- |                   digest. This is the lockstep determinism safety net — it
-- |                   proves the *generative* simulation (the last piece moved into
-- |                   reef) stays bit-identical over thousands of steps, and would
-- |                   surface any float / iteration-order nondeterminism.
-- |   • `betaProbe` — a DIAGNOSTIC isolating the one transcendental in the whole
-- |                   step path: `pow`, inside the Marbles Beta weights, reached
-- |                   only via the GNotes source. IEEE 754 does not mandate a
-- |                   correctly-rounded `pow`, so V8's `Math.pow` and BEAM's
-- |                   `math:pow` *may* disagree in the last ULP and flip a
-- |                   `pickIndex` outcome. We render the Int draws (format-clean)
-- |                   so the cross-runtime script can report whether GNotes is also
-- |                   safe — empirically, not by assumption. (Non-gating: it tells
-- |                   us what P2 must harden if it diverges.)
module Reef.Conformance
  ( run, steps
  , genRun, genSteps, sampleEvery
  , betaProbe
  , inputRun, inputSteps
  ) where

import Prelude

import Data.Array (filter, range, snoc, mapWithIndex)
import Data.Either (Either(..))
import Data.Foldable (foldl, intercalate)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Reef.Odonus (Cell, Fired, Head, Odonus, defaultOdonus, stepEmit)
import Reef.Gen (GenKind(..), GenSource, genKinds, genDefaultRate, genDefaultAmt)
import Reef.Engine (stepTick)
import Reef.Input (Input(..), SimState, Tagged, applyInput)
import Reef.PitchSet (PitchSet(..))
import Reef.Protocol (decodeInput, encodeInput)
import Reef.Marbles (Seed, seedFrom, rollValue)

-- ── 1. the original engine golden ────────────────────────────────────────────

-- | Number of steps to render. Two bars of 16 cells.
steps :: Int
steps = 32

-- | The golden render: one line per step, listing the events that fired.
run :: String
run =
  let
    final = foldl advance { odo: defaultOdonus, out: [] } (range 1 steps)
  in
    intercalate "\n" final.out
  where
  advance :: { odo :: Odonus, out :: Array String } -> Int -> { odo :: Odonus, out :: Array String }
  advance acc i =
    let r = stepEmit acc.odo
    in { odo: r.odo, out: snoc acc.out (renderStep i r.fired) }

renderStep :: Int -> Array Fired -> String
renderStep i fired =
  pad3 i <> " | " <> (if fired == [] then "-" else intercalate "  " (map renderFired fired))

renderFired :: Fired -> String
renderFired f =
  "h" <> show f.headIdx
    <> " p" <> show f.pitch
    <> " d" <> show f.dur
    <> " r" <> show f.ratchet
    <> " v" <> show f.vel
    <> (if f.glide then " ~" else "")

-- ── 2. the long generative determinism net ───────────────────────────────────

-- | How many model steps the generative run threads. "Thousands" per the plan.
genSteps :: Int
genSteps = 2000

-- | Sample the full state to the golden every this-many steps (keeps the fixture
-- | compact — ~80 lines — while still localising any divergence to a 25-step window).
sampleEvery :: Int
sampleEvery = 25

-- | The 10 non-transcendental gen sources, enabled at their defaults. GNotes is
-- | the lone exclusion: its Beta path uses `pow` (see `betaProbe`). Everything
-- | here is exact integer / dyadic-float arithmetic, hence provably identical
-- | across runtimes — which is exactly what this golden asserts.
activeGen :: Array GenSource
activeGen = map mk genKinds
  where
  mk k = { kind: k, on: k /= GNotes, rate: genDefaultRate k, amt: genDefaultAmt k }

-- | Thread `runGen` (mutate) then `stepEmit` (advance) for `genSteps`, from a
-- | fixed seed, sampling the evolving state. Because the sampled digest captures
-- | the whole Odonus + the PRNG seed in small format-clean ints, ANY cross-runtime
-- | divergence — a different mutation, a different pitch, a drifted seed — shows
-- | up at the next sample line.
genRun :: String
genRun =
  let
    final = foldl advance { odo: defaultOdonus, seed: seedFrom 1, out: [] } (range 1 genSteps)
  in
    intercalate "\n" final.out
  where
  advance acc i =
    let
      r = stepTick { gen: activeGen, spread: 0.5, bias: 0.5, odo: acc.odo, seed: acc.seed }
      out' = if i `mod` sampleEvery == 0 then snoc acc.out (digest i r.sim.odo r.sim.seed) else acc.out
    in
      { odo: r.sim.odo, seed: r.sim.seed, out: out' }

-- | A full-state digest line covering EVERY field any active gen source can
-- | mutate, so a divergence in any of them is caught at the next sample:
-- |   • key (GKey) + the scale intervals (GKey · setRandScale) + PRNG seed
-- |     (rounded to an Int — it is an integer-valued Number, kept off the wire as
-- |     a float to dodge cross-runtime show formatting);
-- |   • each cell as note/dur/ratchet/flags, flags = gate|skip<<1|glide<<2
-- |     (GLen·dur, GRatchet·ratchet, GGate/GSkip/GGlide·flags);
-- |   • each head as cursor:seqPos:transp:speedIx:patternIx:len(+m if muted)
-- |     (GPattern, GTransp, GSpeed, GHeads).
-- | All small ints / fixed glyphs — format-clean, no 32-bit-Int overflow risk.
digest :: Int -> Odonus -> Seed -> String
digest i o seed =
  pad4 i <> " | k" <> show o.rootPc <> " [" <> intercalate "," (map show o.scaleIvls) <> "]"
    <> " s" <> show (round seed)
    <> " | " <> intercalate "," (map cellDig o.cells)
    <> " | " <> intercalate "  " (mapWithIndex headDig o.heads)

cellDig :: Cell -> String
cellDig c =
  show c.note <> "/" <> show c.dur <> "/" <> show c.ratchet <> "/"
    <> show ((if c.gate then 1 else 0) + (if c.skip then 2 else 0) + (if c.glide then 4 else 0))

headDig :: Int -> Head -> String
headDig _ h =
  show h.cursor <> ":" <> show h.seqPos <> ":" <> show h.transp
    <> ":" <> show h.speedIx <> ":" <> show h.patternIx <> ":" <> show h.len
    <> (if h.mute then "m" else "")

-- ── 2b. the input-protocol determinism net (the P3 proof) ─────────────────────

-- | How many model steps the input run threads.
inputSteps :: Int
inputSteps = 400

-- | A scripted lockstep session: a tick-tagged stream of user actions covering
-- | every `Input` family — cell/head setters, the quantizer, the chord overlay,
-- | the Reichian macros, gen-source config + the Marbles pad, and the three
-- | seed-threading rolls (RollAllNotes / RollChords / SeedMelody). Several land on
-- | the same tick (a batch). Enabling GNotes at tick 30 also folds the one
-- | transcendental (`pow`, via the Beta) into the long run. These are the inputs
-- | a frontend would broadcast; here both runtimes replay them in lockstep.
inputScript :: Array Tagged
inputScript =
  [ { tick: 5, input: SetRoot 2 }
  , { tick: 5, input: SetNote 0 7 }
  , { tick: 12, input: ToggleHeadMute 1 }
  , { tick: 12, input: SetHeadTransp 1 5 }
  , { tick: 20, input: SetHeadSpeedIx 2 6 }
  , { tick: 20, input: ToggleHeadMute 2 }
  , { tick: 30, input: ToggleGen GNotes }
  , { tick: 30, input: SetGenBias 700 }
  , { tick: 30, input: SetGenSpread 300 }
  , { tick: 45, input: RollAllNotes }
  , { tick: 60, input: SetPitchSet (PitchSet { offsets: [ 0, 2, 4, 7, 9 ], root: 50, period: Just 12 }) }
  , { tick: 75, input: CyclePattern 0 }
  , { tick: 90, input: SetChordFeed [ [ 0, 4, 7 ], [ 2, 5, 9 ] ] }
  , { tick: 90, input: ToggleChord }
  , { tick: 110, input: RollChords }
  , { tick: 130, input: FollowChord (Just [ 0, 3, 7 ]) }
  , { tick: 150, input: ClearPitchSet }
  , { tick: 150, input: SeedMelody }
  , { tick: 175, input: FanOffsets 2 }
  , { tick: 200, input: SetRate GHeads 120 }
  , { tick: 200, input: SetAmt GTransp 60 }
  , { tick: 230, input: NudgeOffsets 1 }
  , { tick: 260, input: SetHeadMask 5 }
  , { tick: 300, input: UnifyHeads }
  , { tick: 340, input: SetSpread 5 }
  , { tick: 380, input: ToggleDistribution }
  ]

-- | Apply one tick-tagged input THROUGH THE CODEC: encode to JSON then decode back
-- | before applying. This puts `encodeInput`/`decodeInput` (both reef functions,
-- | compiled to JS and Erlang) on the critical path of the golden — so the
-- | cross-runtime diff proves the codec is byte-faithful on both runtimes, and the
-- | frozen golden catches any `toWire`/`fromWire` asymmetry. A decode failure would
-- | drop the input and diverge the digest, so a broken codec cannot pass silently.
applyEncoded :: SimState -> Tagged -> SimState
applyEncoded st t = case decodeInput (encodeInput t.input) of
  Right i -> applyInput i st
  Left _ -> st

-- | The P3 net. At each tick: (1) apply any scheduled inputs (round-tripped through
-- | the codec), (2) run the autonomous gen sources, (3) `stepEmit`. ONE seed threads
-- | the per-tick `runGen` AND the roll inputs, so the lockstep seed-sync is exercised
-- | end to end. The full SimState (Odonus + gen config + pad + seed) is digested
-- | every `sampleEvery` steps — any divergence in input application, codec, gen, or
-- | engine surfaces at the next sample. Must be byte-identical node ↔ BEAM.
inputRun :: String
inputRun =
  let
    s0 = { odo: defaultOdonus, gen: activeGen, spread: 0.5, bias: 0.5, seed: seedFrom 1 }
    final = foldl advance { st: s0, out: [] } (range 1 inputSteps)
  in
    intercalate "\n" final.out
  where
  advance acc i =
    let
      due = filter (\t -> t.tick == i) inputScript
      s1 = foldl applyEncoded acc.st due
      r = stepTick s1
      s2 = r.sim
      out' = if i `mod` sampleEvery == 0 then snoc acc.out (inputDigest i s2) else acc.out
    in
      { st: s2, out: out' }

-- | A SimState digest: the Odonus fields (as in `digest`) plus the gen-source
-- | config and the Marbles pad, so the gen-config inputs (ToggleGen / SetRate /
-- | SetAmt) and the pad inputs (SetGenSpread / SetGenBias) are directly observable,
-- | not just via their downstream effect on the notes. Pad as per-mille Ints to
-- | stay format-clean across runtimes.
inputDigest :: Int -> SimState -> String
inputDigest i s =
  pad4 i <> " | k" <> show s.odo.rootPc <> " [" <> intercalate "," (map show s.odo.scaleIvls) <> "]"
    <> " s" <> show (round s.seed)
    <> " sp" <> show (round (s.spread * 1000.0)) <> " bi" <> show (round (s.bias * 1000.0))
    -- chord clock: on-flag, position, and phase — makes the tickChord step in the
    -- shared stepTick composite observable (it advances only when the overlay is on).
    <> " ch" <> (if s.odo.chord.on then "1" else "0") <> show s.odo.chord.ix <> ":" <> show s.odo.chord.phase
    <> " | " <> intercalate "," (map cellDig s.odo.cells)
    <> " | " <> intercalate "  " (mapWithIndex headDig s.odo.heads)
    <> " | " <> intercalate "," (map genDig s.gen)
  where
  genDig :: GenSource -> String
  genDig src = (if src.on then "1" else "0") <> ":" <> show src.rate <> ":" <> show src.amt

-- ── 3. the transcendental (pow / Beta) diagnostic ────────────────────────────

-- | Draw `n` values from the Marbles Beta distribution at one (bias,spread)
-- | setting over a fixed candidate range, threading the seed — the pure heart of
-- | the GNotes source. The results are Ints (chosen candidates), so they format
-- | identically on both runtimes; only a `pow` disagreement large enough to flip
-- | a `pickIndex` bucket would make the sequences differ.
betaProbe :: String
betaProbe = intercalate "\n" (mapWithIndex probeLine settings)
  where
  settings =
    [ { bias: 0.2, spread: 0.3 }
    , { bias: 0.5, spread: 0.5 }
    , { bias: 0.8, spread: 0.7 }
    , { bias: 0.5, spread: 0.95 }
    ]
  cands = range 0 24
  probeLine ix cfg =
    "probe" <> show ix <> " | " <> intercalate "," (map show (draws 60 cfg (seedFrom 7)))
  draws :: Int -> { bias :: Number, spread :: Number } -> Seed -> Array Int
  draws n cfg seed0 = (go n seed0 []).vals
    where
    go k seed acc
      | k <= 0 = { vals: acc }
      | otherwise =
          let r = rollValue cfg cands seed
          in go (k - 1) r.seed (snoc acc r.value)

-- ── shared ───────────────────────────────────────────────────────────────────

pad3 :: Int -> String
pad3 n =
  let s = show n
  in if n < 10 then "  " <> s else if n < 100 then " " <> s else s

pad4 :: Int -> String
pad4 n =
  let s = show n
  in if n < 10 then "   " <> s else if n < 100 then "  " <> s else if n < 1000 then " " <> s else s
