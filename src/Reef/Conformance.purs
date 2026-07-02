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
  , simRun, simSteps
  , balistesRun, balistesSteps
  , balistesSimRun, balistesSimSteps
  , balistesInputRun, balistesInputSteps
  ) where

import Prelude

import Data.Array (filter, null, range, snoc, mapWithIndex)
import Data.Either (Either(..))
import Data.Foldable (foldl, intercalate)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Reef.Balistes.Engine (Trigger, evaluateStep, freshPerturbations) as Bal
import Reef.Balistes.Sim (BalSim, defaultBalSim, stepBal, renderStep) as BSim
import Reef.Balistes.Protocol (decodeBalSim, encodeBalSim, decodeBTagged, encodeBTagged) as BSim
import Reef.Balistes.Input (BInput(..), BTagged, applyBInput) as RBI
import Reef.Odonus (Cell, Fired, Head, Odonus, defaultOdonus, stepEmit)
import Reef.Gen (GenKind(..), GenSource, genKinds, genDefaultRate, genDefaultAmt)
import Reef.Engine (stepTick)
import Reef.Input (Input(..), SimState, Tagged, applyInput)
import Reef.PitchSet (PitchSet(..))
import Reef.Protocol (decodeInput, decodeSim, encodeInput)
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

-- ── 2c. the SimState handoff net (the P4d proof) ──────────────────────────────

-- | How many steps the handoff run threads.
simSteps :: Int
simSteps = 400

-- | A real handoff payload: the JSON `Reef.Protocol.encodeSim` produced in the JS
-- | frontend for a SimState with 10 gen sources ON (GNotes off), a definite seed
-- | (7) and pad. `simRun` DECODES this on whichever runtime it runs on, then steps
-- | `stepTick` from the reconstructed state. Byte-identical node ↔ BEAM proves the
-- | BEAM's `decodeSim` rebuilds exactly the state the browser encoded AND evolves it
-- | identically — i.e. the lockstep handoff lands the rig on the frontend's state.
handoffJson :: String
handoffJson = """{"spreadMille":500,"seedInt":7,"odo":{"span":3,"scaleIvls":[0,2,3,5,7,8,10],"rootPc":0,"octaveShift":0,"heads":[{"transp":0,"speedIx":4,"seqPos":0,"pulses":16,"pendStep":1,"patternIx":0,"offset":0,"mute":false,"len":16,"esteps":16,"direction":0,"cursor":0,"accumulator":0},{"transp":7,"speedIx":2,"seqPos":0,"pulses":16,"pendStep":1,"patternIx":1,"offset":0,"mute":true,"len":16,"esteps":16,"direction":0,"cursor":0,"accumulator":0},{"transp":-12,"speedIx":6,"seqPos":0,"pulses":16,"pendStep":1,"patternIx":3,"offset":0,"mute":true,"len":16,"esteps":16,"direction":1,"cursor":0,"accumulator":0},{"transp":3,"speedIx":3,"seqPos":0,"pulses":16,"pendStep":1,"patternIx":2,"offset":0,"mute":true,"len":16,"esteps":16,"direction":2,"cursor":0,"accumulator":0}],"gatePct":90,"dist":"natural","degShift":0,"chord":{"picks":[15,10,12,13],"phase":0,"period":16,"on":false,"ix":0,"feed":[]},"cells":[{"vel":100,"skip":false,"ratchet":1,"note":0,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":1,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":2,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":3,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":4,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":5,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":6,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":7,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":8,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":9,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":10,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":11,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":12,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":13,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":14,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":15,"glide":false,"gate":true,"dur":1}]},"gen":[{"rate":96,"on":false,"kind":0,"amt":20},{"rate":96,"on":true,"kind":1,"amt":30},{"rate":96,"on":true,"kind":2,"amt":30},{"rate":96,"on":true,"kind":3,"amt":30},{"rate":72,"on":true,"kind":4,"amt":25},{"rate":72,"on":true,"kind":5,"amt":25},{"rate":96,"on":true,"kind":6,"amt":30},{"rate":96,"on":true,"kind":7,"amt":40},{"rate":96,"on":true,"kind":8,"amt":30},{"rate":96,"on":true,"kind":9,"amt":30},{"rate":96,"on":true,"kind":10,"amt":25}],"biasMille":500}"""

-- | Decode the handoff, then step it. A decode failure surfaces as a single
-- | screaming line (so the golden/cross-runtime catches it) rather than silently
-- | passing. Digest is the same `digest` genRun uses.
simRun :: String
simRun = case decodeSim handoffJson of
  Left errs -> "SIM-DECODE-FAIL: " <> show errs
  Right sim0 -> intercalate "\n" (foldl advance { sim: sim0, out: [] } (range 1 simSteps)).out
  where
  advance acc i =
    let r = stepTick acc.sim
        out' = if i `mod` sampleEvery == 0 then snoc acc.out (digest i r.sim.odo r.sim.seed) else acc.out
    in { sim: r.sim, out: out' }

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

-- ── 4. the Balistes engine determinism net (Phase 0 of the Balistes lockstep) ─

-- | How many 16th-note steps the Balistes run threads.
balistesSteps :: Int
balistesSteps = 256

-- | The Balistes cross-runtime net. `Reef.Balistes.Engine` is the newly-shared
-- | Grids core, replacing the two hand-ports (`Triggerfish.Balistes.Engine` +
-- | `balistes_engine.erl`) that could silently disagree on the perturbation RNG.
-- | This run drives all three of the engine's cross-runtime hazards at once:
-- |   • `readDrumMap` bilinear interpolation — X sweeps 0..255 and Y at a coprime
-- |     rate, so every one of the 5×5 drum-map nodes and its fractional weights is
-- |     hit (exercises `Reef.Balistes.Tables` + `u8Mix`);
-- |   • `freshPerturbations` — resampled at each 32-step pattern start, threading a
-- |     single RNG seed through `Reef.Bits.xorshift32` (the FFI 32-bit primitive
-- |     that is the whole reason this consolidation was needed);
-- |   • `evaluateStep` — the density threshold + accent rule.
-- | Every emitted value is a small Int (perturbation bytes 0..255, trigger indices,
-- | accent flags), so it formats identically on both runtimes; any divergence in
-- | interpolation, RNG, or the trigger rule surfaces at the offending step. Must be
-- | byte-identical node ↔ BEAM — that identity is what will let `balistes_voice`
-- | run onto `reef_balistes_engine@ps` and co-simulate the frontend.
balistesRun :: String
balistesRun =
  let
    final = foldl advance { rng: 1, perts: [ 0, 0, 0 ], out: [] } (range 0 (balistesSteps - 1))
  in
    intercalate "\n" final.out
  where
  advance acc i =
    let
      x = (i * 5) `mod` 256
      y = (i * 3) `mod` 256
      step = i `mod` 32
      randomness = (i * 7) `mod` 256
      densities = [ 128, 96 + (i `mod` 64), 160 ]
      resample = step == 0
      fp = Bal.freshPerturbations randomness acc.rng
      perts = if resample then fp.perts else acc.perts
      rng' = if resample then fp.rng else acc.rng
      fired = Bal.evaluateStep step x y densities perts
    in
      { rng: rng', perts, out: snoc acc.out (balDig i x y perts fired) }

balDig :: Int -> Int -> Int -> Array Int -> Array Bal.Trigger -> String
balDig i x y perts fired =
  pad4 i <> " x" <> pad3 x <> " y" <> pad3 y
    <> " p" <> intercalate "," (map show perts)
    <> " | " <> (if null fired then "-" else intercalate " " (map glyph fired))
  where
  glyph t = show t.inst <> (if t.accent then "!" else "")

-- ── 5. the Balistes handoff + shared-render net (P1 of the Balistes lockstep) ─

-- | How many steps the Balistes handoff run threads.
balistesSimSteps :: Int
balistesSimSteps = 128

-- | A non-default handoff state that exercises every render decision: high
-- | densities (lots of hits), high randomness (perturbations active), a raised
-- | OPEN dial (HH hats convert to open), a Dilla push (per-lane ms offsets) and
-- | custom notes. This is the state a frontend would push on Push-to-rig.
balSimSeed :: BSim.BalSim
balSimSeed = BSim.defaultBalSim
  { x = 200, y = 60, densBd = 210, densSd = 180, densHh = 200
  , randomness = 180, open = 120, push = [ 0, 12, -8, -8 ], notes = [ 36, 40, 42, 46 ] }

-- | The P1 net for Balistes. `balSimSeed` is round-tripped THROUGH the codec
-- | (encodeBalSim then decodeBalSim — both reef functions, so both compile to JS and
-- | to the BEAM's `jsx`), then over 128 steps we thread `stepBal` (the shared engine
-- | tick, incl. the perturbation RNG via Reef.Bits.xorshift32) and digest what
-- | `renderStep` (the shared open-hat / note / Dilla-push / ratchet / accent decision)
-- | emits at each played step. Byte-identical node ↔ BEAM proves the WHOLE shared
-- | Balistes path — codec + engine + render — behaves identically on both runtimes,
-- | which is exactly what lets `reef_balistes_voice` co-simulate the frontend from a
-- | pushed handoff. A decode failure screams rather than passing silently.
balistesSimRun :: String
balistesSimRun = case BSim.decodeBalSim (BSim.encodeBalSim balSimSeed) of
  Left errs -> "BAL-SIM-DECODE-FAIL: " <> show errs
  Right sim0 ->
    intercalate "\n" (foldl advance { bal: sim0, out: [] } (range 0 (balistesSimSteps - 1))).out
  where
  advance acc i =
    let
      playedStep = acc.bal.step
      r = BSim.stepBal acc.bal
      evs = BSim.renderStep acc.bal playedStep r.fired
    in
      { bal: r.bal, out: snoc acc.out (evDig i evs) }

evDig :: Int -> Array { note :: Int, velocity :: Int, pushMs :: Int, durMs :: Number, ratchet :: Int } -> String
evDig i evs =
  pad4 i <> " | " <> (if null evs then "-" else intercalate "  " (map one evs))
  where
  one e = show e.note <> "/" <> show e.velocity <> "/" <> show e.pushMs
    <> "/" <> show (round e.durMs) <> "x" <> show e.ratchet

-- ── 6. the Balistes input-protocol net (live knob/gesture lockstep) ───────────

-- | How many steps the Balistes input run threads.
balistesInputSteps :: Int
balistesInputSteps = 200

-- | A scripted tick-tagged Balistes session covering every `BInput` family — the
-- | X/Y pad, all four knob kinds (density/randomness/open/push), a per-lane note, a
-- | ratchet edit, and the two RNG-touching gestures (BReseed at a 32-step pattern
-- | boundary, BReset). These are the gestures a frontend broadcasts; both runtimes
-- | replay them in lockstep.
balistesInputScript :: Array RBI.BTagged
balistesInputScript =
  [ { tick: 5, input: RBI.BSetX 200 }
  , { tick: 5, input: RBI.BSetY 60 }
  , { tick: 12, input: RBI.BSetDensity 0 220 }
  , { tick: 20, input: RBI.BSetRandomness 180 }
  , { tick: 20, input: RBI.BSetOpen 120 }
  , { tick: 32, input: RBI.BReseed }
  , { tick: 40, input: RBI.BSetPush 1 12 }
  , { tick: 55, input: RBI.BSetNote 1 40 }
  , { tick: 64, input: RBI.BSetRatchet 2 4 3 }
  , { tick: 90, input: RBI.BReset }
  , { tick: 120, input: RBI.BSetDensity 2 240 }
  , { tick: 150, input: RBI.BSetY 200 }
  ]

-- | Apply one tick-tagged input THROUGH THE CODEC (encode then decode before apply),
-- | so `encodeBTagged`/`decodeBTagged` (both reef functions, JS + BEAM) are on the
-- | critical path — the cross-runtime diff proves the codec is byte-faithful and a
-- | broken codec can't pass silently.
applyBEncoded :: BSim.BalSim -> RBI.BTagged -> BSim.BalSim
applyBEncoded st t = case BSim.decodeBTagged (BSim.encodeBTagged t) of
  Right dt -> RBI.applyBInput dt.input st
  Left _ -> st

-- | The Balistes live-input net. At each step: apply any scheduled inputs (codec
-- | round-tripped), then stepBal, then digest state + renderStep output. Byte-
-- | identical node ↔ BEAM proves the input protocol AND its codec behave identically
-- | on both runtimes — what lets the frontend broadcast tick-tagged knob edits and
-- | have the rig apply them to the same model step.
balistesInputRun :: String
balistesInputRun =
  intercalate "\n"
    (foldl advance { bal: BSim.defaultBalSim, out: [] } (range 0 (balistesInputSteps - 1))).out
  where
  advance acc i =
    let
      due = filter (\t -> t.tick == i) balistesInputScript
      bal1 = foldl applyBEncoded acc.bal due
      playedStep = bal1.step
      r = BSim.stepBal bal1
      evs = BSim.renderStep bal1 playedStep r.fired
    in
      { bal: r.bal, out: snoc acc.out (balInputDig i bal1 evs) }

balInputDig :: Int -> BSim.BalSim -> Array { note :: Int, velocity :: Int, pushMs :: Int, durMs :: Number, ratchet :: Int } -> String
balInputDig i b evs =
  pad4 i <> " x" <> pad3 b.x <> " y" <> pad3 b.y
    <> " r" <> pad3 b.randomness <> " o" <> pad3 b.open
    <> " | " <> (if null evs then "-" else intercalate "  " (map one evs))
  where
  one e = show e.note <> "/" <> show e.velocity <> "/" <> show e.pushMs
    <> "/" <> show (round e.durMs) <> "x" <> show e.ratchet

-- ── shared ───────────────────────────────────────────────────────────────────

pad3 :: Int -> String
pad3 n =
  let s = show n
  in if n < 10 then "  " <> s else if n < 100 then " " <> s else s

pad4 :: Int -> String
pad4 n =
  let s = show n
  in if n < 10 then "   " <> s else if n < 100 then "  " <> s else if n < 1000 then " " <> s else s
