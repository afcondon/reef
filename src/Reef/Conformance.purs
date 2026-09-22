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
  , fixedRun, fixedRunSteps
  , trigRun, trigRunSteps
  , vetulaRun, vetulaRunSteps
  , vetulaMidiRun
  , chordRun
  , stellatusRun, stellatusRunSteps
  , conspicillumRun, conspicillumGrains
  , conspicillumCloudRun, conspicillumCycles
  , conspicillumHarmonicRun, conspicillumProgression
  ) where

import Prelude

import Data.Array (filter, length, null, range, snoc, mapWithIndex, (!!))
import Data.Either (Either(..))
import Data.Foldable (foldl, intercalate)
import Data.Int (round)
import Data.Maybe (Maybe(..), maybe)
import Reef.Balistes.Engine (Trigger, evaluateStep, freshPerturbations) as Bal
import Reef.Balistes.Sim (BalSim, defaultBalSim, stepBal, renderStep) as BSim
import Reef.Balistes.Protocol (decodeBalSim, encodeBalSim, decodeBTagged, encodeBTagged, decodeFixed, encodeFixed, decodeTrigKit, encodeTrigKit) as BSim
import Reef.Balistes.Trig (TrigKit, renderTrigStep) as RTrig
import Reef.Balistes.Input (BInput(..), BTagged, applyBInput) as RBI
import Reef.Balistes.Fixed (FixedPattern, emptyCell, renderFixed) as RFix
import Reef.Odonus (Cell, Fired, Head, Odonus, defaultOdonus, stepEmit)
import Reef.Gen (GenKind(..), GenSource, genKinds, genDefaultRate, genDefaultAmt)
import Reef.Engine (stepTick)
import Reef.Input (Input(..), SimState, Tagged, applyInput, mkFollowChord)
import Reef.PitchSet (PitchSet(..))
import Reef.Protocol (decodeInput, decodeSim, encodeInput)
import Reef.Marbles (Seed, seedFrom, rollValue)
import Reef.Vetula.Perf (Perf, VDest(..), VRenderer(..), cursorAt, odoCursorAt, odoPcsAt, renderVoiceMidiAt) as VP
import Reef.Vetula.Protocol (decodePerf, encodePerf) as VP
import Reef.Stellatus.Engine (Scene, walk, walkLen, events) as SE
import Reef.Stellatus.Protocol (decodeScene, encodeScene) as SP
import Reef.Conspicillum.Corpus
  (Axis(..), Cmp(..), Grainable, Toward(..), grainAt, pick) as CC
import Reef.Conspicillum.Protocol (Scene, decodeScene, encodeScene) as CP
import Reef.Conspicillum.Cloud (Op(..), Spec, When(..), cycleOf) as CL
import Reef.Conspicillum.Harmonic (Target, fit) as CH

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

-- | The chord-quantised render golden — the coverage gap that let a wrong-octave
-- | bug through (the chord overlay path renders pitches, but no golden exercised it:
-- | `run` is chord-off, `inputRun` digests cell INDICES not sounding pitches). A → odo
-- | Vetula feed is simulated with `mkFollowChord` (turning the chord overlay on), then
-- | 32 steps are rendered showing the SOUNDING pitch. This pins that the chord path
-- | realizes the cell index to a melodic pitch and snaps it to the nearest chord tone
-- | (sane octaves), and — via cross-runtime.sh — that node and the BEAM agree on it.
chordRun :: String
chordRun =
  let
    s0 = applyInput (mkFollowChord [ 0, 4, 7 ])
      { odo: defaultOdonus, gen: [], spread: 0.5, bias: 0.5, seed: seedFrom 1, frozen: false }
    final = foldl advance { st: s0, out: [] } (range 1 steps)
  in
    intercalate "\n" final.out
  where
  advance acc i =
    let r = stepTick acc.st
    in { st: r.sim, out: snoc acc.out (renderStep i r.fired) }

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
      r = stepTick { gen: activeGen, spread: 0.5, bias: 0.5, odo: acc.odo, seed: acc.seed, frozen: false }
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
-- | seed-threading rolls (RollAllNotes / SeedMelody). Several land on
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
    s0 = { odo: defaultOdonus, gen: activeGen, spread: 0.5, bias: 0.5, seed: seedFrom 1, frozen: false }
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
handoffJson = """{"spreadMille":500,"seedInt":7,"odo":{"span":3,"scaleIvls":[0,2,3,5,7,8,10],"rootPc":0,"octaveShift":0,"heads":[{"transp":0,"speedIx":4,"seqPos":0,"pulses":16,"pendStep":1,"patternIx":0,"offset":0,"mute":false,"len":16,"esteps":16,"direction":0,"cursor":0,"accumulator":0,"etick":0},{"transp":7,"speedIx":2,"seqPos":0,"pulses":16,"pendStep":1,"patternIx":1,"offset":0,"mute":true,"len":16,"esteps":16,"direction":0,"cursor":0,"accumulator":0,"etick":0},{"transp":-12,"speedIx":6,"seqPos":0,"pulses":16,"pendStep":1,"patternIx":3,"offset":0,"mute":true,"len":16,"esteps":16,"direction":1,"cursor":0,"accumulator":0,"etick":0},{"transp":3,"speedIx":3,"seqPos":0,"pulses":16,"pendStep":1,"patternIx":2,"offset":0,"mute":true,"len":16,"esteps":16,"direction":2,"cursor":0,"accumulator":0,"etick":0}],"gatePct":90,"dist":"natural","degShift":0,"chord":{"picks":[15,10,12,13],"phase":0,"period":16,"on":false,"ix":0,"feed":[]},"cells":[{"vel":100,"skip":false,"ratchet":1,"note":0,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":1,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":2,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":3,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":4,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":5,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":6,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":7,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":8,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":9,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":10,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":11,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":12,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":13,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":14,"glide":false,"gate":true,"dur":1},{"vel":100,"skip":false,"ratchet":1,"note":15,"glide":false,"gate":true,"dur":1}]},"gen":[{"rate":96,"on":false,"kind":0,"amt":20},{"rate":96,"on":true,"kind":1,"amt":30},{"rate":96,"on":true,"kind":2,"amt":30},{"rate":96,"on":true,"kind":3,"amt":30},{"rate":72,"on":true,"kind":4,"amt":25},{"rate":72,"on":true,"kind":5,"amt":25},{"rate":96,"on":true,"kind":6,"amt":30},{"rate":96,"on":true,"kind":7,"amt":40},{"rate":96,"on":true,"kind":8,"amt":30},{"rate":96,"on":true,"kind":9,"amt":30},{"rate":96,"on":true,"kind":10,"amt":25}],"biasMille":500,"frozen":false}"""

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

-- ── 7. the fixed-rhythm net (Balistes AFixed lockstep) ───────────────────────

-- | How many absolute steps the fixed-rhythm run threads (8 loops of 16 steps, so
-- | the per-loop trig condition and probability variation both show).
fixedRunSteps :: Int
fixedRunSteps = 128

-- | A representative fixed rhythm exercising every eval hazard: BD quarters, SD
-- | backbeats (one with a ratchet), CH eighths at probability 80 (drives the
-- | deterministic cellHash — the multiplication path that must agree across
-- | runtimes), and an OH that fires only every 2nd loop (condX/condY). Notes are the
-- | GM-ish per-lane defaults; grid built programmatically to stay compact.
fixedTestPattern :: RFix.FixedPattern
fixedTestPattern =
  { steps: 16
  , notes: map (\l -> 36 + l) (range 0 15)
  , grid: map laneRow (range 0 15)
  }
  where
  laneRow lane = map (cellFor lane) (range 0 15)
  cellFor lane step = case lane of
    0 -> if step `mod` 4 == 0 then hit 110 1 else RFix.emptyCell
    1 ->
      if step == 4 then hit 100 1
      else if step == 12 then hit 100 3
      else RFix.emptyCell
    4 -> if step `mod` 2 == 0 then RFix.emptyCell { vel = 70, prob = 80 } else RFix.emptyCell
    6 -> if step == 14 then RFix.emptyCell { vel = 90, condX = 2, condY = 2 } else RFix.emptyCell
    _ -> RFix.emptyCell
  hit v r = RFix.emptyCell { vel = v, ratchet = r }

-- | The fixed-rhythm net. The pattern is round-tripped THROUGH the codec (encodeFixed
-- | then decodeFixed — both reef functions, JS + BEAM) then rendered at each absolute
-- | step by the shared `renderFixed`. Byte-identical node ↔ BEAM proves the fixed-
-- | rhythm eval (incl. the cellHash multiplications + the trig conditions) and its
-- | codec behave identically — so `reef_balistes_voice` can play a pushed fixed
-- | rhythm in lockstep. A decode failure screams rather than passing silently.
fixedRun :: String
fixedRun = case BSim.decodeFixed (BSim.encodeFixed fixedTestPattern) of
  Left errs -> "FIXED-DECODE-FAIL: " <> show errs
  Right p -> intercalate "\n" (map (\i -> evDig i (RFix.renderFixed p i)) (range 0 (fixedRunSteps - 1)))

-- ── 7½. the Balistes POLYTRIG net (the TIDAL-tab / ASelene lockstep) ──────────

-- | How many absolute steps the POLYTRIG run threads. 32 = two 16-step cycles, so
-- | the mod-cycle wrap is exercised.
trigRunSteps :: Int
trigRunSteps = 32

-- | A representative resolved rack exercising the slicing hazards: an on-grid four-
-- | on-the-floor (frac always 0), an x*8 hat (two onsets per… no — one per step,
-- | frac 0), and — crucially — an OFF-GRID jack whose onsets land mid-step, so
-- | `frac = onset * cycleSteps - step` is a non-trivial float that must agree across
-- | runtimes (the multiply the frontend and BEAM both compute). The onsets are the
-- | cycle-0 fractions the frontend's `Tidal.Lane` would resolve; reef carries no
-- | parser, so the kit is given directly (as the wire push does).
trigTestKit :: RTrig.TrigKit
trigTestKit =
  [ { note: 36, onsets: [ 0.0, 0.25, 0.5, 0.75 ] }
  , { note: 42, onsets: [ 0.0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875 ] }
  , { note: 39, onsets: [ 0.1, 0.3333333333333333, 0.6666666666666666, 0.95 ] }
  ]

-- | The POLYTRIG net. The kit is round-tripped THROUGH the codec (encodeTrigKit then
-- | decodeTrigKit — both reef functions, JS + BEAM) then sliced at each absolute step
-- | by the shared `renderTrigStep`. Byte-identical node ↔ BEAM proves the rack slicing
-- | (incl. the fractional-onset multiply) + its codec behave identically — so
-- | `reef_balistes_voice` plays a pushed rack in lockstep with the frontend's ASelene
-- | branch. A decode failure screams rather than passing silently.
trigRun :: String
trigRun = case BSim.decodeTrigKit (BSim.encodeTrigKit trigTestKit) of
  Left errs -> "TRIG-DECODE-FAIL: " <> show errs
  Right kit -> intercalate "\n" (map (\i -> trigDig i (RTrig.renderTrigStep kit i 16)) (range 0 (trigRunSteps - 1)))

-- | Digest a step's fires. The fractional onset is rounded to an integer 1e6 grid
-- | before `show` — NOT `show`n raw: purerl's `show :: Number` renders in a different
-- | (longer, scientific) form than JS's shortest-round-trip, so two IDENTICAL doubles
-- | would print differently and spuriously fail the diff. Rounding to a fine integer
-- | grid (µ-step resolution, far finer than audible) renders identically on both
-- | runtimes while still catching any real value divergence — the same discipline the
-- | other goldens use (they `round durMs`). 1e6 of a step ≈ sub-microsecond at tempo.
trigDig :: Int -> Array { note :: Int, frac :: Number } -> String
trigDig i fires =
  pad4 i <> " | " <> (if null fires then "-" else intercalate "  " (map one fires))
  where
  one f = show f.note <> "@" <> show (round (f.frac * 1000000.0))

-- ── 8. the Vetula performance-scheduler net (Vetula lockstep V1) ──────────────

-- | How many absolute pulses (1/16 notes) the Vetula run threads. 128 = 8 bars,
-- | past the longest voice loop (v0 dwells 2 bars × 4 chords = 8 bars = 128
-- | pulses), so every voice wraps and every rest/skip shows.
vetulaRunSteps :: Int
vetulaRunSteps = 128

-- | A representative performance mirroring the Performance-tab screenshot: a 4-chord
-- | progression (C E A B) fanned to voices with DIFFERENT per-chord dwell schedules,
-- | skips, renderers and destinations — block every chord 2 bars, arp skipping E,
-- | strum only the last chord (4 bars), the → odo conductor one bar each, and a
-- | phase-offset block to drive the `(pulse + phase)` wrap. The pcs are
-- | representative (the scheduler is pcs-agnostic); notes carry `playNotes`-shaped
-- | values so the V2 MIDI path has data to grow into.
vetulaPerf :: VP.Perf
vetulaPerf =
  { chords:
      [ { pcs: [ 0, 4, 7 ], notes: [ 36, 60, 64, 67 ] }
      , { pcs: [ 4, 7, 11 ], notes: [ 40, 64, 67, 71 ] }
      , { pcs: [ 9, 0, 4 ], notes: [ 33, 57, 60, 64 ] }
      , { pcs: [ 11, 2, 6 ], notes: [ 35, 59, 62, 66 ] }
      ]
  , voices:
      [ { dest: VP.VToMidi, renderer: VP.VBlock, channel: 0, durs: [ 2, 2, 2, 2 ], phase: 0, muted: false }
      , { dest: VP.VToMidi, renderer: VP.VArp, channel: 1, durs: [ 2, 0, 1, 1 ], phase: 0, muted: false }
      , { dest: VP.VToMidi, renderer: VP.VStrummed, channel: 2, durs: [ 1, 1, 1, 1 ], phase: 0, muted: false }
      , { dest: VP.VToOdonus, renderer: VP.VBlock, channel: 3, durs: [ 1, 1, 1, 1 ], phase: 0, muted: false }
      , { dest: VP.VToMidi, renderer: VP.VBlock, channel: 4, durs: [ 1, 1, 1, 1 ], phase: 8, muted: false }
      ]
  }

-- | The Vetula V1 net. The performance is round-tripped THROUGH the codec (encodePerf
-- | then decodePerf — both reef functions, JS + BEAM) then, at each absolute pulse,
-- | we record every voice's read-head (`cursorAt`, `-` = resting/skipped) plus the
-- | → odo conductor's HELD cursor and the pitch-class set it feeds Odonus
-- | (`odoCursorAt`/`odoPcsAt`, threading the held cursor across rests exactly as the
-- | frontend's `fromMaybe v.cursor`). Byte-identical node ↔ BEAM proves the whole
-- | shared scheduler + its codec agree, which is what lets `reef_vetula_voice`
-- | conduct the rig's Odonus in lockstep with the browser. A decode failure screams.
vetulaRun :: String
vetulaRun = case VP.decodePerf (VP.encodePerf vetulaPerf) of
  Left errs -> "VETULA-DECODE-FAIL: " <> show errs
  Right perf ->
    let nCh = length perf.chords
        final = foldl (advance perf nCh) { cursor: -1, out: [] } (range 0 (vetulaRunSteps - 1))
    in intercalate "\n" final.out
  where
  advance perf nCh acc pulse =
    let cur = VP.odoCursorAt perf pulse acc.cursor
        pcs = VP.odoPcsAt perf cur
        cursors = map (\v -> maybe "-" show (VP.cursorAt nCh v pulse)) perf.voices
        line = pad4 pulse <> " | " <> intercalate " " cursors
          <> " | odo" <> show cur <> " [" <> intercalate "," (map show pcs) <> "]"
    in { cursor: cur, out: snoc acc.out line }

-- ── 9. the Vetula MIDI-render net (Vetula lockstep V2a) ───────────────────────

-- | The V2a net: the shared `renderVoiceMidiAt` (block + arp → gated notes) evaluated
-- | at each absolute pulse over 128 pulses for the same performance `vetulaRun` uses.
-- | For each → midi voice we record the notes it sounds this pulse (note/vel/gate in
-- | pulses). Byte-identical node ↔ BEAM proves the MIDI-render decision agrees on both
-- | runtimes — so reef_vetula_voice emits the same notes the browser's stepVoice does,
-- | the self-contained sync leg (no Odonus) validated before wiring the emit. A decode
-- | failure screams. (Strummed voices render nothing yet — V2b.)
vetulaMidiRun :: String
vetulaMidiRun = case VP.decodePerf (VP.encodePerf vetulaPerf) of
  Left errs -> "VETULA-MIDI-DECODE-FAIL: " <> show errs
  Right perf ->
    intercalate "\n" (map (line perf) (range 0 (vetulaRunSteps - 1)))
  where
  line perf pulse =
    pad4 pulse <> " | "
      <> intercalate " | " (mapWithIndex (\vi v -> voiceCol vi (VP.renderVoiceMidiAt perf.chords v pulse)) perf.voices)
  voiceCol vi evs =
    "v" <> show vi <> ":" <> (if null evs then "-" else intercalate "," (map one evs))
  one e = show e.note <> "/" <> show e.velocity <> "/" <> show (round (e.durPulses * 100.0))

-- ── 10. the Stellatus ring re-sequencer (Stellatus BEAM wiring A) ─────────────

-- | The Stellatus net: the shared `walk` (grid-locked arc walk + weighted jumps)
-- | and `events` (per-step `/dirt/play` bag with glitch-folded speed) computed
-- | over the fixed loop, indexed by `step `mod` walkLen` across 96 absolute steps.
-- | The scene mirrors Triggerfish's default PLAYER text (bd/sn/hh*2/cp/sn, the
-- | `# speed "1 1 2 1 0.5"` sampled per arc, `# sometimes rev` + `# rarely
-- | (# speed 2)`, the jump matrix). Round-tripped through the codec first (a
-- | decode failure screams). Byte-identical node ↔ BEAM proves reef_stellatus_voice
-- | emits exactly what the browser visualizer walks. Floats printed as ×100 ints
-- | so `show Number` can't diverge. Speed is signed (reverse = negative).
stellatusRunSteps :: Int
stellatusRunSteps = 96

stellatusScene :: SE.Scene
stellatusScene =
  { slots:
      -- a couple of slots carry the optional tranche so the golden proves it
      -- crosses the wire byte-identically (cut/legato/accelerate/pan on the snare,
      -- crush/coarse on the clap, cutoff/resonance on a hat).
      [ slot "bd" 0.0 0.2 "808bd" 3 0.0 1.0 1.0 1.0
      , (slot "sn" 0.2 0.2 "sn" 4 0.0 1.0 1.0 0.9)
          { cut = 1.0, legato = 0.5, accelerate = 0.3, pan = 0.2 }
      , (slot "hh" 0.4 0.1 "hh27" 6 0.5 1.0 2.0 0.8)
          { cutoff = 1200.0, resonance = 0.4, pan = 0.8 }
      , slot "hh" 0.5 0.1 "hh27" 6 0.5 1.0 2.0 0.8
      , (slot "cp" 0.6 0.2 "cp" 1 0.0 1.0 1.0 1.0)
          { crush = 4.0, coarse = 8.0 }
      , slot "sn" 0.8 0.2 "sn" 4 0.0 1.0 0.5 0.85
      ]
  , glitch:
      [ { prob: 0.5, kind: 0, amount: 0.0 } -- sometimes rev
      , { prob: 0.25, kind: 1, amount: 2.0 } -- rarely (# speed 2)
      ]
  , jumps:
      { prob: 0.22
      , table:
          [ { from: "bd", targets: [ { name: "sn", weight: 0.6 }, { name: "hh", weight: 0.4 } ] }
          , { from: "sn", targets: [ { name: "cp", weight: 0.5 }, { name: "bd", weight: 0.5 } ] }
          , { from: "hh", targets: [ { name: "hh", weight: 0.7 }, { name: "sn", weight: 0.3 } ] }
          , { from: "cp", targets: [ { name: "bd", weight: 1.0 } ] }
          ]
      }
  , seed: 3
  }
  where
  slot nm on sp s n bg en spd gn =
    { name: nm, onset: on, span: sp, s, n, begin: bg, end: en, speed: spd, gain: gn
    , cut: 0.0, legato: 0.0, accelerate: 0.0, pan: -1.0
    , crush: 0.0, coarse: 0.0, cutoff: 0.0, resonance: 0.0 }

stellatusRun :: String
stellatusRun = case SP.decodeScene (SP.encodeScene stellatusScene) of
  Left errs -> "STELLATUS-DECODE-FAIL: " <> show errs
  Right scene ->
    let evs = SE.events scene
        wk = SE.walk scene
        len = SE.walkLen (length scene.slots)
    in intercalate "\n" (map (line evs wk len) (range 0 (stellatusRunSteps - 1)))
  where
  line evs wk len step =
    let li = step `mod` len
        from = maybe "-" show (join (map _.from (wk !! li)))
    in pad4 step <> " | li" <> pad3 li <> " from" <> from <> " | " <> evCol (evs !! li)
  evCol = case _ of
    Nothing -> "-"
    Just e ->
      e.s <> ":" <> show e.n
        <> " sp" <> show (round (e.speed * 100.0))
        <> " b" <> show (round (e.begin * 100.0)) <> " e" <> show (round (e.end * 100.0))
        <> " g" <> show (round (e.gain * 100.0))
        -- optional tranche: only printed when set, so unset slots keep short lines.
        <> opt "cut" e.cut <> opt "leg" e.legato <> opt "acc" e.accelerate
        <> (if e.pan >= 0.0 then " pan" <> show (round (e.pan * 100.0)) else "")
        <> opt "cru" e.crush <> opt "coa" e.coarse
        <> opt "cf" e.cutoff <> opt "res" e.resonance
  opt label v = if v /= 0.0 then " " <> label <> show (round (v * 100.0)) else ""

-- ── shared ───────────────────────────────────────────────────────────────────

pad3 :: Int -> String
pad3 n =
  let s = show n
  in if n < 10 then "  " <> s else if n < 100 then " " <> s else s

pad4 :: Int -> String
pad4 n =
  let s = show n
  in if n < 10 then "   " <> s else if n < 100 then "  " <> s else if n < 1000 then " " <> s else s


-- ── 12. Conspicillum: the grain selector ─────────────────────────────────────

-- | How many grains the golden draws. Enough that a weighting drift shows up
-- | as a changed distribution rather than as one unlucky draw.
conspicillumGrains :: Int
conspicillumGrains = 64

-- | A synthetic Quadrat set, built so that every branch of the selector is
-- | exercised by the golden rather than merely compiled:
-- |
-- |   * both axis families vary — measured (`zcr`, `decay`, …) and intentional
-- |     (`cell`, and a `harm` parameter);
-- |   * two samples are DEFICIENT on purpose: one has a `cell` too short to
-- |     answer `ACell 1`, one carries no `harm` parameter. The rule is that a
-- |     sample which cannot answer an axis is EXCLUDED, not defaulted, and a
-- |     golden that never contains such a sample would not notice that rule
-- |     being quietly reversed;
-- |   * durations differ, so `grainAt`'s window arithmetic is not accidentally
-- |     uniform.
conspicillumCorpus :: Array CC.Grainable
conspicillumCorpus =
  [ g 0 4.0 0.9 0.30 1200.0 0.62 3.1 [ 0, 0 ] [ p "harm" 0.10 ]
  , g 1 4.2 0.8 0.28 900.0 0.41 2.4 [ 0, 1 ] [ p "harm" 0.35 ]
  , g 2 3.9 1.0 0.34 1800.0 0.78 1.2 [ 0, 2 ] [ p "harm" 0.60 ]
  , g 3 8.0 0.7 0.22 400.0 0.19 6.5 [ 1, 0 ] [ p "harm" 0.85 ]
  , g 4 7.5 0.95 0.31 2400.0 0.88 0.8 [ 1, 1 ] [ p "harm" 0.05 ]
  , g 5 0.6 0.5 0.12 600.0 0.25 0.4 [ 1, 2 ] [ p "harm" 0.50 ]
  , g 6 12.0 0.85 0.27 1500.0 0.70 9.0 [ 2 ] [ p "harm" 0.95 ]   -- cell too short
  , g 7 5.5 0.6 0.19 1100.0 0.55 2.0 [ 2, 1 ] []                 -- no `harm`
  ]
  where
  g ix secs peak rms zcr tilt decay cell params =
    { index: ix, secs, peak, rms, zcr, tilt, decay, cell, params, notes: [] }
  p nm level = { name: nm, level }

-- | The scene: a filter on a MEASURED axis and one on an INTENTIONAL axis
-- | together, leaning toward the bright end of what survives.
conspicillumScene :: CP.Scene
conspicillumScene =
  { corpus: { name: "golden", samples: conspicillumCorpus }
  , query:
      { clauses:
          [ { axis: CC.ADecay, cmp: CC.Gt, value: 0.5 }       -- measured
          , { axis: CC.AParam "harm", cmp: CC.Lte, value: 0.9 } -- intentional
          ]
      , weighting: Just { axis: CC.AZcr, toward: CC.High, strength: 0.8 }
      -- C2 and C3 stay harmonically unconstrained, so their frozen goldens keep
      -- their meaning: adding the axis must not move what they measure.
      , harmonic: Nothing
      }
  -- The scene now carries the whole Spec, so ONE push is the entire cross-
  -- runtime wire — and `conspicillumCloudRun` reading its spec back out of the
  -- DECODED scene means the C3 golden also covers the rule projection: a
  -- broken `When`/`Op` encode would move it.
  , spec: conspicillumSpec
  , seed: 12345
  }

-- | Draw the cloud and render it.
-- |
-- | **Rendered as scaled integers, not as Numbers.** `show` on a Number is a
-- | formatting decision and the two runtimes do not owe each other the same
-- | one; the golden has to compare the arithmetic, not the printer. Same
-- | reasoning as `betaProbe`, and the reason that probe renders Int draws.
-- |
-- | The scene is put through `encodeScene`/`decodeScene` first, so the golden
-- | also proves the wire projection of the three closed ADTs round-trips — a
-- | clause whose axis decoded to the house default would select different
-- | material and report no error at all.
conspicillumRun :: String
conspicillumRun = case CP.decodeScene (CP.encodeScene conspicillumScene) of
  Left errs -> "CONSPICILLUM-DECODE-FAIL: " <> show errs
  Right scene ->
    intercalate "\n" (draw conspicillumGrains (seedFrom scene.seed) [])
  where
  draw :: Int -> Seed -> Array String -> Array String
  draw 0 _ acc = acc
  draw n sd acc =
    let scene = conspicillumScene
        { chosen, seed: s1 } = CC.pick scene.query scene.corpus sd
    in case chosen of
      -- A query that admits nothing is a real state, and the golden says so
      -- rather than skipping: "the filter excluded everything" must not look
      -- like "the cloud is quiet".
      Nothing -> draw (n - 1) s1 (snoc acc "-")
      Just gr ->
        let { grain, seed: s2 } = CC.grainAt scene.spec.cloud gr s1
        in draw (n - 1) s2
             (snoc acc (show grain.n <> " " <> six grain.begin <> " " <> six grain.end))

  -- Six decimal places, as an integer. Comfortably inside the exactness of the
  -- divisions that produced it, and format-free.
  six :: Number -> String
  six x = show (round (x * 1000000.0))


-- ── 13. Conspicillum: the cloud over a cycle ─────────────────────────────────

-- | Cycles rendered, and the one asked for OUT OF ORDER.
-- |
-- | 0,1,2 in sequence then 7 on its own. The out-of-order cycle is the point:
-- | Conspicillum is cycle-ADDRESSED where Stellatus loops, so the browser can
-- | recompute cycle 7 without having simulated the six before it. If the seed
-- | were threaded rather than derived, cycle 7 alone would differ from cycle 7
-- | reached by playing — and the visualizer would be quietly wrong whenever the
-- | player dropped into a running set.
conspicillumCycles :: Array Int
conspicillumCycles = [ 0, 1, 2, 7 ]

-- | Eight onsets, unevenly placed, so `at` is not recoverable from the index
-- | and a drift in onset handling cannot hide behind regular spacing.
conspicillumSpec :: CL.Spec
conspicillumSpec =
  { onsets: [ 0.0, 0.125, 0.1875, 0.375, 0.5, 0.625, 0.6875, 0.875 ]
  , cloud: { sustain: 0.05, position: 0.4, spray: 0.3 }
  , rules:
      -- The headline, and the thing no hardware granulator can express: every
      -- third grain, counted ACROSS cycles, plays backwards. Eight onsets a
      -- cycle against a period of three means the figure walks — grains 0,3,6
      -- in the first cycle, 9,12,15 (= indices 1,4,7) in the second — which is
      -- precisely what the golden has to pin, and what a per-cycle reset would
      -- silently destroy.
      [ { when: CL.Every 3 0, op: CL.OpSpeed (-1.0) }
      -- And a seeded one beside it, so the golden covers both kinds of `when`
      -- and the draw order between them.
      , { when: CL.Chance 0.25, op: CL.OpGain 0.5 }
      ]
  , speed: 1.0
  , gain: 0.8
  , pan: 0.5
  , accelerate: 0.0
  }

-- | Render the cloud for each cycle.
-- |
-- | Scaled integers, for the reason `conspicillumRun` and `betaProbe` give: a
-- | Number's printed form is a formatting decision the two runtimes do not owe
-- | each other, and this golden compares arithmetic.
conspicillumCloudRun :: String
conspicillumCloudRun = case CP.decodeScene (CP.encodeScene conspicillumScene) of
  Left errs -> "CONSPICILLUM-CLOUD-DECODE-FAIL: " <> show errs
  Right scene ->
    intercalate "\n" (map (renderCycle scene) conspicillumCycles)
  where
  renderCycle scene cyc =
    let es = CL.cycleOf scene.corpus scene.query scene.spec scene.seed cyc
    in intercalate "\n" (mapWithIndex (renderEmit cyc) es)

  renderEmit cyc i e =
    "c" <> show cyc <> " g" <> show i
      <> " at " <> six e.at
      <> " n " <> show e.n
      <> " b " <> six e.begin
      <> " sp " <> six e.speed
      <> " gn " <> six e.gain

  six :: Number -> String
  six x = show (round (x * 1000000.0))


-- ── 14. Conspicillum: realising a progression onto recorded chord hits ───────

-- | **A real corpus.** Every one of these is an actual Quadrat chord hit, with
-- | the MIDI notes that were really struck and the measurements really taken —
-- | pulled from the `chord-hits-*` sets on 2026-09-22. Synthetic numbers would
-- | not have told us what the last two rows tell us.
-- |
-- | Read as chords, they are: Gm(maj7), Em, a five-note cluster, Fm, another
-- | cluster, A major, D minor, Dm6 — then a SMEAR of eleven pitch classes and
-- | a hit with NO recorded notes at all. Those last two are not padding. Of the
-- | 136 chord hits recorded so far, 24 carry no notes and 7 are smears, and a
-- | smear covers every chord perfectly on coverage alone. A golden without them
-- | would not notice the instrument learning to prefer its broken material.
conspicillumChordCorpus :: Array CC.Grainable
conspicillumChordCorpus =
  [ g 0 7.77 0.3405 0.0431 1153.8 0.2248 7.4100 [43, 55, 66, 70, 74]   -- clean: pcs [2, 6, 7, 10]
  , g 1 7.88 0.3406 0.0470 942.0 0.1603 7.5947 [40, 52, 55, 64, 71]   -- clean: pcs [4, 7, 11]
  , g 2 7.83 0.4020 0.0506 1104.4 0.1969 7.5149 [39, 51, 62, 65, 67, 72]   -- clean: pcs [0, 2, 3, 5, 7]
  , g 3 8.45 0.4281 0.0488 992.2 0.1728 7.9932 [41, 53, 56, 65, 72]   -- clean: pcs [0, 5, 8]
  , g 4 8.06 0.4592 0.0584 1100.1 0.1949 7.7206 [37, 49, 59, 64, 68, 75]   -- clean: pcs [1, 3, 4, 8, 11]
  , g 5 9.34 0.4427 0.0584 1054.5 0.1892 8.3575 [45, 57, 64, 69, 73]   -- clean: pcs [1, 4, 9]
  , g 6 16.81 0.8394 0.1352 929.1 0.1379 15.3199 [38, 57, 62, 65, 74]   -- clean: pcs [2, 5, 9]
  , g 7 17.12 0.9175 0.1379 1210.9 0.1652 15.6311 [38, 50, 65, 69, 71]   -- clean: pcs [2, 5, 9, 11]
  , g 8 7.56 0.3361 0.0384 1177.0 0.2063 7.1482 [0, 1, 2, 3, 4, 5, 6, 13, 14, 16, 17, 18, 23, 25, 33, 34, 36, 38, 40, 45, 57, 62, 65, 73, 74, 75, 79, 86, 90, 91, 95, 102, 105, 115, 120, 123]   -- smear: pcs [0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11]
  , g 9 7.83 0.2558 0.0320 1138.3 0.2036 7.0129 []   -- empty: pcs []
  ]
  where
  g ix secs peak rms zcr tilt decay notes =
    { index: ix, secs, peak, rms, zcr, tilt, decay
    , cell: [], params: [], notes }

-- | A ii-V-i in D minor, as (name, target). Roots and basses are given
-- | explicitly because a realised `Harmonia.Chord` is SORTED and its root
-- | cannot be recovered from the set — see `Reef.Conspicillum.Harmonic`.
conspicillumProgression :: Array { name :: String, target :: CH.Target }
conspicillumProgression =
  [ { name: "Em7b5", target: { pcs: [ 4, 7, 10, 2 ], root: 4, bass: 4 } }
  , { name: "A7",    target: { pcs: [ 9, 1, 4, 7 ], root: 9, bass: 9 } }
  , { name: "Dm",    target: { pcs: [ 2, 5, 9 ], root: 2, bass: 2 } }
  , { name: "Dm6",   target: { pcs: [ 2, 5, 9, 11 ], root: 2, bass: 2 } }
  ]

-- | For each chord of the progression, every sample's fit.
-- |
-- | This is the claim the whole instrument was proposed for, made checkable:
-- | the SAME corpus ranks differently under each chord, so a progression is
-- | realised onto recorded voicings rather than transposed onto one sample.
-- | Nothing else in the stack can do it, because nothing else recorded what it
-- | sampled.
-- |
-- | Scaled integers, for the reason the other two Conspicillum goldens give.
conspicillumHarmonicRun :: String
conspicillumHarmonicRun =
  intercalate "\n" (map row conspicillumProgression)
  where
  row c =
    pad6 c.name <> " |" <> intercalate ""
      (map (\s -> " " <> pad4 (round (CH.fit c.target s.notes * 1000.0)))
        conspicillumChordCorpus)

  -- Padded numerically rather than by string length: reef does not depend on
  -- `strings`, and a fit is always 0..1000 here, so the cases are exhaustive.
  pad4 :: Int -> String
  pad4 n =
    let t = show n
    in if n >= 1000 then t
       else if n >= 100 then " " <> t
       else if n >= 10 then "  " <> t
       else "   " <> t

  pad6 :: String -> String
  pad6 nm = case nm of
    "Dm" -> "Dm    "
    "A7" -> "A7    "
    "Dm6" -> "Dm6   "
    "Em7b5" -> "Em7b5 "
    _ -> nm
