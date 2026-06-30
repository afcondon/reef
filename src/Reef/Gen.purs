-- | The randomisation engine — and the gen-source descriptor it consumes —
-- | living in `reef` so the *whole* Odonus module (engine + generation) is one
-- | source of truth across the Triggerfish JS frontend and the purerl-tidal
-- | Erlang backend. This is the last un-shared piece: with it here, the rig can
-- | generate autonomously and a frontend co-simulation stays bit-identical
-- | (the lockstep foundation — see reef/docs/PLAN-lockstep-cosimulation.md).
-- |
-- | A matrix of independent slow-drift SOURCES, each with its own firing rate (a
-- | bare period — "one change per N steps"). On every model step each enabled
-- | source draws from the shared PRNG and, with probability 1/period, mutates
-- | ONE random element of its domain by a single notch. Keeping each change tiny
-- | and rare lets several sources run at once without descending into chaos —
-- | the texture evolves, it doesn't scramble.
-- |
-- | The NOTES source draws pitches from the Marbles Beta distribution (so the
-- | X-Y pad still shapes where new notes land); the others walk discrete
-- | parameters. This is pure: same state + same seed → same mutation. The only
-- | transcendental anywhere in the step path is `pow` (inside the Beta weights,
-- | reached only via GNotes); everything else is exact integer / dyadic-float
-- | arithmetic, hence provably identical across runtimes.
module Reef.Gen
  ( GenKind(..)
  , GenSource
  , genKinds
  , genDefaultRate
  , genDefaultAmt
  , rateMax
  , periodOf
  , toggleGen
  , setRate
  , setAmt
  , GenInput
  , runGen
  , rollAllNotes
  , rollChords
  , seedMelody
  ) where

import Prelude

import Data.Array (range, (!!))
import Data.Foldable (foldl)
import Data.Int (round, toNumber)
import Data.Int.Bits (and, shl)
import Data.Maybe (maybe)
import Reef.Numeric (pow)
import Reef.Odonus as M
import Reef.Marbles as Marbles

-- ── the gen-source descriptor ────────────────────────────────────────────────

-- | A randomisation aspect: one independent slow-drift source. Each picks a
-- | random element of its domain when it fires and mutates it by one notch —
-- | a steady, low-probability evolution rather than a one-shot scramble.
-- | (OFFSET and head LENGTH are deliberately excluded — those get direct
-- | Reichian phase controls instead.)
data GenKind
  = GNotes      -- reroll one cell's note from the Marbles Beta distribution
  | GGate       -- occasionally rest a step (biased toward mostly-gated)
  | GSkip       -- occasionally drop a step (biased toward few skips)
  | GGlide      -- occasionally tie/slew a step (biased toward few glides)
  | GLen        -- drift one cell's note length ±1
  | GRatchet    -- occasionally ratchet a step (biased toward few rolls), like GLen
  | GHeads      -- walk the active-playhead combination (one bit on the 4-cube)
  | GTransp     -- nudge one head's scalar transpose
  | GPattern    -- advance one head's access pattern
  | GSpeed      -- nudge one head's speed
  | GKey        -- shift key by a fifth, change mode, or toggle a scale note

derive instance eqGenKind :: Eq GenKind

genKinds :: Array GenKind
genKinds = [ GNotes, GGate, GSkip, GGlide, GLen, GRatchet, GHeads, GTransp, GPattern, GSpeed, GKey ]

-- | One source's stored config: enabled, a rate index 0..`rateMax` (→ firing
-- | period via `periodOf`), and an `amt` 0..100 giving the mutation DEPTH —
-- | how big each change is when the source fires (a small constant nudge vs a
-- | proper shake-up). The two axes are independent: how OFTEN, and how MUCH.
type GenSource = { kind :: GenKind, on :: Boolean, rate :: Int, amt :: Int }

-- | A source's initial rate index. LEN drifts more freely (a lower index = a
-- | shorter period = more frequent) since note-length changes read as phrasing,
-- | not chaos; everything else starts conservative.
genDefaultRate :: GenKind -> Int
genDefaultRate = case _ of
  GLen -> 72
  GRatchet -> 72
  _ -> 96

-- | A source's initial mutation depth (0..100). Chosen so a single source,
-- | turned on alone, makes an audible difference — TRANSP needs more depth to
-- | clear re-quantization, KEY stays gentle (mostly fifths).
genDefaultAmt :: GenKind -> Int
genDefaultAmt = case _ of
  GNotes -> 20
  GTransp -> 40
  GKey -> 25
  GLen -> 25
  GRatchet -> 25
  _ -> 30

-- | The rate-index resolution. Drag the bare-number control across this range.
rateMax :: Int
rateMax = 200

-- | Map a rate index to a firing PERIOD in model steps — the bare number the
-- | control shows. Geometric from 1 (every step, chaos) up to ~16384 (a change
-- | roughly every few thousand steps, i.e. rare drift). One change per N steps.
periodOf :: Int -> Int
periodOf r =
  let rr = if r < 0 then 0 else if r > rateMax then rateMax else r
  in max 1 (round (pow 16384.0 (toNumber rr / toNumber rateMax)))

-- | Flip a source's enable.
toggleGen :: GenKind -> Array GenSource -> Array GenSource
toggleGen k = map \s -> if s.kind == k then s { on = not s.on } else s

-- | Set a source's rate index (from a drag on its bare-number control).
setRate :: GenKind -> Int -> Array GenSource -> Array GenSource
setRate k v = map \s -> if s.kind == k then s { rate = v } else s

-- | Set a source's mutation depth (from a drag on its AMT control).
setAmt :: GenKind -> Int -> Array GenSource -> Array GenSource
setAmt k v = map \s -> if s.kind == k then s { amt = v } else s

-- ── the engine ───────────────────────────────────────────────────────────────

type GenInput =
  { gen :: Array GenSource
  , spread :: Number
  , bias :: Number
  , odo :: M.Odonus
  , seed :: Marbles.Seed
  }

-- | Run every enabled source once over this step, threading the seed. Each
-- | source fires with probability 1/period; a firing applies one notch.
runGen :: GenInput -> { odo :: M.Odonus, seed :: Marbles.Seed }
runGen inp = foldl stepSrc { odo: inp.odo, seed: inp.seed } inp.gen
  where
  stepSrc acc src =
    if not src.on then acc
    else
      let { u, seed: s1 } = Marbles.nextRand acc.seed
      in if u < 1.0 / toNumber (periodOf src.rate)
         then applyGen src.kind inp.spread inp.bias src.amt acc.odo s1
         else acc { seed = s1 }

-- | One firing of a source. `amt` (0..100) is the DEPTH: how big the change is.
-- | For note rolls it's how many cells reroll; for the booleans it's the sparse
-- | density; for the nudges it's the step magnitude; for KEY it's the chance of
-- | a full modal change vs a gentle fifth. Note candidates stay chromatic
-- | (range 36..84) — the quantizer reins them into the scale.
applyGen
  :: GenKind -> Number -> Number -> Int -> M.Odonus -> Marbles.Seed
  -> { odo :: M.Odonus, seed :: Marbles.Seed }
applyGen kind spread bias amt odo seed =
  let amt01 = toNumber amt / 100.0
  in case kind of
    GNotes -> rerollNotes (1 + round (amt01 * 5.0)) bias spread odo seed
    -- A symmetric toggle drifts to ~50% on; these stay sparse instead — landing
    -- on the "rare" state always restores it, the common state only flips with a
    -- low probability (= depth). So gates stay mostly open, skips/glides occasional.
    GGate -> stepBias (amt01 * 0.5) (\c -> not c.gate) M.toggleGate odo seed
    GSkip -> stepBias (amt01 * 0.5) _.skip M.toggleSkip odo seed
    GGlide -> stepBias (amt01 * 0.5) _.glide M.toggleGlide odo seed
    GLen ->
      let mag = 1 + round (amt01 * 3.0)   -- ±1..±4
          { n: i, seed: s1 } = Marbles.nextInt 16 seed
          { n: d, seed: s2 } = Marbles.nextInt 2 s1
          cur = maybe 1 _.dur (odo.cells !! i)
      in { odo: M.setCellDur i (cur + (if d == 0 then -mag else mag)) odo, seed: s2 }
    -- Ratchets stay SPARSE (a ±drift would machine-gun the whole grid): landing
    -- on an already-ratcheted cell restores it to a single hit; a plain cell gets
    -- a 2..4 roll only with probability `amt01` (the depth). So rolls sparkle in
    -- and out rather than accumulating. Mirrors `stepBias`, but sets a count.
    GRatchet ->
      let { n: i, seed: s1 } = Marbles.nextInt 16 seed
          cur = maybe 1 _.ratchet (odo.cells !! i)
      in if cur > 1 then { odo: M.setCellRatchet i 1 odo, seed: s1 }
         else let { u, seed: s2 } = Marbles.nextRand s1
              in if u < amt01 * 0.5   -- like SKIP/GATE/GLIDE: a third of the grid at full depth
                 then let { n: r, seed: s3 } = Marbles.nextInt 3 s2   -- 2..4 hits
                      in { odo: M.setCellRatchet i (2 + r) odo, seed: s3 }
                 else { odo, seed: s2 }
    GHeads -> flipHeads (1 + round (amt01 * 2.0)) odo seed   -- 1..3 bits/fire
    GTransp ->
      let mag = 1 + round (amt01 * 11.0)   -- ±1..±12 semitones (deep enough to re-voice)
          { n: h, seed: s1 } = Marbles.nextInt 4 seed
          { n: d, seed: s2 } = Marbles.nextInt 2 s1
          cur = maybe 0 _.transp (odo.heads !! h)
      in { odo: M.setHeadTransp h (cur + (if d == 0 then -mag else mag)) odo, seed: s2 }
    GPattern ->
      let { n: h, seed: s1 } = Marbles.nextInt 4 seed
      in { odo: M.cyclePattern h odo, seed: s1 }
    GSpeed ->
      let mag = 1 + round (amt01 * 3.0)   -- ±1..±4 speed steps
          { n: h, seed: s1 } = Marbles.nextInt 4 seed
          { n: d, seed: s2 } = Marbles.nextInt 2 s1
          cur = maybe 4 _.speedIx (odo.heads !! h)
      in { odo: M.setHeadSpeedIx h (cur + (if d == 0 then -mag else mag)) odo, seed: s2 }
    GKey ->
      -- Depth = adventurousness. Mostly nudge the tonal centre by a fifth; with
      -- probability `amt01` instead jump to a whole new reasonable scale (never
      -- a random note cluster — see Model.setRandScale / Scale.randomisableScales).
      let { u, seed: s1 } = Marbles.nextRand seed
      in if u < amt01
         then let { n: ix, seed: s2 } = Marbles.nextInt M.numRandScales s1
              in { odo: M.setRandScale ix odo, seed: s2 }
         else let { n: dir, seed: s2 } = Marbles.nextInt 2 s1
              in { odo: M.setRoot (odo.rootPc + (if dir == 0 then 7 else 5)) odo, seed: s2 }

-- | Reroll `n` random cells from the Beta distribution, threading the seed.
rerollNotes
  :: Int -> Number -> Number -> M.Odonus -> Marbles.Seed
  -> { odo :: M.Odonus, seed :: Marbles.Seed }
rerollNotes n bias spread odo seed
  | n <= 0 = { odo, seed }
  | otherwise =
      let { n: i, seed: s1 } = Marbles.nextInt 16 seed
          -- cells are indices into the voice's PitchSet now (0 .. span·N−1).
          r = Marbles.rollValue { bias, spread } (range 0 (M.cellIndexMax odo)) s1
      in rerollNotes (n - 1) bias spread (M.setNote i r.value odo) r.seed

-- | Flip `n` random head bits, never landing on all-voices-off: an empty mask
-- | hands the lone voice to a neighbour instead.
flipHeads :: Int -> M.Odonus -> Marbles.Seed -> { odo :: M.Odonus, seed :: Marbles.Seed }
flipHeads n odo seed
  | n <= 0 = { odo, seed }
  | otherwise =
      let { n: h, seed: s1 } = Marbles.nextInt 4 seed
          bit = shl 1 h
          -- Toggle bit h of the 4-bit head mask ARITHMETICALLY, not via
          -- Data.Int.Bits.xor: purerl's optimizer inlines `xor` to Erlang's
          -- BOOLEAN `xor` operator (the bitwise one is `bxor`), so an integer xor
          -- crashes with badarg on the BEAM. A single-bit toggle is exact as +/-
          -- since the bit is either wholly present (clear it) or absent (set it).
          raw = if and (M.headMask odo) bit /= 0 then M.headMask odo - bit else M.headMask odo + bit
          mask = if raw == 0 then shl 1 ((h + 1) `mod` 4) else raw
      in flipHeads (n - 1) (M.setHeadMask mask odo) s1

-- | A sparse on/off mutation. `isRare` marks the state we want to occur seldom
-- | (a rest for GATE, a skip, a glide). Landing on a cell already in the rare
-- | state always restores it; a cell in the common state enters the rare state
-- | only with probability `pEnter` (the source's depth). Equilibrium ≈
-- | pEnter/(1+pEnter) of 16 cells — occasional, not half-and-half.
stepBias
  :: Number -> (M.Cell -> Boolean) -> (Int -> M.Odonus -> M.Odonus)
  -> M.Odonus -> Marbles.Seed -> { odo :: M.Odonus, seed :: Marbles.Seed }
stepBias pEnter isRare toggle odo seed =
  let { n: i, seed: s1 } = Marbles.nextInt 16 seed
      rare = maybe false isRare (odo.cells !! i)
  in if rare then { odo: toggle i odo, seed: s1 }
     else let { u, seed: s2 } = Marbles.nextRand s1
          in if u < pEnter then { odo: toggle i odo, seed: s2 } else { odo, seed: s2 }

-- | One-shot: reroll EVERY cell note from the current Beta distribution (the
-- | "Roll once" button). amount = 1.0 ⇒ all sixteen regenerate.
rollAllNotes
  :: Number -> Number -> M.Odonus -> Marbles.Seed
  -> { odo :: M.Odonus, seed :: Marbles.Seed }
rollAllNotes spread bias odo seed =
  let m = Marbles.mutateInts { spread, bias, amount: 1.0 } (range 0 (M.cellIndexMax odo)) (map _.note odo.cells) seed
  in { odo: M.setNotes m.values odo, seed: m.seed }

-- | Draw four random chord indices from a table of `tableSize` — a fresh
-- | four-chord progression for the KEY·CHORDS quantiser.
rollChords :: Int -> Marbles.Seed -> { picks :: Array Int, seed :: Marbles.Seed }
rollChords tableSize = go 4 []
  where
  go n acc seed
    | n <= 0 = { picks: acc, seed }
    | otherwise =
        let { n: ix, seed: seed' } = Marbles.nextInt tableSize seed
        in go (n - 1) (acc <> [ ix ]) seed'

-- | Seed a plausible melody: 16 random cell indices across the voice's set. In
-- | index space every index is in-harmony by construction (the set IS the
-- | harmony), so the old pitch-class filter is gone — the line reads as musical
-- | because the set does. `pcs` is no longer needed (kept for call-site stability).
seedMelody :: Array Int -> M.Odonus -> Marbles.Seed -> { odo :: M.Odonus, seed :: Marbles.Seed }
seedMelody _ odo seed0 =
  let hi = M.cellIndexMax odo
      pick acc _ =
        let { n: i, seed } = Marbles.nextInt (hi + 1) acc.seed
        in { values: acc.values <> [ i ], seed }
      r = foldl pick { values: [], seed: seed0 } (range 0 15)
  in { odo: M.setNotes r.values odo, seed: r.seed }
