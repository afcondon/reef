-- | `Reef.Balistes.Engine` — the Grids algorithm core, a faithful port of Emilie
-- | Gillet's `EvaluateDrums()` / `ReadDrumMap()` in the MIT-licensed Grids
-- | firmware. Pure arithmetic on bytes: no state, no floating point.
-- |
-- | This is the SINGLE source of truth, compiled to both the Triggerfish JS
-- | frontend and the purerl-tidal BEAM engine (`reef_balistes_engine@ps`) — it
-- | replaces the two hand-ports (`Triggerfish.Balistes.Engine` +
-- | `balistes_engine.erl`) that could silently disagree on the perturbation RNG.
-- | Cross-runtime byte-identity is proven by `Reef.Conformance.balistesRun`.
-- |
-- | Per 16th-note step, for each of three instruments (BD, SD, HH):
-- |   1. bilinearly interpolate the level at (step, inst) from the four nodes
-- |      surrounding the (X, Y) cursor in the 5x5 style grid (`readDrumMap`);
-- |   2. add a per-pattern random perturbation (scaled by the randomness knob,
-- |      sampled once at step 0 by the model, threaded through xorshift32);
-- |   3. trigger when `level > 255 - density`; accent when `level > 192`.
module Reef.Balistes.Engine
  ( Trigger
  , u8Mix
  , readDrumMap
  , evaluateStep
  , freshPerturbations
  , randByte
  , clampDensity
  ) where

import Prelude

import Data.Array ((!!))
import Data.Int.Bits (and, shl, shr)
import Data.Maybe (fromMaybe)
import Reef.Balistes.Tables (drumMap)
import Reef.Bits (xorshift32)

-- | A fired instrument: which voice (0=BD, 1=SD, 2=HH) and whether it accents.
type Trigger = { inst :: Int, accent :: Boolean }

-- | `u8Mix a b t` — linear interpolation between a and b by t/256. Direct port
-- | of avrlib's `U8Mix`: `a*(255-t) + b*t >> 8`. No rounding offset; 255 (not
-- | 256) is the complement, so t=0 returns a and t=255 returns ~b.
u8Mix :: Int -> Int -> Int -> Int
u8Mix a b t = (a * (255 - t) + b * t) `shr` 8

-- | One byte of a node binary at a flat offset (0 outside the array).
byteAt :: Array Int -> Int -> Int
byteAt bin off = fromMaybe 0 (bin !! off)

-- | `readDrumMap step inst x y` — bilinear lookup. X, Y are 0..255; their top
-- | two bits index the 5x5 node grid, their bottom six bits (shifted up by 2 to
-- | 0..252) are the fractional weights. Returns the interpolated byte 0..255.
readDrumMap :: Int -> Int -> Int -> Int -> Int
readDrumMap step inst x y =
  let
    i = x `shr` 6
    j = y `shr` 6
    aMap = drumMap i j
    bMap = drumMap (i + 1) j
    cMap = drumMap i (j + 1)
    dMap = drumMap (i + 1) (j + 1)
    offset = inst * 32 + step
    a = byteAt aMap offset
    b = byteAt bMap offset
    c = byteAt cMap offset
    d = byteAt dMap offset
    xFrac = (x `and` 0x3F) `shl` 2
    yFrac = (y `and` 0x3F) `shl` 2
  in
    u8Mix (u8Mix a b xFrac) (u8Mix c d xFrac) yFrac

clampDensity :: Int -> Int
clampDensity d
  | d < 0 = 0
  | d > 255 = 255
  | otherwise = d

-- | One instrument's evaluation at a step. The clipping rule (raw+pert > 255 →
-- | 255) matches the firmware's "weird clipping rule" comment.
evaluateOne :: Int -> Int -> Int -> Int -> Int -> Int -> Maybe' Trigger
evaluateOne step inst x y density perturbation =
  let
    raw = readDrumMap step inst x y
    level = if raw + perturbation > 255 then 255 else raw + perturbation
    threshold = 255 - clampDensity density
  in
    if level > threshold then Has { inst, accent: level > 192 } else None

-- | A tiny local Maybe so we can filter without pulling in extra imports — and
-- | to keep the firmware's "silent" sentinel explicit.
data Maybe' a = Has a | None

-- | Evaluate one step. `densities`/`perturbations` are [bd, sd, hh] (0..255).
-- | Perturbations are sampled at step 0 by the model. Returns only the
-- | instruments whose level cleared the density threshold.
evaluateStep :: Int -> Int -> Int -> Array Int -> Array Int -> Array Trigger
evaluateStep step x y densities perturbations =
  let
    dens k = fromMaybe 128 (densities !! k)
    pert k = fromMaybe 0 (perturbations !! k)
    one k = evaluateOne step k x y (dens k) (pert k)
    collect k acc = case one k of
      Has t -> [ t ] <> acc
      None -> acc
  in
    collect 0 (collect 1 (collect 2 []))

-- | `freshPerturbations randomness rng` — sampled once at pattern start. The
-- | firmware does `pert[i] = (GetByte() * (randomness >> 2)) >> 8`. We thread an
-- | explicit xorshift state so the engine stays pure and reproducible per voice.
-- | Returns the three perturbations and the next RNG state.
freshPerturbations :: Int -> Int -> { perts :: Array Int, rng :: Int }
freshPerturbations randomness rng0 =
  let
    scale = randomness `shr` 2
    r1 = randByte rng0
    r2 = randByte r1.rng
    r3 = randByte r2.rng
    p b = (b * scale) `shr` 8
  in
    { perts: [ p r1.byte, p r2.byte, p r3.byte ], rng: r3.rng }

-- | xorshift32 — deterministic per seed, fast, keeps the state a small Int.
-- | Returns a byte 0..255 plus the next state. The step itself lives in
-- | `Reef.Bits.xorshift32` (FFI) so JS's native 32-bit bit-ops and the BEAM's
-- | explicitly-masked unsigned-32 arithmetic produce the same bits — `Data.Int.Bits`
-- | can't: purerl miscompiles `xor`, and `shl` doesn't wrap at 32 bits on the BEAM.
-- | `and 0xFF` extracts the low byte regardless of sign.
randByte :: Int -> { byte :: Int, rng :: Int }
randByte s0 =
  let s = xorshift32 s0
  in { byte: s `and` 0xFF, rng: s }
