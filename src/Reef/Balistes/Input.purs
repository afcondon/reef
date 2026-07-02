-- | `Reef.Balistes.Input` — the Balistes live-control surface as a SERIALISABLE
-- | input, for lockstep knob/gesture sync (the Balistes analogue of `Reef.Input`).
-- |
-- | Every synced gesture (X/Y pad, density/randomness/open/push knobs, per-lane
-- | notes, ratchet drags, dice/reset) becomes a `BInput`. `applyBInput` is the pure
-- | interpreter, row-polymorphic so it runs on BOTH the frontend's rich `Balistes`
-- | record AND the BEAM's `BalSim` — the SAME code applies the edit on both runtimes,
-- | so a tick-tagged gesture lands on the same model step and the two stay
-- | byte-identical (deferred-on-both, RTS-netcode style). A flat `WireBInput` record
-- | gives a free simple-json codec (no hand-written sum decoder to drift), exactly as
-- | `Reef.Input.WireInput` does for Odonus.
module Reef.Balistes.Input
  ( BInput(..)
  , BTagged
  , WireBInput
  , applyBInput
  , toWire
  , fromWire
  ) where

import Prelude

import Data.Array (updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Reef.Balistes.Engine (freshPerturbations)

-- | The closed set of Balistes gestures that mutate shared engine/render state.
-- | Instruments are 0=BD/1=SD/2=HH; push lanes add 3=OH; ratchet is (inst,step).
data BInput
  = BSetX Int
  | BSetY Int
  | BSetDensity Int Int      -- inst, 0..255
  | BSetRandomness Int       -- 0..255
  | BSetOpen Int             -- 0..255
  | BSetPush Int Int         -- lane (0..3), −50..50 ms
  | BSetNote Int Int         -- lane (0..3), 0..127
  | BSetRatchet Int Int Int  -- inst, step, 1..8
  | BReseed                  -- re-roll the perturbation seed (DICE)
  | BReset                   -- jump to step 0 + resample (RESET)

derive instance eqBInput :: Eq BInput

-- | The tick-tagged wire unit: apply this input when model step `tick` arrives.
type BTagged = { tick :: Int, input :: BInput }

-- | The fields any `BInput` may touch. Open row so the frontend `Balistes` record
-- | (snapshots/sequence added) and the BEAM `BalSim` both satisfy it.
type BStateRow r =
  ( x :: Int
  , y :: Int
  , densBd :: Int
  , densSd :: Int
  , densHh :: Int
  , randomness :: Int
  , open :: Int
  , push :: Array Int
  , notes :: Array Int
  , ratchet :: Array Int
  , step :: Int
  , perts :: Array Int
  , rng :: Int
  | r
  )

clampI :: Int -> Int -> Int -> Int
clampI lo hi v = if v < lo then lo else if v > hi then hi else v

-- | The pure interpreter — identical field updates to the frontend `Model` setters,
-- | so a synced gesture reproduces the exact edit on the rig. Absolute + idempotent
-- | for the setters (replaying the settled value lands where the drag settled);
-- | BReseed/BReset thread the RNG deterministically so both runtimes, applying at the
-- | SAME tick from the SAME lockstep state, stay bit-identical.
applyBInput :: forall r. BInput -> Record (BStateRow r) -> Record (BStateRow r)
applyBInput i b = case i of
  BSetX v -> b { x = clampI 0 255 v }
  BSetY v -> b { y = clampI 0 255 v }
  BSetDensity inst v -> case inst of
    0 -> b { densBd = clampI 0 255 v }
    1 -> b { densSd = clampI 0 255 v }
    _ -> b { densHh = clampI 0 255 v }
  BSetRandomness v -> b { randomness = clampI 0 255 v }
  BSetOpen v -> b { open = clampI 0 255 v }
  BSetPush lane v -> b { push = fromMaybe b.push (updateAt lane (clampI (-50) 50 v) b.push) }
  BSetNote lane v -> b { notes = fromMaybe b.notes (updateAt lane (clampI 0 127 v) b.notes) }
  BSetRatchet inst step v ->
    b { ratchet = fromMaybe b.ratchet (updateAt (inst * 32 + step) (clampI 1 8 v) b.ratchet) }
  BReseed ->
    let s = freshPerturbations b.randomness (b.rng + 0x6D2B79F5)
    in b { perts = s.perts, rng = s.rng }
  BReset ->
    let s = freshPerturbations b.randomness b.rng
    in b { step = 0, perts = s.perts, rng = s.rng }

-- | The flat wire form: a tag string + up to three Int slots. simple-json's record
-- | instance encodes/decodes it directly — no bespoke sum codec. Unused slots are 0.
type WireBInput = { tag :: String, a :: Int, b :: Int, c :: Int }

w0 :: WireBInput
w0 = { tag: "", a: 0, b: 0, c: 0 }

toWire :: BInput -> WireBInput
toWire = case _ of
  BSetX v -> w0 { tag = "SetX", a = v }
  BSetY v -> w0 { tag = "SetY", a = v }
  BSetDensity inst v -> w0 { tag = "SetDensity", a = inst, b = v }
  BSetRandomness v -> w0 { tag = "SetRandomness", a = v }
  BSetOpen v -> w0 { tag = "SetOpen", a = v }
  BSetPush lane v -> w0 { tag = "SetPush", a = lane, b = v }
  BSetNote lane v -> w0 { tag = "SetNote", a = lane, b = v }
  BSetRatchet inst step v -> w0 { tag = "SetRatchet", a = inst, b = step, c = v }
  BReseed -> w0 { tag = "Reseed" }
  BReset -> w0 { tag = "Reset" }

fromWire :: WireBInput -> Maybe BInput
fromWire w = case w.tag of
  "SetX" -> Just (BSetX w.a)
  "SetY" -> Just (BSetY w.a)
  "SetDensity" -> Just (BSetDensity w.a w.b)
  "SetRandomness" -> Just (BSetRandomness w.a)
  "SetOpen" -> Just (BSetOpen w.a)
  "SetPush" -> Just (BSetPush w.a w.b)
  "SetNote" -> Just (BSetNote w.a w.b)
  "SetRatchet" -> Just (BSetRatchet w.a w.b w.c)
  "Reseed" -> Just BReseed
  "Reset" -> Just BReset
  _ -> Nothing
