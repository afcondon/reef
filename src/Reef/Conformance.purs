-- | `Reef.Conformance` — the pure cross-runtime oracle for the Odonus engine.
-- |
-- | `run` steps a canonical Odonus (the default, scale-quantised, chord overlay
-- | off) a fixed number of times and renders every fired event to a
-- | deterministic string. Because it is pure and I/O-free, the SAME function
-- | runs under the JS backend and under purerl (Erlang) — diffing the two
-- | outputs proves `stepEmit` behaves identically on both runtimes, which is
-- | exactly what makes wiring `odonus_voice` onto `reef_odonus@ps:stepEmit`
-- | safe. Frozen as a golden in the test suite so any future drift is caught.
module Reef.Conformance (run, steps) where

import Prelude

import Data.Array (range, snoc)
import Data.Foldable (foldl, intercalate)
import Reef.Odonus (Fired, Odonus, defaultOdonus, stepEmit)

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

pad3 :: Int -> String
pad3 n =
  let s = show n
  in if n < 10 then "  " <> s else if n < 100 then " " <> s else s
