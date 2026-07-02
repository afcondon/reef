-- | reef-owned bit primitive, supplied by FFI for the same reason as
-- | `Reef.Numeric.pow`: `Data.Int.Bits.xor` is unusable across both runtimes reef
-- | targets. purerl's optimizer inlines PureScript `xor` to Erlang's BOOLEAN `xor`
-- | operator (the bitwise one is `bxor`), so an integer `xor` crashes with badarg on
-- | the BEAM (see the arithmetic workaround in `Reef.Gen`). A full xorshift32 needs
-- | real integer xor AND 32-bit wraparound, so we own the whole step and delegate to
-- | each runtime's native semantics:
-- |
-- |   • JS  — native 32-bit signed bit-ops (`<<`, `>>>`, `^`, `| 0`);
-- |   • BEAM — arbitrary-precision integers kept explicitly masked to unsigned 32
-- |     (`band 16#FFFFFFFF`), so `bxor` / `bsr` / masked `bsl` reproduce the JS
-- |     low-32-bit result bit-for-bit.
-- |
-- | This makes the Balistes perturbation RNG (the one transcendental-free source of
-- | cross-runtime divergence risk in `Reef.Balistes.Engine`) byte-identical on both
-- | runtimes — proven, not assumed, by `Reef.Conformance.balistesRun`.
module Reef.Bits (xorshift32) where

-- | One xorshift32 step: `s ^= s << 13; s ^= s >>> 17; s ^= s << 5`. Returns the
-- | next 32-bit state; callers take the low byte via `and 0xFF` (sign-agnostic, so
-- | JS's signed and the BEAM's unsigned representation of the same bits agree).
foreign import xorshift32 :: Int -> Int
