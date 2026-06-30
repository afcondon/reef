-- | reef-owned numeric primitives, supplied by FFI so the same source compiles
-- | under both package-set eras reef must target:
-- |
-- |   • standalone JS build — registry set 73.3.0 (modern): `pow` lives in
-- |     `Data.Number`, and there is no `Math` package;
-- |   • via purerl-tidal — the purerl set, pinned to purescript-numbers v8 (2021,
-- |     pre the Math→Data.Number consolidation): `pow` lives in the `Math` module,
-- |     and `Data.Number` has no `pow`.
-- |
-- | So neither `import Data.Number (pow)` nor `import Math (pow)` resolves under
-- | BOTH. Rather than couple reef to one era, we own the primitive and delegate to
-- | each runtime's native one (`Math.pow` on JS, `math:pow` on Erlang). This also
-- | co-locates the engine's sole transcendental — see `Reef.Conformance.betaProbe`,
-- | which probes whether the two native `pow`s agree closely enough for the
-- | generative path to stay bit-identical across runtimes.
module Reef.Numeric (pow) where

-- | The first argument raised to the power of the second. Delegates to the
-- | runtime's native primitive; IEEE 754 does not mandate a correctly-rounded
-- | `pow`, so cross-runtime agreement is empirical (betaProbe), not guaranteed.
foreign import pow :: Number -> Number -> Number
