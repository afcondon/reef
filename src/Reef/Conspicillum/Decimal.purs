-- | **Numbers as text, identically on every runtime.**
-- |
-- | `show` on a Number is the runtime's own: JavaScript prints `0.62` and the
-- | BEAM prints `6.19999999999999995559e-01`. Anything a person reads, or a
-- | golden file compares, is built here from integers instead. The integer
-- | and fractional parts are taken apart first, so a cutoff of 6850 Hz does not
-- | overflow an Int on its way to six places.
-- |
-- | Lifted out of `Reef.Conspicillum.Notation`, where it began, so the parameter
-- | catalogue prints numbers the same way the line does.
module Reef.Conspicillum.Decimal
  ( fixed
  , trimmed
  ) where

import Prelude

import Data.Int (floor, round, toNumber)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU

-- | Exactly `places` decimal places: `fixed 2 0.8 == "0.80"`,
-- | `fixed 0 6850.0 == "6850"`.
fixed :: Int -> Number -> String
fixed places x =
  let { sign, whole, fraction } = split places x
  in sign <> show whole <> (if places <= 0 then "" else "." <> fraction)

-- | At most `places` decimal places, with trailing zeros dropped:
-- | `trimmed 6 1.0 == "1"`, `trimmed 6 0.62 == "0.62"`.
trimmed :: Int -> Number -> String
trimmed places x =
  let
    { sign, whole, fraction } = split places x
    rest = dropZeros fraction
  in sign <> show whole <> (if rest == "" then "" else "." <> rest)

split :: Int -> Number -> { sign :: String, whole :: Int, fraction :: String }
split places x =
  let
    scale = powerOfTen places
    size = if x < 0.0 then -x else x
    whole0 = if places <= 0 then round size else floor size
    fraction0 = if places <= 0 then 0 else round ((size - toNumber whole0) * toNumber scale)
    carried = places > 0 && fraction0 >= scale
    whole = if carried then whole0 + 1 else whole0
    fraction = if carried then 0 else fraction0
    -- Rounding a small negative number to zero drops its sign: never "-0".
    sign = if x < 0.0 && (whole /= 0 || fraction /= 0) then "-" else ""
    -- scale + fraction, less its leading 1, is the fraction zero-padded.
    digits = if places <= 0 then "" else SCU.drop 1 (show (scale + fraction))
  in { sign, whole, fraction: digits }

-- | Not `foldl` over `range 1 n`: `range 1 0` is `[1, 0]`, which counts down.
powerOfTen :: Int -> Int
powerOfTen n = if n <= 0 then 1 else 10 * powerOfTen (n - 1)

dropZeros :: String -> String
dropZeros t = case SCU.charAt (SCU.length t - 1) t of
  Just '0' -> dropZeros (SCU.take (SCU.length t - 1) t)
  _ -> t
