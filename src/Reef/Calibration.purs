-- | Turning an intended pitch into the control voltage that actually produces
-- | it on a particular oscillator.
-- |
-- | An analogue VCO does not track 1 V/octave. Measured on this rig: the four
-- | Saïch voices share one arch but differ 2.5× in its amplitude, peaking
-- | between +10.6 and +26.5 cents; the Instruo Tona droops past 100 cents by
-- | 4.5 kHz. A calibration table records what each oscillator actually did, as
-- | absolute (volts, hz) pairs, and this inverts it.
-- |
-- | **Why this exists in PureScript.** The same arithmetic already lives in
-- | `es9_cv:realise/2` (Erlang, the live rig) and `internal/rig/calib.go` (Go,
-- | DeepStar's verification). Two copies is a cost paid deliberately — DeepStar
-- | reports what the rig WILL do, and cannot do that by computing something
-- | slightly different. A third hand-written copy for the browser would be one
-- | too many, so it goes in reef, which compiles to both JS and Erlang and can
-- | eventually retire the Erlang one rather than join it.
-- |
-- | It therefore mirrors `es9_cv:realise/2` exactly, clamping included. A
-- | browser that rounded differently would place notes where the rig does not.
module Reef.Calibration
  ( Point
  , Table
  , realise
  , realiseNote
  , noteToHz
  , hzToNote
  , cents
  , covers
  , span
  ) where

import Prelude

import Data.Array (head, last, sortBy, uncons)
import Data.Maybe (Maybe(..))
import Reef.Numeric (ln, pow)

-- | One measured step of a sweep: a voltage applied, and the pitch it produced.
type Point = { volts :: Number, hz :: Number }

-- | A VCO's measured response, under the label a realiser looks it up by.
-- | Points need not be sorted; `realise` sorts, as the Erlang does.
type Table = { label :: String, points :: Array Point }

-- | The voltage that produces `targetHz` on this oscillator.
-- |
-- | Interpolation is linear in LOG frequency, because pitch is logarithmic — a
-- | linear-in-Hz reading between two points an octave apart is wrong by tens of
-- | cents, biased toward the upper bracket by more the wider the gap.
-- |
-- | Outside the swept range it CLAMPS rather than extrapolating. A table says
-- | what was measured and nothing more; extrapolating an analogue oscillator's
-- | droop past the last real reading invents a curve. Clamping is audible as a
-- | note that stops rising — a diagnosable symptom — where extrapolation is
-- | quietly wrong.
realise :: Table -> Number -> Number
realise table targetHz =
  let
    pts = sortBy (comparing _.volts) table.points
  in
    case head pts, last pts of
      Just first, Just lastP
        | targetHz <= first.hz -> first.volts
        | targetHz >= lastP.hz -> lastP.volts
        | otherwise -> interp pts targetHz
      _, _ -> 0.0

-- | Walk to the bracketing pair, exactly as `es9_cv:interp/2` does.
interp :: Array Point -> Number -> Number
interp pts hz = case uncons pts of
  Nothing -> 0.0
  Just { head: p0, tail } -> case uncons tail of
    -- One point left and no bracket found: the Erlang returns its voltage.
    Nothing -> p0.volts
    Just { head: p1 } ->
      if p0.hz <= hz && hz <= p1.hz then
        -- Guard the degenerate case a hand-edited table can produce but a
        -- monotonic measured one cannot: two points at the same pitch.
        if p1.hz == p0.hz then p0.volts
        else
          let f = (ln hz - ln p0.hz) / (ln p1.hz - ln p0.hz)
          in p0.volts + f * (p1.volts - p0.volts)
      else interp tail hz

-- | The voltage for an equal-tempered MIDI note. Fractional notes are allowed,
-- | so a bend or a non-12-tone scale needs no separate path.
realiseNote :: Table -> Number -> Number
realiseNote table = realise table <<< noteToHz

-- | Equal-tempered A440. Matches `es9_cv:note_to_hz/1`.
noteToHz :: Number -> Number
noteToHz note = 440.0 * pow 2.0 ((note - 69.0) / 12.0)

hzToNote :: Number -> Number
hzToNote hz = 69.0 + 12.0 * (ln (hz / 440.0) / ln 2.0)

-- | Interval between two frequencies, in cents.
cents :: Number -> Number -> Number
cents from to = 1200.0 * (ln (to / from) / ln 2.0)

-- | Whether a pitch falls inside what was actually swept — i.e. whether
-- | `realise` will interpolate or clamp. A caller that cares about honesty
-- | (Selene deciding whether a jack may carry a pitch voice) should ask,
-- | because a clamped note is not a corrected one.
covers :: Table -> Number -> Boolean
covers table hz =
  let s = span table
  in hz >= s.lo && hz <= s.hi

span :: Table -> { lo :: Number, hi :: Number }
span table =
  let
    pts = sortBy (comparing _.hz) table.points
  in
    case head pts, last pts of
      Just a, Just b -> { lo: a.hz, hi: b.hz }
      _, _ -> { lo: 0.0, hi: 0.0 }
