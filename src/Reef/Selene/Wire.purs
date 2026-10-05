-- | Triggerfish.Selene.Wire — the browser-side encoder for the Selene → modular
-- | push (#142). Turns each rack **destination** into the `apply-polysignal`
-- | envelope JSON the es9-daemon / fh2-config daemon read, plus the (socket, bank)
-- | the BEAM relay (`selene-apply <socket> <bank> <json>`) needs to route it.
-- |
-- | Pure, no I/O — the component (S3) maps this over `sel.destinations` and pushes
-- | the results over the rig WebSocket. The slot field names already match the
-- | daemons' structs verbatim (that was the whole point of `Selene.Model`), so the
-- | encoder is a thin `writeJSON` over records with the wire tokens filled in.
module Reef.Selene.Wire
  ( SeleneApply
  , destinationEnvelope
  , targetSocketBank
  , familyOf
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Simple.JSON (writeJSON)
import Reef.Selene.Model as M

-- | One destination, ready to push: the daemon `socket` id (es9 | fh2), the target
-- | `bank` token (main / cv0 / gt0 / …), and the `apply-polysignal` envelope JSON.
-- | socket + bank travel in the verb so the BEAM echoes them in `selene-reply`,
-- | letting the browser show OK/claim/ERR against the exact destination.
type SeleneApply = { socket :: String, bank :: String, json :: String }

-- | Target → (daemon socket, bank token), or Nothing when the target isn't a
-- | modular CV/gate bank (Midi / Virtual are out of scope for #142).
-- |
-- | ES-9 targets map cleanly (the model distinguishes gate/cv expander blocks).
-- | FH-2's `Target` is a bare index with no gate/cv distinction, so it maps to the
-- | FH-2 `main` bank for now; per-expander FH-2 addressing (cv0-6 / gt0-7) needs
-- | the `Target` model to grow FH-2 gate/cv constructors — deferred (FH-2 isn't in
-- | the default rack). The es9-daemon supports main / cv0 / gt0 today; other bank
-- | indices surface as an ERR in the reply, which is the intended feedback.
targetSocketBank :: M.Target -> Maybe { socket :: String, bank :: String }
targetSocketBank = case _ of
  M.ES9Main -> Just { socket: "es9", bank: "main" }
  M.ES9Gt n -> Just { socket: "es9", bank: "gt" <> show n }
  M.ES9Cv n -> Just { socket: "es9", bank: "cv" <> show n }
  M.FH2 _ -> Just { socket: "fh2", bank: "main" }
  M.Midi _ -> Nothing
  M.Virtual _ -> Nothing

-- | The `family` wire token for a bank's generator kind.
familyOf :: M.GenBank -> String
familyOf = case _ of
  M.GLfo _ -> "polylfo"
  M.GClock _ -> "polyclock"
  M.GEuclid _ -> "polyeuclid"
  M.GNote _ -> "polypresetnote"
  M.GEnv _ -> "polyenv"

-- | Encode one destination into a `SeleneApply`, or Nothing when its target isn't
-- | a modular bank. The alias is stable per (socket, bank) so a re-push replaces
-- | the same claim cleanly.
destinationEnvelope :: M.Destination -> Maybe SeleneApply
destinationEnvelope d = do
  sb <- targetSocketBank d.target
  let
    bank = sb.bank
    family = familyOf d.bank
    -- `Nothing` must OMIT the key, not send null: the daemon reads it with
    -- `optStringField`, which tests for presence and would reject a null. A
    -- `Maybe` field writes `undefined`, and JSON.stringify drops those keys —
    -- verified against the actual emitted JSON, not assumed.
    range = map M.rangeToWire d.range
    alias = "selene:" <> sb.socket <> ":" <> bank
    json = case d.bank of
      M.GLfo slots ->
        writeJSON { bank, family, alias, outputRange: range, slots }
      M.GEuclid slots ->
        writeJSON { bank, family, alias, outputRange: range, slots }
      M.GNote slots ->
        writeJSON { bank, family, alias, outputRange: range, slots }
      -- Flat 0..127 bytes, field names already matching the daemon's record.
      M.GEnv slots ->
        writeJSON { bank, family, alias, outputRange: range, slots }
      -- clock `base` is an ADT; the daemon wants the wire token string.
      M.GClock slots ->
        writeJSON { bank, family, alias, outputRange: range, slots: map clockWire slots }
  pure { socket: sb.socket, bank, json }
  where
  clockWire s =
    { base: M.clockBaseToWire s.base
    , multiplier: s.multiplier
    , pulseWidth: s.pulseWidth
    , phase: s.phase
    }
