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
  , ownerOf
  , familyOf
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Simple.JSON (writeJSON)
import Reef.Selene.Model as M
import Reef.Selene.Rack as Rack

-- | One destination, ready to push: the daemon `socket` id (es9 | fh2), the target
-- | `bank` token (main / cv0 / gt0 / …), and the `apply-polysignal` envelope JSON.
-- | socket + bank travel in the verb so the BEAM echoes them in `selene-reply`,
-- | letting the browser show OK/claim/ERR against the exact destination.
type SeleneApply = { socket :: String, bank :: String, json :: String }

-- | Target → (daemon socket, bank token), or Nothing when the target isn't a
-- | modular CV/gate bank (Midi / Virtual are out of scope for #142).
-- |
-- | Both devices name their banks the same way: `main`, then `gtN` for gate
-- | expanders and `cvN` for CV expanders, N counted from 0. The FH-2's `FH2 n`
-- | is its own eight (0) then each FHX-8GT; `FH2Cv n` is FHX-8CV expansion
-- | n + 1. A bank the daemon does not have surfaces as an ERR in the reply,
-- | which is the intended feedback.
targetSocketBank :: M.Target -> Maybe { socket :: String, bank :: String }
targetSocketBank = case _ of
  M.ES9Main -> Just { socket: "es9", bank: "main" }
  M.ES9Gt n -> Just { socket: "es9", bank: "gt" <> show n }
  M.ES9Cv n -> Just { socket: "es9", bank: "cv" <> show n }
  -- the FH-2's own eight, then each FHX-8 expander, which in AC's rack is an
  -- FHX-8GT, gates (Triggerfish.Rig's assumption, spelt out there)
  M.FH2 0 -> Just { socket: "fh2", bank: "main" }
  M.FH2 n -> Just { socket: "fh2", bank: "gt" <> show (n - 1) }
  -- an FHX-8CV, by its expansion index: the FH-2 daemon's cv0..cv6
  M.FH2Cv n -> Just { socket: "fh2", bank: "cv" <> show n }
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

-- | The name a bank's claim is held under in its daemon: one per (socket,
-- | bank), whatever is on it, so a re-push replaces the claim and an `off`
-- | releases it by the same name.
ownerOf :: { socket :: String, bank :: String } -> String
ownerOf sb = "selene:" <> sb.socket <> ":" <> sb.bank

-- | Encode one destination into a `SeleneApply`, or Nothing when its target isn't
-- | a modular bank. The alias is stable per (socket, bank) so a re-push replaces
-- | the same claim cleanly.
-- |
-- | A muted destination sends its silent version (`Rack.silenced`): the jacks
-- | go to 0 V and the bank itself is untouched, so unmuting sends it again.
destinationEnvelope :: M.Destination -> Maybe SeleneApply
destinationEnvelope d0 = do
  let d = if d0.muted then Rack.silenced d0 else d0
  sb <- targetSocketBank d.target
  let
    bank = sb.bank
    family = familyOf d.bank
    -- `Nothing` must OMIT the key, not send null: the daemon reads it with
    -- `optStringField`, which tests for presence and would reject a null. A
    -- `Maybe` field writes `undefined`, and JSON.stringify drops those keys —
    -- verified against the actual emitted JSON, not assumed.
    range = map M.rangeToWire d.range
    alias = ownerOf sb
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
