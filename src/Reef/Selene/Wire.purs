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
  , placeOn
  , familyOf
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Simple.JSON (writeJSON)
import Reef.Selene.Model as M
import Reef.Selene.Rack as Rack

-- | One destination, ready to push: the daemon `socket` id (es9 | fh2), the target
-- | `bank` token (main / cv0 / gt0 / …), and the `apply-polysignal` envelope JSON.
-- | socket + bank travel in the verb so the BEAM echoes them in `selene-reply`,
-- | letting the browser show OK/claim/ERR against the exact destination.
-- | `bank` is the destination's PLACE: the daemon's bank token, and its run of
-- | jacks when it holds part of the bank (`main:5-8`). The rig keys what it
-- | keeps by it and echoes it back, so two halves of one bank are two things;
-- | the daemon reads its own bank and the run from the json.
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
  M.GLfoT _ -> "polylfot"
  M.GClock _ -> "polyclock"
  M.GEuclid _ -> "polyeuclid"
  M.GNote _ -> "polypresetnote"
  M.GEnv _ -> "polyenv"
  M.GVoice _ -> "polyvoice"
  -- the rig plays a throw itself, as a CC; no daemon has one
  M.GThrow _ -> "throw"

-- | The name a bank's claim is held under in its daemon: one per (socket,
-- | bank), whatever is on it, so a re-push replaces the claim and an `off`
-- | releases it by the same name.
ownerOf :: { socket :: String, bank :: String } -> M.Jacks -> String
ownerOf sb jacks = "selene:" <> sb.socket <> ":" <> placeOn sb jacks

-- | A daemon bank and a run as one token, `main` or `main:5-8`: what the rig
-- | keys a destination by.
placeOn :: { socket :: String, bank :: String } -> M.Jacks -> String
placeOn sb jacks = sb.bank <> M.jacksToken jacks

-- | Encode one destination into a `SeleneApply`, or Nothing when its target isn't
-- | a modular bank. The alias is stable per (socket, bank, run) so a re-push
-- | replaces the same claim cleanly. A run that is not the whole bank carries
-- | `offset`, its first jack (0-based), and one slot per jack of the run.
-- |
-- | A muted destination sends its silent version (`Rack.silenced`): the jacks
-- | go to 0 V and the bank itself is untouched, so unmuting sends it again.
-- | Its muted slots (`mutedSlots`) go out silent in the same way.
destinationEnvelope :: M.Destination -> Maybe SeleneApply
destinationEnvelope d0 = do
  let d = if d0.muted then Rack.silenced d0 else Rack.slotsSilenced d0
  sb <- targetSocketBank d.target
  let
    bank = sb.bank
    family = familyOf d.bank
    -- `Nothing` must OMIT the key, not send null: the daemon reads it with
    -- `optStringField`, which tests for presence and would reject a null. A
    -- `Maybe` field writes `undefined`, and JSON.stringify drops those keys —
    -- verified against the actual emitted JSON, not assumed.
    range = map M.rangeToWire d.range
    alias = ownerOf sb d.jacks
    -- the run's first jack, 0-based in the bank; omitted for a whole bank,
    -- so a whole bank's envelope is what it always was
    offset = if M.isWholeBank d.jacks then Nothing else Just (d.jacks.first - 1)
    json = case d.bank of
      -- a voice layout: its gates on their own bank (the same daemon's), its
      -- outputs by name, and a channel and an envelope a voice. With nowhere
      -- for its gates it has nothing to send.
      M.GVoice vb -> do
        g <- vb.gates
        gsb <- targetSocketBank g.target
        if gsb.socket /= sb.socket then Nothing else Just $
          writeJSON
            { bank, family, alias, offset
            , gates: { bank: gsb.bank, offset: g.jacks.first - 1 }
            , outputs: map outputName (Array.filter (\o -> Array.elem o vb.outputs) M.voiceOuts)
            , slots: map voiceWire vb.voices
            }
      -- `wave` is the editor's memory of a flat slot's shape; the daemon has
      -- only the mix.
      M.GLfo slots ->
        Just $ writeJSON { bank, family, alias, offset, outputRange: range, slots: map lfoWire slots }
      -- the period in ticks of 24 a beat, where a POLYLFO sends Hz
      M.GLfoT slots ->
        Just $ writeJSON { bank, family, alias, offset, outputRange: range, slots: map lfoTWire slots }
      M.GEuclid slots ->
        Just $ writeJSON { bank, family, alias, offset, outputRange: range, slots }
      M.GNote slots ->
        Just $ writeJSON { bank, family, alias, offset, outputRange: range, slots }
      -- Flat 0..127 bytes, field names already matching the daemon's record.
      M.GEnv slots ->
        Just $ writeJSON { bank, family, alias, offset, outputRange: range, slots }
      -- clock `base` is an ADT; the daemon wants the wire token string.
      M.GClock slots ->
        Just $ writeJSON { bank, family, alias, offset, outputRange: range, slots: map clockWire slots }
      M.GThrow _ -> Nothing
  json' <- json
  pure { socket: sb.socket, bank: placeOn sb d.jacks, json: json' }
  where
  outputName = case _ of
    M.VVelocity -> "velocity"
    M.VEnvelope -> "envelope"
    M.VPressure -> "pressure"
  voiceWire v =
    { channel: v.channel
    , attack: v.env.attack, decay: v.env.decay, sustain: v.env.sustain, release: v.env.release
    , depth: v.env.depth, velDepth: v.env.velDepth, timeRange: v.env.timeRange
    , randomDepth: v.env.randomDepth
    , attackShape: v.env.attackShape, decayShape: v.env.decayShape, releaseShape: v.env.releaseShape
    }
  lfoWire s =
    { rate: s.rate, phase: s.phase, level: s.level
    , sin: s.sin, sqr: s.sqr, tri: s.tri, saw: s.saw, rnd: s.rnd, nse: s.nse
    }
  lfoTWire s =
    { ticks: M.lfoTicks s, phase: s.phase, level: s.level
    , sin: s.sin, sqr: s.sqr, tri: s.tri, saw: s.saw, rnd: s.rnd, nse: s.nse
    }
  clockWire s =
    { base: M.clockBaseToWire s.base
    , multiplier: s.multiplier
    , pulseWidth: s.pulseWidth
    , phase: s.phase
    }
