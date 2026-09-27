-- | **Where a hit goes**: the routing table as both runtimes read it.
-- |
-- | Triggerfish's routing table (`Triggerfish.Routing.Model`) says where each
-- | source fans out to: a set of legs, each a destination with a trim. Until
-- | now only the browser honoured it; the rig played every drum hit on the
-- | FH-2, channel 10, whatever the table said. So Solo and Atlantis sounded
-- | different, and nothing said so.
-- |
-- | This module is the part of the table the rig needs, and the one function
-- | that turns a hit into what is sent. The browser resolves its table into a
-- | `DrumRouting` (it knows which ports exist) and pushes it; the rig keeps it;
-- | both call `drumSends` for every hit. `Reef.Conformance.routingRun` holds the
-- | two runtimes to the same answer.
-- |
-- | **Every destination here is a MIDI message on a named port**, which is
-- | what the browser's `Wire` found too: an FH-2 gate is a note on the FH-2
-- | whose pitch selects the jack, an FH-2 envelope is a note on channel =
-- | envelope. The ES-9 kinds are not here: the browser cannot reach them
-- | either, so leaving them out keeps the two runtimes playing the same thing.
module Reef.Routing
  ( RampleLeg
  , Leg
  , DrumRouting
  , Hit
  , Send(..)
  , legSends
  , drumSends
  , encodeDrumRouting
  , decodeDrumRouting
  ) where

import Prelude

import Data.Array (concatMap, filter, findIndex, head, null, (!!))
import Data.Either (Either)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign (MultipleErrors)
import Reef.Rample as Rample
import Simple.JSON (readJSON, writeJSON)

-- | A Rample voice playing a sliced card: the pitch is which slice, sent as
-- | the voice's start-point control `settleMs` ahead of the trigger note.
type RampleLeg =
  { voice :: Int
  , slots :: Int
  , pitchOfSlot0 :: Int
  , settleMs :: Int
  }

-- | One live leg, resolved.
-- |
-- | - `port`: the whole port name. The rig matches names exactly, so the
-- |   browser, which matches the table's needles by substring, sends the name
-- |   it would itself have sent to.
-- | - `channel`: 1..16.
-- | - `note`: what the leg sends, or below 0 for the hit's own note. An FH-2
-- |   gate sends its selector; a Rample, its trigger.
-- | - `offsetMs`: the per-leg trim, so a doubled kick does not flam.
-- | - `rample`: at most one. An array, not a `Maybe`, for the reason
-- |   `Reef.Conspicillum.Protocol` gives: null round-trips through V8 and jsx
-- |   in ways that agree until one day they do not.
type Leg =
  { port :: String
  , channel :: Int
  , note :: Int
  , offsetMs :: Number
  , rample :: Array RampleLeg
  }

-- | The drum lanes, in kit order: the note that names each lane, and the lane's
-- | live legs (muted ones are left out by the browser before it pushes).
type DrumRouting =
  { notes :: Array Int
  , lanes :: Array (Array Leg)
  }

-- | One drum hit, as the engines render it. `atMs` is relative: the browser
-- | adds it to now, the rig to the step's wall time.
type Hit =
  { note :: Int
  , velocity :: Int
  , atMs :: Number
  , durMs :: Number
  }

data Send
  = Note { port :: String, channel :: Int, note :: Int, velocity :: Int, atMs :: Number, durMs :: Number }
  | Control { port :: String, channel :: Int, controller :: Int, value :: Int, atMs :: Number }

derive instance eqSend :: Eq Send

-- | What one hit sends down one leg. A Rample that does not hold the pitch
-- | sends nothing: a silently transposed note is harder to notice than a
-- | missing one.
legSends :: Leg -> Hit -> Array Send
legSends leg hit = case head leg.rample of
  Nothing ->
    [ Note { port: leg.port, channel: leg.channel, note, velocity: hit.velocity, atMs, durMs: hit.durMs } ]
  Just r ->
    case Rample.slotFor { velocity: Nothing, slots: r.slots, pitchOfSlot0: Just r.pitchOfSlot0, slotPitches: Nothing } hit.note of
      Nothing -> []
      Just slot ->
        [ Control
            { port: leg.port, channel: leg.channel, controller: Rample.startCC r.voice
            , value: Rample.ccForSlot slot r.slots, atMs: atMs - toNumber r.settleMs
            }
        , Note { port: leg.port, channel: leg.channel, note, velocity: hit.velocity, atMs, durMs: hit.durMs }
        ]
  where
  atMs = hit.atMs + leg.offsetMs
  note = if leg.note < 0 then hit.note else leg.note

-- | What one drum hit sends, down every leg of its lane.
-- |
-- | A note outside the kit (the Tidal rack can name any) has no lane. It
-- | borrows the first lane's legs, but only those that carry the hit's own
-- | note: a leg that sends a note of its own, a gate selector or a Rample
-- | trigger, belongs to that lane's drum, and would sound the kick.
drumSends :: DrumRouting -> Hit -> Array Send
drumSends routing hit = case findIndex (_ == hit.note) routing.notes of
  Just lane -> concatMap (\leg -> legSends leg hit) (legsOf lane)
  Nothing -> concatMap (\leg -> legSends leg hit) (filter carriesOwnNote (legsOf 0))
  where
  legsOf lane = fromMaybe [] (routing.lanes !! lane)
  carriesOwnNote leg = leg.note < 0 && null leg.rample

encodeDrumRouting :: DrumRouting -> String
encodeDrumRouting = writeJSON

decodeDrumRouting :: String -> Either MultipleErrors DrumRouting
decodeDrumRouting = readJSON
