-- | `Reef.Odonus.Voices` — VOICES, the strategy behind SPREAD
-- | (docs/kb/plans/odonus-voices.md, first cut 2026-10-15).
-- |
-- | A strategy is one Harmonia voicing and a rule per head:
-- |
-- |   * `Melody` — the head's own cell, SPREAD, INT and the snap, as without
-- |     a strategy.
-- |   * `Voice k` — the k-th note, low to high, of the voicing of the sounding
-- |     chord (past its size, an octave up). The head's cells still give its
-- |     rhythm, gate, length and velocity.
-- |   * `Lead n` — head I's note moved n steps of the sounding set: chord tones
-- |     with a chord, scale steps without.
-- |
-- | Kept on the Odonus as plain Ints (`Voices`), so the state crosses the wire
-- | with simple-json on both runtimes; the ADTs here are its readings. The
-- | text (`drop2 melody voice 1 voice 3 lead -2`) is the one syntax for the
-- | patch, the input and the page.
module Reef.Odonus.Voices
  ( Voices
  , RuleWire
  , Rule(..)
  , VoicingKind(..)
  , voicingKinds
  , voicingName
  , voicingOf
  , voicingIx
  , wantsSeventh
  , strategy
  , ruleOf
  , ruleWire
  , rulesOf
  , parseVoices
  , printVoices
  ) where

import Prelude

import Data.Array (findIndex, length, take, uncons, (!!))
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, split, trim)
import Data.Array (filter) as A
import Harmonia.Voicing (Voicing, cluster, drop2, drop2and4, openTriad, quartal)

type RuleWire = { kind :: Int, n :: Int }

-- | The strategy as kept: an index into `voicingKinds`, and up to four rules
-- | (a head with none is `Melody`).
type Voices = { voicing :: Int, rules :: Array RuleWire }

data Rule = Melody | Voice Int | Lead Int

derive instance eqRule :: Eq Rule

data VoicingKind = VClose | VOpen | VDrop2 | VDrop24 | VQuartal | VCluster

derive instance eqVoicingKind :: Eq VoicingKind

voicingKinds :: Array VoicingKind
voicingKinds = [ VClose, VOpen, VDrop2, VDrop24, VQuartal, VCluster ]

voicingName :: VoicingKind -> String
voicingName = case _ of
  VClose -> "close"
  VOpen -> "open"
  VDrop2 -> "drop2"
  VDrop24 -> "drop24"
  VQuartal -> "quartal"
  VCluster -> "cluster"

voicingOf :: Int -> VoicingKind
voicingOf i = fromMaybe VClose (voicingKinds !! i)

voicingIx :: VoicingKind -> Int
voicingIx k = fromMaybe 0 (findIndex (_ == k) voicingKinds)

-- | The drops and quartal want four notes: without a chord, the scale's
-- | seventh on the lead rather than its triad.
wantsSeventh :: VoicingKind -> Boolean
wantsSeventh = case _ of
  VDrop2 -> true
  VDrop24 -> true
  VQuartal -> true
  _ -> false

-- | Harmonia's transformation of a close voicing.
strategy :: VoicingKind -> Voicing -> Voicing
strategy = case _ of
  VClose -> identity
  VOpen -> openTriad
  VDrop2 -> drop2
  VDrop24 -> drop2and4
  VQuartal -> quartal
  VCluster -> cluster

ruleOf :: RuleWire -> Rule
ruleOf r = case r.kind of
  1 -> Voice r.n
  2 -> Lead r.n
  _ -> Melody

ruleWire :: Rule -> RuleWire
ruleWire = case _ of
  Melody -> { kind: 0, n: 0 }
  Voice k -> { kind: 1, n: k }
  Lead n -> { kind: 2, n }

-- | The four heads' rules, `Melody` where none is given.
rulesOf :: Voices -> Array Rule
rulesOf v = map (\i -> maybe' (v.rules !! i)) [ 0, 1, 2, 3 ]
  where
  maybe' = case _ of
    Just r -> ruleOf r
    Nothing -> Melody

-- | `drop2 melody voice 1 voice 3 lead -2`: a voicing, then a rule per head.
parseVoices :: String -> Either String Voices
parseVoices txt = case uncons (A.filter (_ /= "") (split (Pattern " ") (trim txt))) of
  Nothing -> Left "voices needs a voicing"
  Just { head: v, tail } -> case findIndex (\k -> voicingName k == v) voicingKinds of
    Nothing -> Left ("no voicing " <> v <> " (close, open, drop2, drop24, quartal, cluster)")
    Just ix -> (\rs -> { voicing: ix, rules: map ruleWire (take 4 rs) }) <$> rules tail
  where
  rules ws = case uncons ws of
    Nothing -> Right []
    Just { head: "melody", tail } -> (\rs -> [ Melody ] <> rs) <$> rules tail
    Just { head: w, tail } | w == "voice" || w == "lead" -> case uncons tail of
      Just { head: num, tail: rest } | Just n <- fromString num ->
        (\rs -> [ if w == "voice" then Voice n else Lead n ] <> rs) <$> rules rest
      _ -> Left (w <> " needs a number")
    Just { head: w } -> Left ("no rule " <> w <> " (melody, voice N, lead N)")

printVoices :: Voices -> String
printVoices v = joinWith " " ([ voicingName (voicingOf v.voicing) ] <> map rule (take (length v.rules) (rulesOf v)))
  where
  rule = case _ of
    Melody -> "melody"
    Voice k -> "voice " <> show k
    Lead n -> "lead " <> show n
