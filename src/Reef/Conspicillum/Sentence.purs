-- | **A scene in words**, with its numbers left as holes.
-- |
-- | The module says what a scene does as a few sentences. Every number in them
-- | is a `Value Parameter`, not text, so a renderer can make it draggable, mark
-- | the ones a knob is bound to, and print it with `Parameter.format`. The
-- | line is the machine form of a scene, and this is the human form.
-- |
-- | A clause appears when it is doing something, or when a knob is bound to
-- | it: a knob whose number you cannot see would be the one control on the
-- | panel with no readout.
-- |
-- | Rules read better as a table than as prose (which grains, what, by how
-- | much), so they are `ruleRows` rather than sentences.
module Reef.Conspicillum.Sentence
  ( Fragment(..)
  , Sentence
  , sentences
  , plainText
  , RuleRow
  , ruleRows
  ) where

import Prelude

import Data.Array (elem, filter, index, length, null)
import Data.Foldable (intercalate)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Reef.Conspicillum.Cloud (Kind(..), Op(..), ResonatorFollow(..), Rule, Spec, Step(..), When(..))
import Reef.Conspicillum.Decimal (fixed, trimmed)
import Reef.Conspicillum.Display (Material(..))
import Reef.Conspicillum.Notation (Line, OnsetLayout(..), onsetLayout, stepsText)
import Reef.Conspicillum.Parameter (CloudEffect(..), GrainEffect(..), Parameter(..), SendBus(..), describe, format, neutral, opAmount, opParameter, read)
import Reef.Conspicillum.Harmonic (nameOfChord)

data Fragment
  = Words String
  -- | A number in the scene: drawn from the spec, draggable, knob-able.
  | Value Parameter
  -- | The name of the set, which reads better set apart.
  | SetName String
  -- | Something best shown as written: a list of hits, a step table.
  | Code String
  -- | An aside, quieter than the rest.
  | Aside String

type Sentence = Array Fragment

sentences
  :: { line :: Line, material :: Material, knobs :: Array Parameter }
  -> Array Sentence
sentences { line, material, knobs } =
  filter (not <<< null)
    [ what <> onsets <> grain
    , reading
    , walking
    , order
    , stepTable
    , swinging
    , voice
    , effects grainEffects "Each grain goes through "
    , effects cloudEffects "The whole cloud goes through "
    , sends
    , chords
    ]
  where
  spec = line.spec
  -- Shown when doing something, or when a knob is on it.
  shows parameter active = active || elem parameter knobs
  moved parameter = read parameter spec /= neutral parameter

  what = case material of
    HitsAsBars keys ->
      [ Words "Play hits ", Code (intercalate " " (map show keys)), Words " of ", SetName line.set, Words " as a progression, one a bar" ]
    WholeTape t ->
      [ Words "Read ", SetName line.set
      , Words (" as a " <> (if t.bars <= 1 then "one" else show t.bars) <> "-bar tape"
          <> maybe "" (\bpm -> " at " <> trimmed 1 bpm <> " bpm") t.bpm)
      ]
    OneSample s
      | line.n == Nothing -> [ Words "Take the one sample in ", SetName line.set ]
      | otherwise -> [ Words ("Take sample " <> show s.index <> " of "), SetName line.set ]
    ManySamples ss -> [ Words ("Scatter grains from all " <> show (length ss) <> " samples of "), SetName line.set ]

  onsets = case onsetLayout spec.onsets of
    EvenGrains k -> [ Words (": " <> show k <> " grains a bar") ]
    Euclidean k n -> [ Words (": " <> show k <> " grains spread over " <> show n <> " steps") ]
    Placed os -> [ Words (": " <> show (length os) <> " grains at hand-placed times") ]

  grain = [ Words ", each ", Value GrainLength, Words " long." ]

  reading =
    let
      followed = spec.cloud.follow /= 0.0
      start =
        if followed then [ Words "The read head moves with the tape at ", Value TapeFollow, Words " its pace" ]
        else [ Words "The read head stays ", Value ReadPosition, Words " of the way in" ]
      offset =
        if followed && shows ReadPosition (spec.cloud.position > 0.0) then [ Words ", from ", Value ReadPosition, Words " in" ]
        else []
      spray = if shows Spray (spec.cloud.spray > 0.0) then [ Words ", sprayed across ", Value Spray ] else []
    in
      start <> offset <> spray <> [ Words "." ]

  walking =
    if not (shows WalkJump (spec.walk.jump > 0.0) || shows WalkHold (spec.walk.hold > 0.0)) then []
    else
      [ Words "It jumps somewhere else ", Value WalkJump, Words " of the time" ]
        <> (if shows WalkReach (spec.walk.reach > 0) then [ Words ", never more than ", Value WalkReach, Words " steps away" ] else [])
        <> (if shows WalkHold (spec.walk.hold > 0.0) then [ Words ", and repeats a step ", Value WalkHold, Words " of the time" ] else [])
        <> (if shows WalkHome (spec.walk.home > 0.0) then [ Words ", returning to the tape ", Value WalkHome, Words " of the time" ] else [])
        <> [ Words "." ]

  order =
    if null spec.tape.order then []
    else [ Words "The bars play in the order ", Code (intercalate " " (map show spec.tape.order)), Words "." ]

  stepTable =
    if null spec.steps.to then []
    else [ Words "A step table sends steps elsewhere: ", Code (stepsText spec.steps), Words "." ]

  swinging =
    if not (shows PlaySwing (spec.swing.play /= spec.swing.tape)) then []
    else if spec.swing.tape /= 0.5 then [ Words "Its own swing of ", Value TapeSwing, Words " is played at ", Value PlaySwing, Words "." ]
    else [ Words "Its swing is ", Value PlaySwing, Words "." ]

  voice =
    let
      parts = filter (not <<< null)
        [ if shows Speed (moved Speed) then [ Words "at ", Value Speed, Words " speed" ] else []
        , if shows Accelerate (moved Accelerate) then [ Words "gliding by ", Value Accelerate ] else []
        , if shows Pan (moved Pan) then [ Words "panned ", Value Pan ] else []
        ]
    in
      if null parts then [] else [ Words "Played " ] <> joinWith ", " parts <> [ Words "." ]

  effects list opening =
    let
      on = filter (\p -> shows p (moved p)) list
    in
      if null on then []
      else [ Words opening ] <> joinWith ", " (map (\p -> [ Words ((describe p).label <> " "), Value p ]) on) <> [ Words "." ]

  sends =
    [ Words "It throws ", Value (SendLevel SendA), Words " to A and ", Value (SendLevel SendB), Words " to B, at gain "
    , Value Gain, Words "."
    ]

  chords =
    let p = spec.progression
    in
      if null p.chords then []
      else
        [ Words "Its chord changes each cycle: ", Code (intercalate " → " (map (\c -> fromMaybe "?" (nameOfChord c)) p.chords)) ]
          <> (case p.follow of
                NoFollow -> []
                FollowRoot -> [ Words ", and the resonators follow its root" ]
                FollowBass -> [ Words ", and the resonators follow its bass" ]
                FollowChordTones -> [ Words ", and the resonators play its tones" ])
          <> [ Words "." ]

joinWith :: String -> Array Sentence -> Sentence
joinWith separator parts = intercalate [ Words separator ] parts

grainEffects :: Array Parameter
grainEffects = map GrainEffect
  [ Waveshape, BitCrush, SampleRateReduction, LowPassCutoff, HighPassCutoff, BandPassCentre
  , FilterResonance, Vowel, PitchShift, TremoloRate, TremoloDepth, PhaserRate, PhaserDepth
  , GrainEnvelope, EnvelopePeak, EnvelopePlateau, EnvelopeAttack, EnvelopeHold, EnvelopeRelease
  , EnvelopeCurve, ResonatorPitch, ResonatorDecay, ResonatorBrightness, ResonatorMix, ResonatorModel
  ]

cloudEffects :: Array Parameter
cloudEffects = map CloudEffect
  [ ReverbAmount, ReverbSize, ReverbDry, DelayAmount, DelayTime, DelayFeedback, DelayLockedToCycle
  , LeslieAmount, LeslieRate, LeslieSize
  , SharedResonatorSend, SharedResonatorPitch, SharedResonatorDecay, SharedResonatorBrightness
  ]

-- | The sentences as plain text, each number printed as a player reads it.
plainText :: Spec -> Array Sentence -> String
plainText spec = intercalate " " <<< map (intercalate "" <<< map fragment)
  where
  fragment = case _ of
    Words w -> w
    Value p -> format p (read p spec)
    SetName n -> n
    Code c -> c
    Aside a -> "(" <> a <> ")"

-- ── rules, as a table ────────────────────────────────────────────────────────

type RuleRow = { which :: String, what :: String, amount :: String }

ruleRows :: Array Rule -> Array RuleRow
ruleRows = map row
  where
  row r = { which: which r.when, what: what r.op, amount: amount r }

  which = case _ of
    Always -> "all"
    Every n k
      | n <= 1 -> "all"
      | k == 0 -> "every " <> ordinal n
      | otherwise -> "every " <> ordinal n <> ", from the " <> nth (k + 1)
    Chance p -> fixed 0 (p * 100.0) <> "%, at random"
    Hit kind _ -> kindName kind <> "-like"

  what op = case op of
    OpShift _ -> "read shift"
    OpRatchet _ -> "ratchet"
    OpSend _ -> "send"
    _ -> maybe "?" (_.label <<< describe) (opParameter op)

  amount r
    | not (null r.values) = intercalate " " (map (trimmed 3) r.values) <> (if r.step == PerBar then " (a bar each)" else " (a hit each)")
    | otherwise = case r.op of
        -- Speed, gain and length rules multiply what the grain already has.
        OpSpeed x -> trimmed 3 x <> "×"
        OpGain x -> trimmed 3 x <> "×"
        OpLength x -> trimmed 3 x <> "×"
        OpShift x -> trimmed 3 x <> " of the tape"
        OpRatchet x -> trimmed 0 x <> " repeats"
        OpSend x -> "bus " <> maybe (trimmed 0 x) identity (index [ "A", "B" ] (Int.round x - 1))
        op -> maybe (trimmed 3 (opAmount op)) (\p -> format p (opAmount op)) (opParameter op)


kindName :: Kind -> String
kindName = case _ of
  Kick -> "kick"
  Snare -> "snare"
  Hat -> "hat"

ordinal :: Int -> String
ordinal n = case n of
  2 -> "other"
  3 -> "third"
  4 -> "fourth"
  5 -> "fifth"
  6 -> "sixth"
  7 -> "seventh"
  8 -> "eighth"
  _ -> show n <> suffix
  where
  -- 11th, 12th and 13th break the rule of their last digit.
  suffix
    | n `mod` 100 >= 11 && n `mod` 100 <= 13 = "th"
    | n `mod` 10 == 1 = "st"
    | n `mod` 10 == 2 = "nd"
    | n `mod` 10 == 3 = "rd"
    | otherwise = "th"

nth :: Int -> String
nth n = case n of
  1 -> "first"
  2 -> "second"
  _ -> ordinal n
