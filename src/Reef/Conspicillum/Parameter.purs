-- | **Every playable number in a Conspicillum scene, named and described.**
-- |
-- | `Reef.Conspicillum.Cloud.Spec` is what the engine reads. This is the
-- | control layer over it: the same numbers as things a player turns, each with
-- | a name, a range, a taper, a unit, and a sentence about what it does. A knob,
-- | a draggable number in the module's sentence, a MIDI mapping and a preset's
-- | list of knobs all point at a `Parameter`, and so a misspelt knob is a
-- | compile error rather than a control that silently moves nothing.
-- |
-- | Each parameter comes with a getter and setter on `Spec` (`read` and
-- | `write`), total over the closed type, which is a lens in all but name: the
-- | purerl package set has no lens library, and this compiles on both runtimes.
-- |
-- | The engine's own record fields keep SuperDirt's spellings (`lpf`, `grsn`),
-- | because they are the wire. Here the names are whole words
-- | (`LowPassCutoff`, `SharedResonatorSend`), and `read` and `write` are where
-- | the two meet.
-- |
-- | What is NOT here, because it is structure rather than a number to turn:
-- | the onsets, the tape's bar order and samples, the step table, the rules and
-- | the query. Those are edited as what they are, not as a knob.
module Reef.Conspicillum.Parameter
  ( Parameter(..)
  , SendBus(..)
  , GrainEffect(..)
  , CloudEffect(..)
  , Section(..)
  , Measure(..)
  , Taper(..)
  , Description
  , allParameters
  , describe
  , identifier
  , fromIdentifier
  , sectionName
  , read
  , write
  , set
  , snap
  , normalise
  , denormalise
  , format
  , neutral
  , opParameter
  , engaged
  ) where

import Prelude

import Data.Array (any, filter, find, index, length, updateAt, (..))
import Data.Int (round, toNumber)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Reef.Conspicillum.Cloud (Chain, Fx, Op(..), Rule, Send, Spec, noChain)
import Reef.Conspicillum.Decimal (fixed)
import Reef.Numeric (ln, pow)

-- ── the parameters ───────────────────────────────────────────────────────────

data Parameter
  -- the grain and where it reads
  = GrainLength
  | ReadPosition
  | Spray
  | TapeFollow
  -- the walk: how the read head wanders within a bar
  | WalkJump
  | WalkHold
  | WalkHome
  | WalkReach
  -- swing
  | PlaySwing
  | TapeSwing
  -- the voice every grain starts from
  | Speed
  | Gain
  | Pan
  | Accelerate
  -- the two send buses
  | SendLevel SendBus
  -- effects on each grain, and on the cloud as a whole
  | GrainEffect GrainEffect
  | CloudEffect CloudEffect

derive instance eqParameter :: Eq Parameter

data SendBus = SendA | SendB

derive instance eqSendBus :: Eq SendBus

-- | SuperDirt's per-event effects, one per grain. Zero is off for the ones
-- | that have an amount (see `Cloud.Fx`); the rest shape an effect that is on.
data GrainEffect
  = Waveshape
  | BitCrush
  | SampleRateReduction
  | LowPassCutoff
  | HighPassCutoff
  | BandPassCentre
  | FilterResonance
  | Vowel
  | PitchShift
  | TremoloRate
  | TremoloDepth
  | PhaserRate
  | PhaserDepth
  | GrainEnvelope
  | EnvelopePeak
  | EnvelopePlateau
  | EnvelopeAttack
  | EnvelopeHold
  | EnvelopeRelease
  | EnvelopeCurve
  | ResonatorPitch
  | ResonatorDecay
  | ResonatorBrightness
  | ResonatorMix
  | ResonatorModel

derive instance eqGrainEffect :: Eq GrainEffect

-- | The chain the whole cloud feeds: one reverb, one delay, one leslie, one
-- | resonator that every grain strikes.
data CloudEffect
  = ReverbAmount
  | ReverbSize
  | ReverbDry
  | DelayAmount
  | DelayTime
  | DelayFeedback
  | DelayLockedToCycle
  | LeslieAmount
  | LeslieRate
  | LeslieSize
  | SharedResonatorSend
  | SharedResonatorPitch
  | SharedResonatorDecay
  | SharedResonatorBrightness

derive instance eqCloudEffect :: Eq CloudEffect

-- ── how a parameter is described ─────────────────────────────────────────────

data Section = Grain | ReadHead | Walk | Swing | Voice | Sends | GrainEffects | CloudEffects

derive instance eqSection :: Eq Section

-- | How a value reads, which is also what it means.
data Measure
  = Seconds                  -- "125 ms", "1.50 s"
  | Proportion               -- "35%"
  | Multiple                 -- "1.00×"
  | SwingAmount              -- "straight", "0.58"
  | Level                    -- "0.80"
  | Hertz                    -- "800 Hz"
  | Count                    -- "3"
  | Stereo                   -- "centre", "30% left"
  | MidiNote                 -- "A#2"; zero is "off"
  | Choice (Array String)    -- a small closed set, by index
  | Plain Int                -- a bare number, to this many places

-- | `Exponential` for the numbers people hear in ratios: a grain of 20 ms
-- | against 40 ms is as big a step as 1 s against 2 s.
data Taper = Linear | Exponential

derive instance eqTaper :: Eq Taper

type Description =
  { label :: String
  , section :: Section
  , low :: Number
  , high :: Number
  , step :: Number
  , taper :: Taper
  , measure :: Measure
  -- | Where a scene that says nothing about it stands: Sector's defaults
  -- | (`Reef.Conspicillum.Notation.defaultLine`), and for the effects,
  -- | `noFx` and `noChain`, which are the effects switched off.
  , neutral :: Number
  -- | What has to be above zero for this to be heard at all. Empty means
  -- | itself: an effect's amount is engaged when it is above zero, and a
  -- | depth is engaged when its rate is.
  , engagedBy :: Array Parameter
  -- | Zero is a meaningful setting of this one, not "off": a peak at 0 is a
  -- | percussive envelope, a curve of 0 is linear, a mix of 0 is dry.
  , zeroMeans :: Boolean
  , about :: String
  }

-- | Every parameter, in the order a panel would list them.
allParameters :: Array Parameter
allParameters =
  [ GrainLength, ReadPosition, Spray, TapeFollow
  , WalkJump, WalkHold, WalkHome, WalkReach
  , PlaySwing, TapeSwing
  , Speed, Gain, Pan, Accelerate
  , SendLevel SendA, SendLevel SendB
  ]
    <> map GrainEffect
      [ Waveshape, BitCrush, SampleRateReduction, LowPassCutoff, HighPassCutoff, BandPassCentre
      , FilterResonance, Vowel, PitchShift, TremoloRate, TremoloDepth, PhaserRate, PhaserDepth
      , GrainEnvelope, EnvelopePeak, EnvelopePlateau, EnvelopeAttack, EnvelopeHold, EnvelopeRelease
      , EnvelopeCurve, ResonatorPitch, ResonatorDecay, ResonatorBrightness, ResonatorMix, ResonatorModel
      ]
    <> map CloudEffect
      [ ReverbAmount, ReverbSize, ReverbDry, DelayAmount, DelayTime, DelayFeedback, DelayLockedToCycle
      , LeslieAmount, LeslieRate, LeslieSize
      , SharedResonatorSend, SharedResonatorPitch, SharedResonatorDecay, SharedResonatorBrightness
      ]

-- | A linear parameter with a plain description.
plain :: String -> Section -> Number -> Number -> Number -> Measure -> Number -> String -> Description
plain label section low high step measure neutral about =
  { label, section, low, high, step, taper: Linear, measure, neutral
  , engagedBy: [], zeroMeans: false, about }

-- | An effect setting that only matters while another is engaged.
shaping :: Array Parameter -> Description -> Description
shaping by d = d { engagedBy = by, zeroMeans = true }

-- | Engaged while any of these is, but its own zero is still "off".
gatedBy :: Array Parameter -> Description -> Description
gatedBy by d = d { engagedBy = by }

describe :: Parameter -> Description
describe = case _ of
  GrainLength -> (plain "grain length" Grain 0.01 2.0 0.005 Seconds 0.125
    "How long each grain sounds. On a tape followed at a sixteenth a grain, a sixteenth is the whole slice; longer grains overlap, shorter ones leave gaps.")
    { taper = Exponential }
  ReadPosition -> plain "read position" ReadHead 0.0 1.0 0.005 Proportion 0.0
    "Where in the material the read head sits, as a fraction of it. With the tape followed, it is an offset from where the tape would be."
  Spray -> plain "spray" ReadHead 0.0 1.0 0.005 Proportion 0.0
    "How far each grain may land from the read head, at random, as a fraction of the material."
  TapeFollow -> plain "tape follow" ReadHead (-1.0) 2.0 0.01 Multiple 1.0
    "How fast the read head moves along the tape, against the bar: 1 reads the tape in time, 0 holds still, 2 reads a bar of tape in half a bar, negative runs backwards."
  WalkJump -> plain "jump" Walk 0.0 1.0 0.01 Proportion 0.0
    "How often the read head relocates to another step in the bar and reads on from there. The walk resets at every bar line."
  WalkHold -> plain "repeat" Walk 0.0 1.0 0.01 Proportion 0.0
    "How often the read head stays where it is, so the grain repeats the one before it: Sector's repeat, and a run of them is a stutter."
  WalkHome -> plain "return" Walk 0.0 1.0 0.01 Proportion 0.0
    "How often a displaced read head goes back to where the tape is."
  WalkReach -> plain "reach" Walk 0.0 32.0 1.0 Count 0.0
    "The furthest a jump may go, in steps of the walk's grid; 0 means anywhere in the bar."
  PlaySwing -> plain "swing" Swing 0.5 0.75 0.01 SwingAmount 0.5
    "Where the offbeat of each pair lands: 0.5 straight, 0.66 triplet, 0.75 dotted."
  TapeSwing -> plain "tape swing" Swing 0.5 0.75 0.01 SwingAmount 0.5
    "The swing the tape was played with, so its slices are cut on its own hits. Equal to the swing, a followed tape plays as recorded."
  Speed -> plain "speed" Voice (-2.0) 3.0 0.01 Multiple 1.0
    "Playback rate of each grain, which moves its pitch: 2 is an octave up, negative plays it backwards."
  Gain -> plain "gain" Voice 0.0 1.5 0.01 Level 1.0
    "How loud every grain is before its rules."
  Pan -> plain "pan" Voice 0.0 1.0 0.01 Stereo 0.5
    "Where every grain sits, left to right, before its rules."
  Accelerate -> plain "glide" Voice (-1.0) 2.0 0.01 (Plain 2) 0.0
    "A pitch glide across each grain, as SuperDirt's accelerate."
  SendLevel bus -> plain ("send " <> sendName bus) Sends 0.0 1.0 0.01 Level 0.8
    ("How loud a grain sent to bus " <> sendName bus <> " plays there. The bus is a dry orbit on its own outputs, for a return in Ableton.")
  GrainEffect effect -> describeGrainEffect effect
  CloudEffect effect -> describeCloudEffect effect

describeGrainEffect :: GrainEffect -> Description
describeGrainEffect = case _ of
  Waveshape -> fx "waveshape" 0.0 0.99 0.01 (Plain 2) 0.0
    "Distortion by waveshaping; 0 is off."
  BitCrush -> fx "bit crush" 0.0 16.0 0.25 (Plain 2) 0.0
    "Bit depth: 4 is brutal, 16 is transparent, 0 is off."
  SampleRateReduction -> fx "sample-rate reduction" 0.0 32.0 1.0 Count 0.0
    "Keep one sample in every n; 1 or less does nothing."
  LowPassCutoff -> fx "low-pass" 0.0 12000.0 50.0 Hertz 0.0
    "Low-pass filter cutoff; 0 is no filter, not a closed one."
  HighPassCutoff -> fx "high-pass" 0.0 6000.0 25.0 Hertz 0.0
    "High-pass filter cutoff; 0 is off."
  BandPassCentre -> fx "band-pass" 0.0 8000.0 25.0 Hertz 0.0
    "Band-pass filter centre; 0 is off."
  FilterResonance -> gatedBy [ GrainEffect LowPassCutoff, GrainEffect HighPassCutoff, GrainEffect Vowel ]
    (fx "resonance" 0.0 0.9 0.01 (Plain 2) 0.0 "Resonance, shared by the low-pass, high-pass and vowel filters.")
  Vowel -> fx "vowel" 0.0 5.0 1.0 (Choice [ "off", "a", "e", "i", "o", "u" ]) 0.0
    "A formant filter that makes the grain say a vowel."
  PitchShift -> fx "pitch shift" 0.0 3.0 0.01 Multiple 0.0
    "Transpose without changing speed, as a ratio: 1.5 is a fifth up. 0 is off."
  TremoloRate -> fx "tremolo" 0.0 60.0 0.1 Hertz 0.0
    "Tremolo rate. Past about 30 Hz it stops being tremolo and becomes ring modulation."
  TremoloDepth -> gatedBy [ GrainEffect TremoloRate ] (fx "tremolo depth" 0.0 1.0 0.01 Proportion 0.5 "How deep the tremolo goes.")
  PhaserRate -> fx "phaser" 0.0 10.0 0.05 Hertz 0.0 "Phaser rate; 0 is off."
  PhaserDepth -> gatedBy [ GrainEffect PhaserRate ] (fx "phaser depth" 0.0 1.0 0.01 Proportion 0.5 "How deep the phaser goes.")
  GrainEnvelope -> fx "grain envelope" 0.0 1.0 1.0 (Choice [ "off", "on" ]) 0.0
    "Give each grain its own amplitude envelope (SuperDirt's grenvelo), shaped by peak and plateau."
  EnvelopePeak -> shaping [ GrainEffect GrainEnvelope ] (fx "envelope peak" 0.0 1.0 0.01 Proportion 0.25
    "Where in the grain the envelope peaks: near 0 is a struck attack, near 1 a reverse swell.")
  EnvelopePlateau -> shaping [ GrainEffect GrainEnvelope ] (fx "envelope plateau" 0.0 1.0 0.01 Proportion 0.3
    "How much of the grain the envelope holds at its peak: low is a bump, high a burst.")
  EnvelopeAttack -> fx "attack" 0.0 0.5 0.005 Seconds 0.0 "Attack of the plain envelope; 0 is off."
  EnvelopeHold -> shaping [ GrainEffect EnvelopeAttack, GrainEffect EnvelopeRelease ]
    (fx "hold" 0.0 0.5 0.005 Seconds 0.0 "Hold of the plain envelope.")
  EnvelopeRelease -> fx "release" 0.0 1.0 0.01 Seconds 0.0 "Release of the plain envelope; 0 is off."
  EnvelopeCurve -> shaping [ GrainEffect GrainEnvelope, GrainEffect EnvelopeAttack, GrainEffect EnvelopeRelease ]
    (fx "envelope curve" (-8.0) 8.0 0.5 (Plain 1) 0.0 "The envelopes' curve: 0 linear, negative snaps, positive swells.")
  ResonatorPitch -> fx "resonator" 0.0 96.0 1.0 MidiNote 0.0
    "Strike a tuned resonator with each grain, at this note. Its ring ends with the grain; the shared resonator rings on."
  ResonatorDecay -> gatedBy [ GrainEffect ResonatorPitch ] (fx "resonator decay" 0.0 6.0 0.05 Seconds 1.5 "How long the resonator rings.")
  ResonatorBrightness -> gatedBy [ GrainEffect ResonatorPitch ]
    (fx "resonator brightness" 0.3 0.99 0.01 (Plain 2) 0.7 "How much of its ring each higher partial keeps.")
  ResonatorMix -> shaping [ GrainEffect ResonatorPitch ] (fx "resonator mix" 0.0 1.0 0.01 Proportion 1.0 "Resonator against dry grain.")
  ResonatorModel -> shaping [ GrainEffect ResonatorPitch ] (fx "resonator model" 0.0 1.0 1.0 (Choice [ "ringing bank", "plucked string" ]) 0.0
    "A bank of ringing partials, or a plucked string (Karplus–Strong).")
  where
  fx label = plain label GrainEffects

describeCloudEffect :: CloudEffect -> Description
describeCloudEffect = case _ of
  ReverbAmount -> chain "reverb" 0.0 1.0 0.01 Level 0.0 "How much of the cloud goes to the reverb; 0 is off."
  ReverbSize -> gatedBy [ CloudEffect ReverbAmount ] (chain "reverb size" 0.0 1.0 0.01 Proportion 0.4 "The size of the room.")
  ReverbDry -> gatedBy [ CloudEffect ReverbAmount ] (chain "reverb dry" 0.0 1.0 0.01 Level 0.0 "Dry signal kept beside the reverb.")
  DelayAmount -> chain "delay" 0.0 1.0 0.01 Level 0.0 "How much of the cloud goes to the delay; 0 is off."
  DelayTime -> gatedBy [ CloudEffect DelayAmount ] (chain "delay time" 0.0 2.0 0.005 Seconds 0.25 "Time between repeats (or a fraction of the cycle, when locked).")
  DelayFeedback -> gatedBy [ CloudEffect DelayAmount ] (chain "delay feedback" 0.0 0.98 0.01 Proportion 0.4 "How much of each repeat repeats again.")
  DelayLockedToCycle -> shaping [ CloudEffect DelayAmount ] (chain "delay locked" 0.0 1.0 1.0 (Choice [ "seconds", "cycle" ]) 0.0
    "Whether the delay time is in seconds or a fraction of the cycle.")
  LeslieAmount -> chain "leslie" 0.0 1.0 0.01 Level 0.0 "A rotating speaker; 0 is off."
  LeslieRate -> gatedBy [ CloudEffect LeslieAmount ] (chain "leslie rate" 0.0 20.0 0.1 Hertz 6.7 "How fast the speaker turns.")
  LeslieSize -> gatedBy [ CloudEffect LeslieAmount ] (chain "leslie size" 0.0 1.0 0.01 Proportion 0.3 "How big the speaker cabinet is.")
  SharedResonatorSend -> chain "shared resonator" 0.0 1.0 0.01 Level 0.0
    "How much of the cloud strikes one resonator that rings freely: many grains, one body."
  SharedResonatorPitch -> shaping [ CloudEffect SharedResonatorSend ] (chain "shared resonator pitch" 0.0 96.0 1.0 MidiNote 45.0
    "The note the shared resonator is tuned to.")
  SharedResonatorDecay -> gatedBy [ CloudEffect SharedResonatorSend ] (chain "shared resonator decay" 0.0 12.0 0.1 Seconds 3.0
    "How long the shared resonator rings.")
  SharedResonatorBrightness -> gatedBy [ CloudEffect SharedResonatorSend ] (chain "shared resonator brightness" 0.3 0.99 0.01 (Plain 2) 0.85
    "How much of its ring each higher partial keeps.")
  where
  chain label = plain label CloudEffects

sendName :: SendBus -> String
sendName = case _ of
  SendA -> "A"
  SendB -> "B"

sectionName :: Section -> String
sectionName = case _ of
  Grain -> "the grain"
  ReadHead -> "the read head"
  Walk -> "the walk"
  Swing -> "swing"
  Voice -> "the voice"
  Sends -> "the sends"
  GrainEffects -> "effects on each grain"
  CloudEffects -> "effects on the whole cloud"

-- ── identifiers: a stable name for saving and mapping ────────────────────────

-- | A stable text name, for preset files, MIDI maps and URLs. Never shown to a
-- | player; `label` is for that.
identifier :: Parameter -> String
identifier = case _ of
  GrainLength -> "grainLength"
  ReadPosition -> "readPosition"
  Spray -> "spray"
  TapeFollow -> "tapeFollow"
  WalkJump -> "walkJump"
  WalkHold -> "walkHold"
  WalkHome -> "walkHome"
  WalkReach -> "walkReach"
  PlaySwing -> "playSwing"
  TapeSwing -> "tapeSwing"
  Speed -> "speed"
  Gain -> "gain"
  Pan -> "pan"
  Accelerate -> "accelerate"
  SendLevel SendA -> "sendLevelA"
  SendLevel SendB -> "sendLevelB"
  GrainEffect e -> "grain." <> grainEffectIdentifier e
  CloudEffect e -> "cloud." <> cloudEffectIdentifier e

grainEffectIdentifier :: GrainEffect -> String
grainEffectIdentifier = case _ of
  Waveshape -> "waveshape"
  BitCrush -> "bitCrush"
  SampleRateReduction -> "sampleRateReduction"
  LowPassCutoff -> "lowPassCutoff"
  HighPassCutoff -> "highPassCutoff"
  BandPassCentre -> "bandPassCentre"
  FilterResonance -> "filterResonance"
  Vowel -> "vowel"
  PitchShift -> "pitchShift"
  TremoloRate -> "tremoloRate"
  TremoloDepth -> "tremoloDepth"
  PhaserRate -> "phaserRate"
  PhaserDepth -> "phaserDepth"
  GrainEnvelope -> "grainEnvelope"
  EnvelopePeak -> "envelopePeak"
  EnvelopePlateau -> "envelopePlateau"
  EnvelopeAttack -> "envelopeAttack"
  EnvelopeHold -> "envelopeHold"
  EnvelopeRelease -> "envelopeRelease"
  EnvelopeCurve -> "envelopeCurve"
  ResonatorPitch -> "resonatorPitch"
  ResonatorDecay -> "resonatorDecay"
  ResonatorBrightness -> "resonatorBrightness"
  ResonatorMix -> "resonatorMix"
  ResonatorModel -> "resonatorModel"

cloudEffectIdentifier :: CloudEffect -> String
cloudEffectIdentifier = case _ of
  ReverbAmount -> "reverbAmount"
  ReverbSize -> "reverbSize"
  ReverbDry -> "reverbDry"
  DelayAmount -> "delayAmount"
  DelayTime -> "delayTime"
  DelayFeedback -> "delayFeedback"
  DelayLockedToCycle -> "delayLockedToCycle"
  LeslieAmount -> "leslieAmount"
  LeslieRate -> "leslieRate"
  LeslieSize -> "leslieSize"
  SharedResonatorSend -> "sharedResonatorSend"
  SharedResonatorPitch -> "sharedResonatorPitch"
  SharedResonatorDecay -> "sharedResonatorDecay"
  SharedResonatorBrightness -> "sharedResonatorBrightness"

fromIdentifier :: String -> Maybe Parameter
fromIdentifier name = find (\p -> identifier p == name) allParameters

-- ── reading and writing a spec ───────────────────────────────────────────────

read :: Parameter -> Spec -> Number
read parameter spec = case parameter of
  GrainLength -> spec.cloud.sustain
  ReadPosition -> spec.cloud.position
  Spray -> spec.cloud.spray
  TapeFollow -> spec.cloud.follow
  WalkJump -> spec.walk.jump
  WalkHold -> spec.walk.hold
  WalkHome -> spec.walk.home
  WalkReach -> toNumber spec.walk.reach
  PlaySwing -> spec.swing.play
  TapeSwing -> spec.swing.tape
  Speed -> spec.speed
  Gain -> spec.gain
  Pan -> spec.pan
  Accelerate -> spec.accelerate
  SendLevel bus -> maybe' (_.level) (index spec.sends (busIndex bus))
  GrainEffect effect -> readGrainEffect effect spec.fx
  CloudEffect effect -> readCloudEffect effect spec.chain
  where
  maybe' f m = case m of
    Just x -> f x
    Nothing -> (describe parameter).neutral

-- | Set a parameter to exactly this value. `set` is the one to call from a
-- | control, since it keeps the value in range and on the step.
write :: Parameter -> Number -> Spec -> Spec
write parameter value spec = case parameter of
  GrainLength -> spec { cloud = spec.cloud { sustain = value } }
  ReadPosition -> spec { cloud = spec.cloud { position = value } }
  Spray -> spec { cloud = spec.cloud { spray = value } }
  TapeFollow -> spec { cloud = spec.cloud { follow = value } }
  WalkJump -> spec { walk = spec.walk { jump = value } }
  WalkHold -> spec { walk = spec.walk { hold = value } }
  WalkHome -> spec { walk = spec.walk { home = value } }
  -- Reach is counted in the walk's own grid, so it can be no more than it.
  WalkReach -> spec { walk = spec.walk { reach = min spec.walk.grid (max 0 (round value)) } }
  PlaySwing -> spec { swing = spec.swing { play = value } }
  TapeSwing -> spec { swing = spec.swing { tape = value } }
  Speed -> spec { speed = value }
  Gain -> spec { gain = value }
  Pan -> spec { pan = value }
  Accelerate -> spec { accelerate = value }
  SendLevel bus -> spec { sends = setLevel (busIndex bus) value (standardSends spec.sends) }
  GrainEffect effect -> spec { fx = writeGrainEffect effect value spec.fx }
  CloudEffect effect -> spec { chain = writeCloudEffect effect value spec.chain }

busIndex :: SendBus -> Int
busIndex = case _ of
  SendA -> 0
  SendB -> 1

-- | A spec with fewer than two sends gets the rig's standard pair filled in —
-- | dry, on orbits 10 and 11 — so that turning send B is never a no-op.
standardSends :: Array Send -> Array Send
standardSends sends =
  map (\i -> fromMaybe (standard i) (index sends i)) (0 .. (max 1 (length sends - 1)))
  where
  standard i = { chain: noChain { orbit = 10 + i }, level: 0.8 }

setLevel :: Int -> Number -> Array Send -> Array Send
setLevel i value sends = fromMaybe sends do
  send <- index sends i
  updateAt i (send { level = value }) sends

readGrainEffect :: GrainEffect -> Fx -> Number
readGrainEffect = case _ of
  Waveshape -> _.shape
  BitCrush -> _.crush
  SampleRateReduction -> _.coarse
  LowPassCutoff -> _.lpf
  HighPassCutoff -> _.hpf
  BandPassCentre -> _.bpf
  FilterResonance -> _.res
  Vowel -> _.vowel
  PitchShift -> _.pshift
  TremoloRate -> _.tremolo
  TremoloDepth -> _.tremdepth
  PhaserRate -> _.phaser
  PhaserDepth -> _.phdepth
  GrainEnvelope -> _.genv
  EnvelopePeak -> _.gtilt
  EnvelopePlateau -> _.gplat
  EnvelopeAttack -> _.atk
  EnvelopeHold -> _.hold
  EnvelopeRelease -> _.rel
  EnvelopeCurve -> _.curve
  ResonatorPitch -> _.rsnpitch
  ResonatorDecay -> _.rsndecay
  ResonatorBrightness -> _.rsnbright
  ResonatorMix -> _.rsnmix
  ResonatorModel -> _.rsnmodel

writeGrainEffect :: GrainEffect -> Number -> Fx -> Fx
writeGrainEffect effect x fx = case effect of
  Waveshape -> fx { shape = x }
  BitCrush -> fx { crush = x }
  SampleRateReduction -> fx { coarse = x }
  LowPassCutoff -> fx { lpf = x }
  HighPassCutoff -> fx { hpf = x }
  BandPassCentre -> fx { bpf = x }
  FilterResonance -> fx { res = x }
  Vowel -> fx { vowel = x }
  PitchShift -> fx { pshift = x }
  TremoloRate -> fx { tremolo = x }
  TremoloDepth -> fx { tremdepth = x }
  PhaserRate -> fx { phaser = x }
  PhaserDepth -> fx { phdepth = x }
  GrainEnvelope -> fx { genv = x }
  EnvelopePeak -> fx { gtilt = x }
  EnvelopePlateau -> fx { gplat = x }
  EnvelopeAttack -> fx { atk = x }
  EnvelopeHold -> fx { hold = x }
  EnvelopeRelease -> fx { rel = x }
  EnvelopeCurve -> fx { curve = x }
  ResonatorPitch -> fx { rsnpitch = x }
  ResonatorDecay -> fx { rsndecay = x }
  ResonatorBrightness -> fx { rsnbright = x }
  ResonatorMix -> fx { rsnmix = x }
  ResonatorModel -> fx { rsnmodel = x }

readCloudEffect :: CloudEffect -> Chain -> Number
readCloudEffect = case _ of
  ReverbAmount -> _.room
  ReverbSize -> _.size
  ReverbDry -> _.dry
  DelayAmount -> _.delay
  DelayTime -> _.delaytime
  DelayFeedback -> _.delayfeedback
  DelayLockedToCycle -> _.lock
  LeslieAmount -> _.leslie
  LeslieRate -> _.lrate
  LeslieSize -> _.lsize
  SharedResonatorSend -> _.grsn
  SharedResonatorPitch -> _.grsnpitch
  SharedResonatorDecay -> _.grsndecay
  SharedResonatorBrightness -> _.grsnbright

writeCloudEffect :: CloudEffect -> Number -> Chain -> Chain
writeCloudEffect effect x chain = case effect of
  ReverbAmount -> chain { room = x }
  ReverbSize -> chain { size = x }
  ReverbDry -> chain { dry = x }
  DelayAmount -> chain { delay = x }
  DelayTime -> chain { delaytime = x }
  DelayFeedback -> chain { delayfeedback = x }
  DelayLockedToCycle -> chain { lock = x }
  LeslieAmount -> chain { leslie = x }
  LeslieRate -> chain { lrate = x }
  LeslieSize -> chain { lsize = x }
  SharedResonatorSend -> chain { grsn = x }
  SharedResonatorPitch -> chain { grsnpitch = x }
  SharedResonatorDecay -> chain { grsndecay = x }
  SharedResonatorBrightness -> chain { grsnbright = x }

-- ── the value as a control sees it ───────────────────────────────────────────

-- | Into range, and onto the step. The step is counted from `low`, so a range
-- | that does not start on a multiple of its step still lands on its own grid.
-- |
-- | The result is then rounded to six decimal places, which is not cosmetic:
-- | `0.01 + 12 * 0.005` is `0.07000000000000001`, and a value the notation
-- | prints to six places could not come back as that double. Snapped values
-- | are exactly the numbers the line writes.
snap :: Parameter -> Number -> Number
snap parameter value =
  let
    d = describe parameter
    clamped = max d.low (min d.high value)
    steps = Int.round ((clamped - d.low) / d.step)
  in
    max d.low (min d.high (sixPlaces (d.low + toNumber steps * d.step)))

-- | Built from integers, since the purerl set's `Data.Number` has no `round`,
-- | and a whole value times a million would overflow an Int (a 12 kHz cutoff).
-- | The numerator is an exact integer in a double, and IEEE division rounds
-- | correctly, so the result is the double the six-place text parses to.
sixPlaces :: Number -> Number
sixPlaces x =
  let
    whole = Int.floor x
    millionths = Int.round ((x - toNumber whole) * 1000000.0)
  in
    (toNumber whole * 1000000.0 + toNumber millionths) / 1000000.0

-- | A control's move: snapped, then written.
set :: Parameter -> Number -> Spec -> Spec
set parameter value = write parameter (snap parameter value)

-- | 0 to 1 along the control's travel, by its taper. An exponential control
-- | whose range starts at zero cannot be exponential all the way down, and none
-- | here is: grain length starts at 10 ms.
normalise :: Parameter -> Number -> Number
normalise parameter value =
  let d = describe parameter
  in
    clampUnit case d.taper of
      Exponential | d.low > 0.0 -> ln (max d.low value / d.low) / ln (d.high / d.low)
      _ -> (value - d.low) / (d.high - d.low)

denormalise :: Parameter -> Number -> Number
denormalise parameter position =
  let
    d = describe parameter
    u = clampUnit position
  in
    snap parameter case d.taper of
      Exponential | d.low > 0.0 -> d.low * pow (d.high / d.low) u
      _ -> d.low + u * (d.high - d.low)

clampUnit :: Number -> Number
clampUnit = max 0.0 <<< min 1.0

neutral :: Parameter -> Number
neutral = _.neutral <<< describe

-- | The value as a player reads it, identically on every runtime.
format :: Parameter -> Number -> String
format parameter value
  | offAtZero (describe parameter) && value == 0.0 = "off"
  | otherwise = case (describe parameter).measure of
    Seconds
      | value < 1.0 -> fixed 0 (value * 1000.0) <> " ms"
      | otherwise -> fixed 2 value <> " s"
    Proportion -> fixed 0 (value * 100.0) <> "%"
    Multiple -> fixed 2 value <> "×"
    SwingAmount
      | value < 0.505 -> "straight"
      | otherwise -> fixed 2 value
    Level -> fixed 2 value
    Hertz
      | value < 100.0 -> fixed 1 value <> " Hz"
      | otherwise -> fixed 0 value <> " Hz"
    Count -> fixed 0 value
    Stereo
      | value > 0.495 && value < 0.505 -> "centre"
      | value < 0.5 -> fixed 0 ((0.5 - value) * 200.0) <> "% left"
      | otherwise -> fixed 0 ((value - 0.5) * 200.0) <> "% right"
    MidiNote
      | value <= 0.0 -> "off"
      | otherwise -> noteName (round value)
    Choice names -> fromMaybe (fixed 0 value) (index names (round value))
    Plain places -> fixed places value

-- | An effect whose zero is "not engaged" reads "off" there, not "0 Hz".
offAtZero :: Description -> Boolean
offAtZero d = (d.section == GrainEffects || d.section == CloudEffects) && not d.zeroMeans && d.engagedBy == []

noteName :: Int -> String
noteName midi =
  let pitchClass = ((midi `mod` 12) + 12) `mod` 12
  in fromMaybe "?" (index [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ] pitchClass)
    <> show (Int.floor (toNumber midi / 12.0) - 1)

-- ── rules, as moves of a parameter ───────────────────────────────────────────

-- | The parameter a rule's op sets, where there is one. `OpLength` scales the
-- | grain as the rule finds it, and `OpShift`, `OpRatchet` and `OpSend` act on
-- | the grain's reading, count and destination rather than on any one setting,
-- | so they name none.
opParameter :: Op -> Maybe Parameter
opParameter = case _ of
  OpSpeed _ -> Just Speed
  OpGain _ -> Just Gain
  OpLength _ -> Just GrainLength
  OpPan _ -> Just Pan
  OpAccelerate _ -> Just Accelerate
  OpShape _ -> Just (GrainEffect Waveshape)
  OpCrush _ -> Just (GrainEffect BitCrush)
  OpCoarse _ -> Just (GrainEffect SampleRateReduction)
  OpLpf _ -> Just (GrainEffect LowPassCutoff)
  OpHpf _ -> Just (GrainEffect HighPassCutoff)
  OpBpf _ -> Just (GrainEffect BandPassCentre)
  OpRes _ -> Just (GrainEffect FilterResonance)
  OpVowel _ -> Just (GrainEffect Vowel)
  OpPshift _ -> Just (GrainEffect PitchShift)
  OpTremolo _ -> Just (GrainEffect TremoloRate)
  OpPhaser _ -> Just (GrainEffect PhaserRate)
  OpGenv _ -> Just (GrainEffect GrainEnvelope)
  OpGtilt _ -> Just (GrainEffect EnvelopePeak)
  OpGplat _ -> Just (GrainEffect EnvelopePlateau)
  OpAtk _ -> Just (GrainEffect EnvelopeAttack)
  OpHold _ -> Just (GrainEffect EnvelopeHold)
  OpRel _ -> Just (GrainEffect EnvelopeRelease)
  OpCurve _ -> Just (GrainEffect EnvelopeCurve)
  OpRsnPitch _ -> Just (GrainEffect ResonatorPitch)
  OpRsnDecay _ -> Just (GrainEffect ResonatorDecay)
  OpRsnBright _ -> Just (GrainEffect ResonatorBrightness)
  OpRsnMix _ -> Just (GrainEffect ResonatorMix)
  OpRsnModel _ -> Just (GrainEffect ResonatorModel)
  OpShift _ -> Nothing
  OpRatchet _ -> Nothing
  OpSend _ -> Nothing

-- | **Is this parameter doing anything?** The question the workshop's hot
-- | markers ask. A control can be set and silent — a resonator tuned with its
-- | send at zero — and a preset that records it reads like one that uses it.
-- |
-- | Engaged when it is (non-zero or `zeroMeans`) and what it rides on is
-- | above zero, either in the spec or because a rule sets it: the formant
-- | choir's vowels come entirely from rules, with the base vowel at off.
-- | Parameters outside the effects are always engaged; the read head and the
-- | voice are never "off".
engaged :: Parameter -> Spec -> Boolean
engaged parameter spec = case parameter of
  GrainEffect _ -> effectEngaged
  CloudEffect _ -> effectEngaged
  _ -> true
  where
  d = describe parameter
  by = if d.engagedBy == [] then [ parameter ] else d.engagedBy
  ruled p = any (\r -> opParameter r.op == Just p && ruleEngages r) spec.rules
  ruleEngages :: Rule -> Boolean
  ruleEngages r = any (_ > 0.0) r.values || opAmount r.op > 0.0
  above p = read p spec > 0.0 || ruled p
  effectEngaged = (d.zeroMeans || read parameter spec > 0.0 || ruled parameter)
    && (d.engagedBy == [] || any above (filter (_ /= parameter) by))
    && (d.engagedBy /= [] || above parameter)

opAmount :: Op -> Number
opAmount = case _ of
  OpSpeed x -> x
  OpGain x -> x
  OpLength x -> x
  OpPan x -> x
  OpAccelerate x -> x
  OpShape x -> x
  OpCrush x -> x
  OpCoarse x -> x
  OpLpf x -> x
  OpHpf x -> x
  OpBpf x -> x
  OpRes x -> x
  OpVowel x -> x
  OpPshift x -> x
  OpTremolo x -> x
  OpPhaser x -> x
  OpGenv x -> x
  OpGtilt x -> x
  OpGplat x -> x
  OpAtk x -> x
  OpHold x -> x
  OpRel x -> x
  OpCurve x -> x
  OpRsnPitch x -> x
  OpRsnDecay x -> x
  OpRsnBright x -> x
  OpRsnMix x -> x
  OpRsnModel x -> x
  OpShift x -> x
  OpRatchet x -> x
  OpSend x -> x
