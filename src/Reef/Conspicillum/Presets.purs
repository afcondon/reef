-- | **The presets**, as data: 86 scenes, each its line, what the
-- | line cannot carry, and the knobs worth playing.
-- |
-- | First written on 2026-09-27 by triggerfish's `audio/generate-presets.mjs`
-- | from the workshop's preset literals, which were auditioned at the rig one by
-- | one. **This file is now the source of truth**: edit presets here, not in the
-- | workshop page. Every line is resolved on both runtimes by the conformance
-- | suite (`conspicillumPresetRun`).
module Reef.Conspicillum.Presets
  ( presetSources
  , presets
  ) where

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Reef.Conspicillum.Corpus (Axis(..), Cmp(..), Toward(..), emptyQuery)
import Reef.Conspicillum.Parameter (Parameter(..), SendBus(..))
import Reef.Conspicillum.Preset (Bank(..), Preset, PresetSource, ResonatorFollow(..), knob, resolve)

-- | Every preset resolved, or the first that will not: never a silent drop.
presets :: Either String (Array Preset)
presets = traverse resolve presetSources

presetSources :: Array PresetSource
presetSources =
  [ { name: "Single hit"
    , bank: Early
    , about: "one sample, fixed read head — the Morphagene gesture"
    , line: "s \"chord-hits-0916-185508:0\" # grains 8 # sustain 0.14 # position 0.35 # follow 0 # gain 0.9 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Read head walk"
    , bank: Early
    , about: "long grains, no spray — drag position"
    , line: "s \"chord-hits-0916-185508:0\" # grains 4 # sustain 0.4 # position 0.1 # spray 0.02 # follow 0 # gain 0.95 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Cloud"
    , bank: Early
    , about: "whole corpus, full spray — the Borderlands move"
    , line: "s \"chord-hits-0915-232247\" # grains 32 # sustain 0.07 # position 0.5 # spray 1 # follow 0 # gain 0.55 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Every third backwards"
    , bank: Early
    , about: "the figure no granulator can express"
    , line: "s \"chord-hits-0916-185508\" # sustain 0.12 # position 0.4 # spray 0.6 # follow 0 # gain 0.8 # every 3 0 (speed -1) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Stutter cascade"
    , bank: Early
    , about: "euclid 7/16, every other grain clipped short"
    , line: "s \"chord-hits-0915-232247\" # euclid 7 16 # sustain 0.05 # position 0.45 # spray 0.35 # follow 0 # gain 0.9 # accelerate 0.4 # every 2 0 (length 0.35) # every 4 1 (speed 2) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Chance rain"
    , bank: Early
    , about: "re-rolled from the cycle seed — never twice the same"
    , line: "s \"chord-hits-0915-232247\" # grains 24 # sustain 0.06 # position 0.5 # spray 0.9 # follow 0 # gain 0.6 # sometimesBy 0.35 (pan 0.95) # sometimesBy 0.25 (speed -1) # sometimesBy 0.2 (gain 0.25) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Only the bright"
    , bank: Early
    , about: "a hard cut on zcr — filter, not weighting"
    , line: "s \"chord-hits-0915-232247\" # grains 20 # sustain 0.1 # position 0.4 # spray 0.7 # follow 0 # gain 0.75 # seed 12345"
    , query: emptyQuery { clauses = [ { axis: AZcr, cmp: Gt, value: 900.0 } ], weighting = Nothing }
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Mostly the long ones"
    , bank: Early
    , about: "a soft lean on decay — weighting, not filter"
    , line: "s \"chord-hits-0915-232247\" # grains 12 # sustain 0.18 # position 0.3 # spray 0.8 # follow 0 # gain 0.8 # seed 12345"
    , query: emptyQuery { clauses = [], weighting = Just { axis: ADecay, toward: High, strength: 0.9 } }
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Deep drone"
    , bank: Early
    , about: "quarter speed, half-second grains"
    , line: "s \"chord-hits-0916-185508:14\" # grains 4 # sustain 0.5 # position 0.2 # spray 0.15 # follow 0 # speed 0.25 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Transect sweep"
    , bank: Early
    , about: "the 4×12 drum grid, straight through"
    , line: "s \"drum-hits-0912-121546\" # euclid 9 16 # sustain 0.09 # position 0.15 # spray 0.25 # follow 0 # gain 0.7 # every 4 2 (speed -1) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Andrew's chord"
    , bank: Found
    , about: "one hit, long grains from the attack, saturated into a slow leslie"
    , line: "s \"chord-hits-0916-185508:0\" # grains 4 # sustain 1.62 # spray 0.185 # follow 0 # gain 1.02 # shape 0.45 # lpf 5500 # room 0.37 # size 0.47 # dry 0.29 # delay 0.22 # delaytime 0.31 # leslie 0.23 # lrate 10.4 # lsize 0.09 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Brion Gysin cut-ups"
    , bank: Found
    , about: "one two-second splice per bar, swelling in backwards, out of a 116-second tape"
    , line: "s \"burroughs-junky\" # grains 1 # sustain 2 # position 0.2 # spray 0.55 # follow 0 # gain 0.8 # lpf 4200 # res 0.2 # genv 1 # gtilt 0.92 # gplat 0.05 # curve -3 # room 0.65 # size 0.9 # dry 0.2 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Brion Gysin Daleks"
    , bank: Found
    , about: "reversed at a tenth speed, crushed, then pitched back up — the voice only pshift can make"
    , line: "s \"burroughs-junky\" # euclid 5 16 # sustain 2 # position 0.2 # spray 0.55 # follow 0 # speed -0.1 # gain 0.8 # crush 5.5 # coarse 15 # lpf 6850 # res 0.47 # pshift 1.82 # phdepth 0.31 # gtilt 0.95 # gplat 0.08 # curve -2.5 # room 0.29 # size 0.83 # dry 0.2 # grsnpitch 46 # grsndecay 5.7 # grsnbright 0.9 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Brion Gysin Speeding"
    , bank: Found
    , about: "23 bursts a bar, each sounding for a seventh of the slot it sits in"
    , line: "s \"burroughs-junky:0\" # euclid 23 29 # sustain 1.545 # position 0.3 # spray 0.25 # follow 0 # gain 0.9 # coarse 7 # lpf 7100 # res 0.25 # tremolo 6.5 # tremdepth 0.85 # atk 0.055 # hold 0.11 # rel 0.05 # curve -2 # room 0.55 # size 0.85 # dry 0.2 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Polychord pairs"
    , bank: Polychords
    , about: "two plain triads struck at once every other bar — a walk through the set's pairings"
    , line: "s \"chord-hits-0924-171929\" # onsets \"0 0 0.5\" # sustain 2.6 # follow 0 # gain 0.75 # room 0.3 # size 0.7 # dry 0.1 # every 6 2 (gain 0) # every 6 4 (gain 0) # every 3 0 (pan 0.3) # every 3 1 (pan 0.7) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Tape"
    , bank: Sector
    , about: "the bar as sixteen grains, in order — should sound exactly like the loop"
    , line: "s \"fd-beat-bar\" # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob TapeFollow "tape rate", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Repeat"
    , bank: Sector
    , about: "the head sometimes doesn't advance: a sixteenth again, and the tape catches up"
    , line: "s \"fd-beat-bar\" # walk 0 0.14 0 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob TapeFollow "tape rate", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Relocate"
    , bank: Sector
    , about: "the head jumps anywhere and carries on from there — the bar re-laid, home at every downbeat"
    , line: "s \"fd-beat-bar\" # walk 0.22 0 0.06 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob TapeFollow "tape rate", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Eighth hops"
    , bank: Sector
    , about: "short hops on the eighth-note grid, often back to the tape"
    , line: "s \"fd-beat-bar\" # walk 0.25 0 0.3 # grid 8 # reach 2 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob TapeFollow "tape rate", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Fill"
    , bank: Sector
    , about: "plain for a bar, then a ratcheted fill into the next downbeat"
    , line: "s \"fd-beat-bar\" # every 32 24 (shift -0.125) # every 32 26 (shift -0.25) # every 32 28 (ratchet 2) # every 32 29 (ratchet 3) # every 32 30 (ratchet 4) # every 32 31 (ratchet 6) # every 32 31 (gain 0.8) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob TapeFollow "tape rate", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Stutter"
    , bank: Sector
    , about: "one grain in seven rolls into four; one in nine holds"
    , line: "s \"fd-beat-bar\" # walk 0 0.11 0 # sometimesBy 0.14 (ratchet 4) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob TapeFollow "tape rate", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Beat echo"
    , bank: Sector
    , about: "every sixteenth twice: itself, and a quieter copy from a beat earlier, panned right"
    , line: "s \"fd-beat-bar\" # onsets \"0 0 0.0625 0.0625 0.125 0.125 0.1875 0.1875 0.25 0.25 0.3125 0.3125 0.375 0.375 0.4375 0.4375 0.5 0.5 0.5625 0.5625 0.625 0.625 0.6875 0.6875 0.75 0.75 0.8125 0.8125 0.875 0.875 0.9375 0.9375\" # every 2 1 (shift -0.25) # every 2 1 (gain 0.45) # every 2 1 (pan 0.85) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob TapeFollow "tape rate", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Drag"
    , bank: Sector
    , about: "the head at half rate: each sixteenth heard twice over, pitch untouched — half-time"
    , line: "s \"fd-beat-bar\" # follow 0.5 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob TapeFollow "tape rate", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Half-speed tape"
    , bank: Sector
    , about: "the tape really slowed: half rate AND half speed, an octave down"
    , line: "s \"fd-beat-bar\" # follow 0.5 # speed 0.5 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob TapeFollow "tape rate", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Double time"
    , bank: Sector
    , about: "the head at twice the rate: the bar twice a bar, every other sixteenth"
    , line: "s \"fd-beat-bar\" # follow 2 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob TapeFollow "tape rate", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Reverse bar"
    , bank: Sector
    , about: "the tape backwards: slices in reverse order, each played backwards"
    , line: "s \"fd-beat-bar\" # position -0.0625 # follow -1 # speed -1 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob TapeFollow "tape rate", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Tape sag"
    , bank: Sector
    , about: "the last beat slows, grain by grain, like a tape running down"
    , line: "s \"fd-beat-bar\" # every 16 12 (speed 0.8) # every 16 13 (speed 0.62) # every 16 14 (speed 0.45) # every 16 15 (speed 0.3) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob TapeFollow "tape rate", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Cloud at the head"
    , bank: Sector
    , about: "48 short grains sprayed round a moving head — the beat smeared, still in time"
    , line: "s \"fd-beat-bar\" # grains 48 # sustain 0.09 # spray 0.035 # gain 0.6 # sometimesBy 0.5 (pan 0.25) # sometimesBy 0.5 (pan 0.75) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob TapeFollow "tape rate", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Breakdown"
    , bank: Sector
    , about: "hops, holds, reversals and a roll — the Sector move, coming home every bar"
    , line: "s \"fd-beat-bar\" # walk 0.2 0.12 0.1 # grid 8 # reach 3 # every 16 14 (ratchet 3) # sometimesBy 0.1 (speed -1) # sometimesBy 0.06 (speed 0.5) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob TapeFollow "tape rate", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Every 4th downbeat back"
    , bank: Fix
    , about: "ordinals count across bars, so every 64th grain is the downbeat of every fourth bar"
    , line: "s \"fd-beat-bar\" # every 64 0 (speed -1) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob GrainLength "grain", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Snare up a fifth"
    , bank: Fix
    , about: "only what scores as snare is pitch-shifted — fix (# pshift 1.5) snare"
    , line: "s \"fd-beat-bar\" # fix snare 0.5 (pshift 1.5) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob GrainLength "grain", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Kick bassline"
    , bank: Fix
    , about: "each kick rings a resonator tuned from a sequence — the kick drum plays the bass"
    , line: "s \"fd-beat-bar\" # rsndecay 0.9 # rsnbright 0.35 # rsnmix 0.75 # fix kick 0.5 (rsnpitch \"36 36 43 39\") # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob GrainLength "grain", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Kick bassline, per bar"
    , bank: Fix
    , about: "one note a bar, <36 36 39 31> — four bars of root motion"
    , line: "s \"fd-beat-bar\" # rsndecay 1.4 # rsnbright 0.3 # rsnmix 0.8 # fix kick 0.5 (rsnpitch \"<36 36 39 31>\") # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob GrainLength "grain", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Snare roll build"
    , bank: Fix
    , about: "the snare ratchets 1, 2, 3, 4 across four bars, <1 2 3 4>"
    , line: "s \"fd-beat-bar\" # fix snare 0.5 (ratchet \"<1 2 3 4>\") # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob GrainLength "grain", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Hat ping-pong"
    , bank: Fix
    , about: "hats alternate left and right; at 0.2 the kicks' click counts as hat too"
    , line: "s \"fd-beat-bar\" # fix hat 0.2 (pan \"0.1 0.9\") # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob GrainLength "grain", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Creative errors"
    , bank: Fix
    , about: "thresholds low enough to be wrong: half the bar is 'snare', crushed and dropped an octave"
    , line: "s \"fd-beat-bar\" # fix snare 0.1 (pshift 0.5) # fix hat 0.15 (crush 5) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob GrainLength "grain", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Fix under the walk"
    , bank: Fix
    , about: "the Breakdown walk, with snare and kick rules that follow whatever the head lands on"
    , line: "s \"fd-beat-bar\" # walk 0.2 0.12 0.1 # grid 8 # reach 3 # rsndecay 0.8 # rsnbright 0.35 # rsnmix 0.7 # fix snare 0.5 (pshift 1.5) # fix kick 0.5 (rsnpitch \"36 43 39 34\") # fix hat 0.5 (ratchet 3) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob GrainLength "grain", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Snare to the return"
    , bank: Dub
    , about: "every snare flicked into send A — put a delay on 3/4"
    , line: "s \"fd-beat-bar\" # fix snare 0.5 (send 1) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob (SendLevel SendA) "send A", knob (SendLevel SendB) "send B", knob WalkJump "chaos" ]
    }
  , { name: "Throw the last hit"
    , bank: Dub
    , about: "the last sixteenth of every bar into send A, loud"
    , line: "s \"fd-beat-bar\" # sendA 1 # every 16 15 (send 1) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob (SendLevel SendA) "send A", knob (SendLevel SendB) "send B", knob WalkJump "chaos" ]
    }
  , { name: "Hats to the room"
    , bank: Dub
    , about: "hats into send B — put a reverb on 5/6"
    , line: "s \"fd-beat-bar\" # fix hat 0.3 (send 2) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob (SendLevel SendA) "send A", knob (SendLevel SendB) "send B", knob WalkJump "chaos" ]
    }
  , { name: "Random throws"
    , bank: Dub
    , about: "now and then a grain goes to A, or B, alternating"
    , line: "s \"fd-beat-bar\" # sendA 0.9 # sendB 0.9 # sometimesBy 0.1 (send \"1 2\") # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob (SendLevel SendA) "send A", knob (SendLevel SendB) "send B", knob WalkJump "chaos" ]
    }
  , { name: "Echo out every 4th bar"
    , bank: Dub
    , about: "the whole last beat of every fourth bar thrown into A"
    , line: "s \"fd-beat-bar\" # sendA 1 # every 64 60 (send 1) # every 64 61 (send 1) # every 64 62 (send 1) # every 64 63 (send 1) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob (SendLevel SendA) "send A", knob (SendLevel SendB) "send B", knob WalkJump "chaos" ]
    }
  , { name: "Dub breakdown"
    , bank: Dub
    , about: "the Breakdown walk, snares to A and the odd jump-landing to B"
    , line: "s \"fd-beat-bar\" # walk 0.2 0.12 0.1 # grid 8 # reach 3 # fix snare 0.5 (send 1) # sometimesBy 0.08 (send 2) # fix snare 0.5 (pshift 0.75) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob (SendLevel SendA) "send A", knob (SendLevel SendB) "send B", knob WalkJump "chaos" ]
    }
  , { name: "Swing it"
    , bank: Swung
    , about: "a straight break played at 62%: offbeat sixteenths land late"
    , line: "s \"fd-beat-bar\" # swing 0.62 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob PlaySwing "swing", knob WalkJump "chaos", knob WalkHold "repeat" ]
    }
  , { name: "Heavy shuffle"
    , bank: Swung
    , about: "68%, near triplet — the short offbeats get clipped, which is the feel"
    , line: "s \"fd-beat-bar\" # swing 0.68 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob PlaySwing "swing", knob WalkJump "chaos", knob WalkHold "repeat" ]
    }
  , { name: "Eighth swing"
    , bank: Swung
    , about: "swing on 8ths instead of 16ths: the and-of-each-beat moves"
    , line: "s \"fd-beat-bar\" # swing 0.6 # swinggrid 8 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob PlaySwing "swing", knob WalkJump "chaos", knob WalkHold "repeat" ]
    }
  , { name: "Swung tape, as it was"
    , bank: Swung
    , about: "a swung break with its swing declared: the tape back exactly"
    , line: "s \"fd-beat-bar-swung\" # swing 0.62 # tapeswing 0.62 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob PlaySwing "swing", knob WalkJump "chaos", knob WalkHold "repeat" ]
    }
  , { name: "Swung breakdown, kept"
    , bank: Swung
    , about: "the Breakdown walk on a swung break, slices cut on ITS grid: the swing survives the chopping"
    , line: "s \"fd-beat-bar-swung\" # walk 0.2 0.12 0.1 # grid 8 # reach 3 # swing 0.62 # tapeswing 0.62 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob PlaySwing "swing", knob WalkJump "chaos", knob WalkHold "repeat" ]
    }
  , { name: "Swung breakdown, broken"
    , bank: Swung
    , about: "the same, told the tape is straight: offbeat slices land on downbeats late — the Sector problem"
    , line: "s \"fd-beat-bar-swung\" # walk 0.2 0.12 0.1 # grid 8 # reach 3 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob PlaySwing "swing", knob WalkJump "chaos", knob WalkHold "repeat" ]
    }
  , { name: "Straighten it"
    , bank: Swung
    , about: "the swung break played straight: cut on its swing, landed on the grid"
    , line: "s \"fd-beat-bar-swung\" # tapeswing 0.62 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob PlaySwing "swing", knob WalkJump "chaos", knob WalkHold "repeat" ]
    }
  , { name: "Progression from hits"
    , bank: Harmony
    , about: "four separate chord hits as a I–vi–IV–V in G, one a bar — nothing rendered"
    , line: "s \"chord-hits-0924-171929\" # bars 4 # samples \"<4 7 5 0>\" # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob TapeFollow "tape rate", knob PlaySwing "swing", knob (SendLevel SendB) "space" ]
    }
  , { name: "ii–V–I–vi from hits"
    , bank: Harmony
    , about: "the same hits re-chosen: Am7 D G Em, a turnaround"
    , line: "s \"chord-hits-0924-171929\" # bars 4 # samples \"<8 0 4 7>\" # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob PlaySwing "swing", knob (SendLevel SendB) "space" ]
    }
  , { name: "Hits, Sector'd"
    , bank: Harmony
    , about: "the I–vi–IV–V with the walk on the eighths and a stutter now and then"
    , line: "s \"chord-hits-0924-171929\" # walk 0.25 0.15 0.2 # grid 8 # reach 3 # bars 4 # samples \"<4 7 5 0>\" # sometimesBy 0.1 (ratchet 3) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Progression as tape"
    , bank: Harmony
    , about: "the comping loop as 16 grains a bar — should sound like the loop"
    , line: "s \"prog-g-2bar\" # bars 2 # gain 1.6 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob TapeFollow "tape rate", knob PlaySwing "swing", knob (SendLevel SendB) "space" ]
    }
  , { name: "Chords swapped"
    , bank: Harmony
    , about: "each bar's two chords traded: G-Em becomes Em-G, Csus2-D becomes D-Csus2"
    , line: "s \"prog-g-2bar\" # bars 2 # steps \"2 3 0 1\" # gain 1.6 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob PlaySwing "swing", knob (SendLevel SendB) "space" ]
    }
  , { name: "Bar order 0 1 1 0"
    , bank: Harmony
    , about: "the phrase re-laid a bar at a time: four-bar form from a two-bar loop"
    , line: "s \"prog-g-2bar\" # bars 2 # order \"0 1 1 0\" # gain 1.6 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob PlaySwing "swing", knob (SendLevel SendB) "space" ]
    }
  , { name: "Harmonic Sector"
    , bank: Harmony
    , about: "Sector's step table on the eighths: some steps jump, some only sometimes"
    , line: "s \"prog-g-2bar\" # walk 0 0.1 0 # grid 8 # bars 2 # steps \"~ ~ 5?0.4 ~ 2?0.35 ~ ~ 7?0.25\" # gain 1.6 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkHold "repeat", knob WalkJump "chaos", knob (SendLevel SendB) "space" ]
    }
  , { name: "Chord hops"
    , bank: Harmony
    , about: "the walk on the half-bar grid: whole chords swapped in and out, home every bar"
    , line: "s \"prog-g-2bar\" # walk 0.35 0 0.2 # grid 2 # bars 2 # gain 1.6 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHome "home", knob (SendLevel SendB) "space" ]
    }
  , { name: "Comping stutter"
    , bank: Harmony
    , about: "the chord re-struck as a roll now and then"
    , line: "s \"prog-g-2bar\" # bars 2 # gain 1.6 # sometimesBy 0.12 (ratchet 3) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob (SendLevel SendA) "throw" ]
    }
  , { name: "Swung comping"
    , bank: Harmony
    , about: "the straight comping loop at 62% swing"
    , line: "s \"prog-g-2bar\" # swing 0.62 # bars 2 # gain 1.6 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob PlaySwing "swing", knob WalkJump "chaos", knob (SendLevel SendB) "space" ]
    }
  , { name: "Stabs to the room"
    , bank: Harmony
    , about: "every downbeat chord into send B — put a reverb on it"
    , line: "s \"prog-g-2bar\" # bars 2 # gain 1.6 # every 8 0 (send 2) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob (SendLevel SendB) "space", knob WalkJump "chaos", knob PlaySwing "swing" ]
    }
  , { name: "Progression breakdown"
    , bank: Harmony
    , about: "hops, holds, a reversed chord, a roll — the Breakdown on harmony"
    , line: "s \"prog-g-2bar\" # walk 0.2 0.12 0.1 # grid 8 # reach 3 # bars 2 # gain 1.6 # sometimesBy 0.08 (speed -1) # every 16 14 (ratchet 3) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob WalkJump "chaos", knob WalkHold "repeat", knob (SendLevel SendB) "space" ]
    }
  , { name: "Attack wave"
    , bank: Envelopes
    , about: "a six-step triangle over the ATTACK, indexed by grain ordinal — an LFO that cannot drift"
    , line: "s \"burroughs-junky:0\" # euclid 23 29 # sustain 1.2 # position 0.3 # spray 0.35 # follow 0 # gain 0.9 # coarse 7 # lpf 7100 # res 0.25 # atk 0.03 # hold 0.11 # rel 0.05 # curve -2 # room 0.55 # size 0.85 # dry 0.2 # every 6 0 (atk 0.004) # every 6 1 (atk 0.03) # every 6 2 (atk 0.09) # every 6 3 (atk 0.18) # every 6 4 (atk 0.09) # every 6 5 (atk 0.03) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Crushed every fifth"
    , bank: Effects
    , about: "the headline — one grain in five, bitcrushed"
    , line: "s \"chord-hits-0916-185508\" # grains 20 # sustain 0.11 # position 0.4 # spray 0.55 # follow 0 # gain 0.8 # room 0.25 # size 0.5 # every 5 0 (crush 3) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Pitch shadow"
    , bank: Effects
    , about: "a fifth up and an octave down, in the SAME time"
    , line: "s \"chord-hits-0916-185508\" # grains 12 # sustain 0.16 # position 0.35 # spray 0.45 # follow 0 # gain 0.8 # room 0.35 # size 0.6 # every 3 0 (pshift 1.5) # every 7 2 (pshift 0.5) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Formant choir"
    , bank: Effects
    , about: "a, i, o, u — one vowel per grain, in rotation"
    , line: "s \"chord-hits-0915-232247\" # grains 8 # sustain 0.22 # position 0.4 # spray 0.5 # follow 0 # gain 0.85 # res 0.45 # room 0.5 # size 0.75 # dry 0.15 # every 4 0 (vowel 1) # every 4 1 (vowel 3) # every 4 2 (vowel 4) # every 4 3 (vowel 5) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Cathedral"
    , bank: Effects
    , about: "four long grains a cycle into a very large room"
    , line: "s \"chord-hits-0916-185508\" # grains 4 # sustain 0.45 # position 0.25 # spray 0.3 # follow 0 # gain 0.75 # lpf 3500 # res 0.1 # room 0.85 # size 0.95 # dry 0.25 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Dub chamber"
    , bank: Effects
    , about: "delay LOCKED to the cycle, so it stays in time"
    , line: "s \"chord-hits-0916-185508\" # euclid 5 16 # sustain 0.09 # position 0.4 # spray 0.6 # follow 0 # gain 0.8 # room 0.2 # delay 0.6 # delaytime 0.375 # delayfeedback 0.72 # lock 1 # sometimesBy 0.4 (pan 0.92) # every 4 2 (hpf 900) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Shredder"
    , bank: Effects
    , about: "decimate, crush and distort — all three at once"
    , line: "s \"drum-hits-0912-121546\" # euclid 11 16 # sustain 0.05 # position 0.1 # spray 0.4 # follow 0 # gain 0.55 # accelerate 0.2 # shape 0.65 # crush 3 # lpf 6000 # res 0.2 # room 0.15 # every 3 1 (coarse 12) # sometimesBy 0.3 (speed -1) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Filter rain"
    , bank: Effects
    , about: "every grain lands in a different band"
    , line: "s \"chord-hits-0915-232247\" # grains 28 # sustain 0.07 # position 0.5 # spray 0.95 # follow 0 # gain 0.65 # res 0.35 # room 0.45 # size 0.8 # delay 0.25 # delaytime 0.125 # delayfeedback 0.5 # lock 1 # sometimesBy 0.4 (bpf 400) # sometimesBy 0.3 (bpf 1400) # sometimesBy 0.25 (bpf 4200) # sometimesBy 0.5 (pan 0.85) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Leslie drift"
    , bank: Effects
    , about: "rotary on the chain, phaser on every third grain"
    , line: "s \"chord-hits-0915-232247\" # grains 6 # sustain 0.2 # position 0.35 # spray 0.4 # follow 0 # gain 0.85 # phdepth 0.8 # room 0.3 # leslie 0.75 # lrate 5.2 # lsize 0.4 # every 3 0 (phaser 0.4) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Tape saturation"
    , bank: Effects
    , about: "soft clip and a lid — nothing per grain at all"
    , line: "s \"chord-hits-0916-185508\" # sustain 0.13 # position 0.35 # spray 0.5 # follow 0 # gain 0.7 # shape 0.45 # lpf 5200 # res 0.15 # room 0.2 # size 0.5 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Tremolo bed"
    , bank: Effects
    , about: "long grains, amplitude-chopped under the cloud"
    , line: "s \"chord-hits-0915-232247\" # grains 3 # sustain 0.5 # position 0.3 # spray 0.25 # follow 0 # speed 0.5 # gain 0.9 # lpf 2600 # res 0.25 # tremolo 6.5 # tremdepth 0.85 # room 0.55 # size 0.85 # dry 0.2 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Struck bar"
    , bank: Resonators
    , about: "a ringing bank — high partials die first"
    , line: "s \"chord-hits-0916-185508\" # grains 4 # sustain 1.6 # position 0.02 # spray 0.25 # follow 0 # gain 0.75 # genv 1 # gtilt 0.015 # gplat 0.02 # curve -4 # rsnpitch 52 # rsnbright 0.9 # rsnmix 0.85 # room 0.4 # size 0.8 # dry 0.2 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Dull thud"
    , bank: Resonators
    , about: "the same bank with the highs taken out"
    , line: "s \"chord-hits-0916-185508\" # euclid 5 16 # sustain 0.7 # position 0.02 # spray 0.2 # follow 0 # gain 0.85 # genv 1 # gtilt 0.01 # gplat 0.02 # curve -4 # rsnpitch 40 # rsndecay 1.2 # rsnbright 0.38 # rsnmix 0.9 # room 0.2 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Plucked chord"
    , bank: Resonators
    , about: "Karplus-Strong, strung with the grain — one chord tone per grain"
    , line: "s \"chord-hits-0916-185508\" # grains 6 # sustain 1.4 # position 0.02 # spray 0.15 # follow 0 # gain 0.8 # genv 1 # gtilt 0.01 # gplat 0.02 # curve -4 # rsnpitch 48 # rsndecay 1.3 # rsnbright 0.82 # rsnmix 0.95 # rsnmodel 1 # room 0.35 # size 0.7 # delay 0.2 # delayfeedback 0.45 # lock 1 # seed 12345"
    , query: emptyQuery
    , progression: Just
        { chords: [ "F#m(maj7)", "B7", "E", "E" ]
        , minimumFit: 0.4
        , strength: 0.9
        , resonatorFollows: FollowChordTones
        }
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Answer the chord"
    , bank: Resonators
    , about: "a filter the material often fails — the resonator carries the harmony"
    , line: "s \"chord-hits-0916-185508\" # grains 10 # sustain 0.5 # position 0.02 # spray 0.3 # follow 0 # gain 0.9 # genv 1 # gtilt 0.02 # gplat 0.05 # curve -4 # room 0.55 # size 0.9 # dry 0.25 # grsn 0.85 # grsndecay 5.5 # grsnbright 0.88 # seed 12345"
    , query: emptyQuery
    , progression: Just
        { chords: [ "Bm", "Bm(maj7)", "Bdim", "F#m" ]
        , minimumFit: 0.75
        , strength: 0.98
        , resonatorFollows: FollowRoot
        }
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "One body, many strikes"
    , bank: Resonators
    , about: "the global resonator — the cloud is the exciter, nothing is per grain"
    , line: "s \"drum-hits-0912-121546\" # euclid 9 16 # sustain 0.05 # position 0.05 # spray 0.4 # follow 0 # gain 0.6 # room 0.3 # size 0.7 # grsn 0.9 # grsnpitch 38 # grsndecay 8 # grsnbright 0.92 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Reverse swell"
    , bank: Envelopes
    , about: "tilt 0.92 — every grain arrives backwards"
    , line: "s \"chord-hits-0915-232247\" # grains 5 # sustain 0.35 # position 0.2 # spray 0.55 # follow 0 # gain 0.8 # lpf 4200 # res 0.2 # genv 1 # gtilt 0.92 # gplat 0.05 # curve -3 # room 0.65 # size 0.9 # dry 0.2 # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Burst grains"
    , bank: Envelopes
    , about: "tremolo at 6 cycles per grain — a multi-lobe envelope from an effect wired for something else"
    , line: "s \"chord-hits-0915-232247\" # grains 6 # sustain 0.25 # position 0.15 # spray 0.4 # follow 0 # gain 0.85 # tremolo 24 # tremdepth 1 # genv 1 # gtilt 0.3 # gplat 0.4 # room 0.3 # size 0.6 # every 3 1 (tremolo 40) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Struck every fourth"
    , bank: Resonators
    , about: "the resonator as a RULE — three dry grains, then a rung one"
    , line: "s \"chord-hits-0915-232247\" # grains 8 # sustain 0.9 # position 0.03 # spray 0.5 # follow 0 # gain 0.7 # genv 1 # gtilt 0.02 # gplat 0.03 # curve -4 # rsndecay 0.85 # rsnbright 0.86 # rsnmix 0.8 # rsnmodel 1 # room 0.4 # size 0.75 # every 4 0 (rsnpitch 57) # every 8 4 (rsnpitch 64) # sometimesBy 0.3 (pan 0.9) # seed 12345"
    , query: emptyQuery
    , progression: Nothing
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "ii – V – I in E"
    , bank: Progressions
    , about: "leaf-cloud answers all three exactly"
    , line: "s \"chord-hits-0915-232247\" # sustain 0.11 # position 0.4 # spray 0.6 # follow 0 # gain 0.8 # seed 12345"
    , query: emptyQuery
    , progression: Just
        { chords: [ "F#m(maj7)", "B7", "E", "E" ]
        , minimumFit: 0.5
        , strength: 0.95
        , resonatorFollows: NoFollow
        }
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Chromatic descent"
    , bank: Progressions
    , about: "Bm → Bm(maj7) → Bdim → F#m"
    , line: "s \"chord-hits-0916-185508\" # grains 12 # sustain 0.15 # position 0.35 # spray 0.5 # follow 0 # gain 0.85 # seed 12345"
    , query: emptyQuery
    , progression: Just
        { chords: [ "Bm", "Bm(maj7)", "Bdim", "F#m" ]
        , minimumFit: 0.5
        , strength: 0.95
        , resonatorFollows: NoFollow
        }
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Diminished drift"
    , bank: Progressions
    , about: "chord per cycle, chance on top"
    , line: "s \"chord-hits-0915-232247\" # euclid 11 16 # sustain 0.08 # position 0.45 # spray 0.85 # follow 0 # gain 0.7 # accelerate 0.15 # sometimesBy 0.3 (pan 0.9) # every 5 0 (speed -1) # seed 12345"
    , query: emptyQuery
    , progression: Just
        { chords: [ "C#dim", "F#dim", "D#m", "B7" ]
        , minimumFit: 0.5
        , strength: 0.95
        , resonatorFollows: NoFollow
        }
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Wet progression"
    , bank: Progressions
    , about: "a chord a cycle, through the whole chain"
    , line: "s \"chord-hits-0915-232247\" # grains 14 # sustain 0.14 # position 0.4 # spray 0.6 # follow 0 # gain 0.75 # lpf 7000 # res 0.2 # room 0.6 # size 0.85 # dry 0.15 # delay 0.3 # delayfeedback 0.55 # lock 1 # every 5 0 (pshift 2) # sometimesBy 0.25 (crush 4) # seed 12345"
    , query: emptyQuery
    , progression: Just
        { chords: [ "F#m(maj7)", "B7", "E", "E" ]
        , minimumFit: 0.5
        , strength: 0.95
        , resonatorFollows: NoFollow
        }
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  , { name: "Everything at once"
    , bank: Progressions
    , about: "progression + euclid + chance + reversal"
    , line: "s \"chord-hits-0915-232247\" # euclid 13 32 # sustain 0.06 # position 0.5 # spray 1 # follow 0 # gain 0.55 # accelerate 0.5 # every 3 0 (speed -1) # sometimesBy 0.4 (pan 0.95) # every 7 2 (length 0.3) # sometimesBy 0.2 (accelerate 1.5) # seed 12345"
    , query: emptyQuery
    , progression: Just
        { chords: [ "E", "D", "Bm", "E", "F#m(maj7)", "B7", "E", "E" ]
        , minimumFit: 0.45
        , strength: 0.95
        , resonatorFollows: NoFollow
        }
    , knobs: [ knob GrainLength "grain", knob ReadPosition "position", knob Spray "spray", knob (SendLevel SendB) "space" ]
    }
  ]
