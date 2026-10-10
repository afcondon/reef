-- | Reef test entry. Pins the Odonus `stepEmit` conformance golden (the same
-- | output the BEAM produces — see conformance/cross-runtime.sh). Asserts via
-- | Test.Assert so a drift in the engine fails CI under node; the cross-runtime
-- | script proves the Erlang side matches the same golden.
module Test.Main (main) where

import Prelude

import Data.Array (filter, length, mapWithIndex, range, take)
import Data.Either (Either(..), hush)
import Data.Int (toNumber)
import Data.Int as Int
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Reef.Vetula.Perf (wrapAt)
import Reef.Balistes.Kit (laneOfName)
import Reef.Routing as Routing
import Reef.Conformance (voicingRun, seleneRun, durRun, outScaleRun, routeRun, conspicillumHarmonicRun, conspicillumCloudRun, conspicillumRun, conspicillumGrains, run, chordRun, harmonyRun, headsRun, articulationRun, scaleRun, genRun, betaProbe, inputRun, simRun, balistesRun, balistesSimRun, balistesInputRun, fixedRun, vetulaRun, vetulaMidiRun)
import Reef.Conspicillum.Cloud as CL
import Reef.Conspicillum.Corpus as CC
import Reef.Marbles (seedFrom)
import Reef.Odonus (defaultOdonus)
import Reef.PitchSetGolden (tableRender)
import Reef.Protocol (decodeOdonus, encodeOdonus)
import Test.Assert (assert', assertEqual')
import Test.Calibration (calibrationTests)
import Test.Cards (cardsTests)
import Test.Rample (rampleTests)
import Test.Voices (voicesTests)

main :: Effect Unit
main = do
  voicesTests
  cardsTests
  rampleTests
  calibrationTests
  assertEqual' "Odonus stepEmit conformance (defaultOdonus, scale source, 32 steps)"
    { actual: run, expected: golden }
  log "Reef Odonus conformance golden: OK"

  -- Selene's language (docs/kb/plans/selene-in-tidal.md): the rack printed and
  -- read back, lines applied in turn and printed back, refusals. The BEAM
  -- prints the same (conformance/cross-runtime.sh).
  assertEqual' "Selene rack and line form golden"
    { actual: seleneRun, expected: seleneGolden }
  log "Reef Selene language golden: OK"

  -- Conspicillum: the grain selector (triggerfish/docs/CONSPICILLUM-DESIGN.md, C2).
  --
  -- The golden pins three things a drift in any of which is SILENT — the cloud
  -- would simply be made of different material, with no error anywhere:
  --
  --   the FILTER. Only samples 0-4 may appear. 5 fails `decay > 0.5`, 6 fails
  --   `harm <= 0.9`, and 7 is excluded because it carries no `harm` AT ALL —
  --   the "a sample that cannot answer an axis is excluded, not defaulted"
  --   rule, which a golden without a deficient sample could not defend.
  --
  --   the WEIGHTING. Leaning 0.8 toward high `zcr` over survivors spanning
  --   400-2400 Hz predicts roughly 35/26/18/14/7 percent for samples
  --   4/2/0/1/3; these 64 draws land 26/15/11/9/3.
  --
  --   the WINDOW. Every grain's `end - begin` is exactly `sustain / secs` for
  --   whichever sample it landed in — 12500 for a 4.0 s sample, 6250 for an
  --   8.0 s one. That is what keeps a cloud from transposing itself sample by
  --   sample, and it is wrong in a way that sounds deliberate.
  --
  -- Rendered as scaled integers: `show` on a Number is a formatting decision
  -- the two runtimes do not owe each other, and this golden has to compare the
  -- arithmetic. Same reasoning as the Beta/pow probe.
  assertEqual' ("Conspicillum grain-selector golden (" <> show conspicillumGrains <> " draws, filter + weighting + window)")
    { actual: conspicillumRun, expected: conspicillumGolden }
  log "Reef Conspicillum selector golden: OK"

  -- Conspicillum C3: the cloud over a cycle. Pins the two properties that make
  -- a grain addressable at all.
  --
  --   `Every 3 0` counted ACROSS cycles, not within one. With 8 onsets a cycle
  --   the reversed grains land on ordinals 0,3,6 then 9,12,15 (indices 1,4,7)
  --   then 18,21 — the figure WALKS. A per-cycle reset would restart it at
  --   index 0 every bar, which is wrong and completely silent.
  --
  --   Cycle 7 rendered OUT OF ORDER, after 0,1,2. Conspicillum derives each
  --   cycle's seed from the base seed and the cycle number instead of threading
  --   it, so any cycle can be computed directly — which is what the browser
  --   visualizer needs when the player drops into a set already running.
  assertEqual' "Conspicillum cloud golden (cycles 0,1,2,7 — cross-cycle Every + seeded Chance)"
    { actual: conspicillumCloudRun, expected: conspicillumCloudGolden }
  log "Reef Conspicillum cloud golden: OK"

  -- A cloud that FOLLOWS reads the tape at the rate the cycle plays it. Sixteen
  -- even grains a sixteenth long over a two-second take must begin at exactly
  -- i/16 — scaled by `room` the way the scan point is, the last slice would
  -- read a sixteenth early — and a `position` of a beat must wrap, so the
  -- grain on the last beat reads the first.
  let tape = { index: 0, secs: 2.0, peak: 1.0, rms: 0.5, zcr: 0.0, tilt: 0.0, decay: 0.0, cell: [], notes: [], params: [], hits: CC.noHits }
      followAt pos at = (CC.grainAt { sustain: 0.125, position: pos, spray: 0.0, follow: 1.0 } at tape (seedFrom 1)).grain.begin
      sixteenths = map (\k -> toNumber k / 16.0) (range 0 15)
  assertEqual' "Conspicillum follow: sixteen grains read the bar in order"
    { actual: map (followAt 0.0) sixteenths, expected: sixteenths }
  assertEqual' "Conspicillum follow: a beat of offset wraps"
    { actual: map (followAt 0.25) [ 0.0, 0.75, 0.8125 ], expected: [ 0.25, 0.0, 0.0625 ] }
  log "Reef Conspicillum follow: OK"

  -- And `OpShift` is how the tape is broken: one sixteenth told to read the one
  -- before it is a repeat, and the grains either side still read their own.
  let tapeCorpus = { name: "tape", samples: [ tape ] }
      shifted = map _.begin (CL.cycleOf tapeCorpus CC.emptyQuery
        { onsets: sixteenths
        , cloud: { sustain: 0.125, position: 0.0, spray: 0.0, follow: 1.0 }
        , walk: CL.noWalk, swing: CL.noSwing, tape: CL.oneBar, steps: CL.noSteps, warp: CL.noWarp, progression: CL.noProgression
        , rules: [ CL.rule (CL.Every 16 5) (CL.OpShift (-0.0625)) ]
        , speed: 1.0, gain: 1.0, pan: 0.5, accelerate: 0.0
        , fx: CL.noFx, chain: CL.noChain, sends: [] } 1 0)
  assertEqual' "Conspicillum shift: sixteenth 5 repeats sixteenth 4"
    { actual: shifted
    , expected: map (\k -> toNumber (if k == 5 then 4 else k) / 16.0) (range 0 15) }
  log "Reef Conspicillum shift: OK"

  -- Swing. Tape and play swing equal on a following cloud reproduce a swung
  -- tape exactly: every grain reads from where it lands, sized to its slot.
  -- A straight tape played swung lands on the swung grid, and the short
  -- offbeat slots cut their grains short rather than leak the next hit.
  let swungSpec tapeSw playSw =
        { onsets: sixteenths
        , cloud: { sustain: 0.125, position: 0.0, spray: 0.0, follow: 1.0 }
        , walk: CL.noWalk
        , swing: { tape: tapeSw, play: playSw, grid: 16 }
        , tape: CL.oneBar, steps: CL.noSteps, warp: CL.noWarp, progression: CL.noProgression
        , rules: []
        , speed: 1.0, gain: 1.0, pan: 0.5, accelerate: 0.0
        , fx: CL.noFx, chain: CL.noChain, sends: [] }
      same = CL.cycleOf tapeCorpus CC.emptyQuery (swungSpec 0.62 0.62) 1 0
      added = CL.cycleOf tapeCorpus CC.emptyQuery (swungSpec 0.5 0.62) 1 0
      r6 x = toNumber (Int.round (x * 1000000.0)) / 1000000.0
  assertEqual' "Conspicillum swing: equal tape and play swing read each grain where it lands"
    { actual: map (\e -> r6 e.begin) same, expected: map (\e -> r6 e.at) same }
  assertEqual' "Conspicillum swing: slots alternate long and short at 0.62"
    { actual: map (\e -> r6 e.sustain) (take 4 same), expected: [ 0.155, 0.095, 0.155, 0.095 ] }
  assertEqual' "Conspicillum swing: a straight tape gains swing — offbeat 16ths land late"
    { actual: map (\e -> r6 (e.at * 16.0)) (take 4 added), expected: [ 0.0, 1.24, 2.0, 3.24 ] }
  assertEqual' "Conspicillum swing: straight slices in swung slots last the shorter of the two"
    { actual: map (\e -> r6 e.sustain) (take 4 added), expected: [ 0.125, 0.095, 0.125, 0.095 ] }
  log "Reef Conspicillum swing: OK"

  -- A two-bar tape: each cycle reads its own bar, `order` re-lays them, and a
  -- certain step table is a permutation of the bar.
  let longTape = tape { secs = 4.0 }
      longCorpus = { name: "tape", samples: [ longTape ] }
      barSpec tp st =
        { onsets: sixteenths
        , cloud: { sustain: 0.125, position: 0.0, spray: 0.0, follow: 1.0 }
        , walk: CL.noWalk, swing: CL.noSwing, tape: tp, steps: st, warp: CL.noWarp, progression: CL.noProgression
        , rules: []
        , speed: 1.0, gain: 1.0, pan: 0.5, accelerate: 0.0
        , fx: CL.noFx, chain: CL.noChain, sends: [] }
      beginsAt tp st c = map (\e -> e.begin * 32.0) (CL.cycleOf longCorpus CC.emptyQuery (barSpec tp st) 1 c)
      heads c = map (\k -> toNumber (16 * c + k)) (range 0 15)
  assertEqual' "Conspicillum tape: a two-bar tape reads bar 2 on the second cycle"
    { actual: beginsAt { bars: 2, order: [], samples: [] } CL.noSteps 1, expected: heads 1 }
  assertEqual' "Conspicillum tape: order [1, 0] plays the bars swapped"
    { actual: beginsAt { bars: 2, order: [ 1, 0 ], samples: [] } CL.noSteps 0, expected: heads 1 }
  assertEqual' "Conspicillum steps: [2, 3, 0, 1] on a grid of 4 swaps the halves of the bar"
    { actual: map (\e -> e * 4.0) (everyFourth (map _.begin (CL.cycleOf tapeCorpus CC.emptyQuery
        (barSpec CL.oneBar { grid: 4, to: [ 2, 3, 0, 1 ], p: [] }) 1 0)))
    , expected: [ 2.0, 3.0, 0.0, 1.0 ] }
  log "Reef Conspicillum tape and steps: OK"

  -- Conspicillum C5: realising a progression onto RECORDED chord voicings.
  --
  -- A real corpus — actual Quadrat chord hits with the MIDI notes really
  -- struck — scored against each chord of a ii-V-i in D minor. The point is
  -- that the same corpus ranks DIFFERENTLY under each chord: Em7b5 picks the
  -- Gm(maj7) (three shared tones of four), A7 picks the A major by a mile, and
  -- Dm and Dm6 each pick themselves and rank the other second, so the scorer
  -- distinguishes a sixth.
  --
  -- The last two columns are the ones that matter most. Of the 136 chord hits
  -- recorded so far, 24 carry no notes and 7 are eleven-pitch-class smears, and
  -- a smear covers every chord perfectly on coverage alone. Here the smear
  -- never exceeds 149 and the empty hit is 0 throughout — if that ever stops
  -- being true, the instrument has started preferring its broken material.
  assertEqual' "Conspicillum harmonic golden (real chord hits under a ii-V-i)"
    { actual: conspicillumHarmonicRun, expected: conspicillumHarmonicGolden }
  log "Reef Conspicillum harmonic golden: OK"
  -- The chord-quantised render golden: a → odo Vetula feed turns the chord overlay
  -- on, then 32 steps render the SOUNDING pitch — pinning that the chord path realizes
  -- the cell index to a melodic pitch and snaps it to the nearest chord tone (sane
  -- octaves). Coverage for the octave bug that shipped because no golden rendered a
  -- chord-on pitch. cross-runtime.sh proves node == BEAM here too.
  assertEqual' "Odonus chord-quantised render golden (FollowChord + pitch, 32 steps)"
    { actual: chordRun, expected: chordGolden }
  log "Reef Odonus chord-quantised render golden: OK"
  -- Harmony as a Tidal pattern: the overlay follows what the host's sampler
  -- reads at each step (a stub here; Littorina in the hosts), rests on the
  -- scale, and lets go when cleared. SetHarmony crosses the wire codec.
  assertEqual' "Odonus harmony golden (followHarmony, 40 steps)"
    { actual: harmonyRun, expected: harmonyGolden }
  log "Reef Odonus harmony golden: OK"
  -- Heads by pattern: which heads play, sampled each step (Reef.Odonus.followHeads).
  assertEqual' "Odonus heads-pattern golden (followHeads, 80 steps)"
    { actual: headsRun, expected: headsGolden }
  log "Reef Odonus heads-pattern golden: OK"
  -- How a voice's notes are played down every kind of leg (Reef.Articulation).
  assertEqual' "Articulation golden (legato, slides, ES-9 lines, triggers, Rample)"
    { actual: articulationRun, expected: articulationGolden }
  log "Reef articulation golden: OK"
  -- Scales by name: the scale follows the host-sampled pattern of names,
  -- holds through a rest, yields to a hand on the scale, and lets go back
  -- to the authored scale. SetScalePattern crosses the wire codec.
  assertEqual' "Odonus scale-pattern golden (followScale, 40 steps)"
    { actual: scaleRun, expected: scaleGolden }
  log "Reef Odonus scale-pattern golden: OK"
  -- Protocol round-trip: encode -> decode -> re-encode must reproduce the
  -- original JSON, proving the wire codec is faithful for the full Odonus
  -- record. (Odonus has no Show, so we compare the canonical JSON form.)
  let json = encodeOdonus defaultOdonus
  case decodeOdonus json of
    Left errs ->
      assertEqual' "Protocol decode of encoded defaultOdonus must succeed"
        { actual: "decode error: " <> show errs, expected: "Right _" }
    Right odo ->
      assertEqual' "Protocol round-trip (decode . encode is faithful)"
        { actual: encodeOdonus odo, expected: json }
  log "Reef.Protocol round-trip: OK"
  -- The frozen quantisation table (project_reef_quantisation_realize). Pins the
  -- agreed PitchSet examples: literal offsets, flat-equal mapping, finite vs
  -- periodic, octave vs non-octave (period 19). Row 3 is the "don't lop the 9th"
  -- proof — index 4 realizes to D4=62, the high ninth, not a folded base-octave D.
  assertEqual' "PitchSet quantisation table golden"
    { actual: tableRender, expected: pitchSetGolden }
  log "Reef.PitchSet quantisation table golden: OK"
  -- The lockstep determinism net (P1): the long generative run with 10 sources
  -- active, sampled over 2000 steps. The cross-runtime script proves the BEAM
  -- produces the same bytes — that identity is what makes lockstep co-simulation
  -- sound (see reef/docs/PLAN-lockstep-cosimulation.md).
  assertEqual' "Odonus generative determinism golden (10 sources, 2000 steps)"
    { actual: genRun, expected: genGolden }
  log "Reef generative determinism golden: OK"
  -- The Beta/pow diagnostic: the one transcendental, isolated. node == BEAM here
  -- means V8 Math.pow and BEAM math:pow agree, so the GNotes path is safe too.
  assertEqual' "Marbles Beta (pow) probe golden"
    { actual: betaProbe, expected: betaGolden }
  log "Reef Beta/pow probe golden: OK"
  -- The input-protocol determinism net (P3): a scripted tick-tagged stream of
  -- user actions (every Input family + the seed-threading rolls), each round-tripped
  -- through the Reef.Protocol codec, interleaved with the autonomous gen sources and
  -- stepEmit over 400 steps. The cross-runtime script proves the BEAM produces the
  -- same bytes — so the input protocol AND its codec behave identically on both
  -- runtimes, which is what lets the rig apply the frontend's tick-tagged inputs to
  -- the same state (reef/docs/PLAN-lockstep-cosimulation.md, P3).
  assertEqual' "Odonus input-protocol determinism golden (lockstep replay, 400 steps)"
    { actual: inputRun, expected: inputGolden }
  log "Reef input-protocol determinism golden: OK"
  -- The SimState handoff net (P4d): decode a real handoff JSON (a SimState the JS
  -- frontend encoded, gen on + a seed) and step it. The cross-runtime script proves
  -- the BEAM's decodeSim rebuilds exactly the state the browser encoded and evolves
  -- it identically — the lockstep handoff lands the rig on the frontend's state.
  assertEqual' "Odonus SimState handoff golden (decodeSim + step, 400 steps)"
    { actual: simRun, expected: simGolden }
  log "Reef SimState handoff golden: OK"
  -- The Balistes engine determinism net (Phase 0 of the Balistes lockstep). The
  -- shared `Reef.Balistes.Engine` (Grids core) driven over 256 steps: X/Y swept
  -- across the whole drum-map grid (bilinear interp + tables), perturbations
  -- resampled every 32 steps threading one RNG seed through `Reef.Bits.xorshift32`,
  -- and the trigger/accent rule. The cross-runtime script proves the BEAM produces
  -- the same bytes — so `balistes_voice` can run onto `reef_balistes_engine@ps` and
  -- co-simulate the frontend byte-for-byte, retiring the two divergent hand-ports.
  assertEqual' "Balistes engine determinism golden (Grids core, 256 steps)"
    { actual: balistesRun, expected: balistesGolden }
  log "Reef Balistes engine determinism golden: OK"
  -- The Balistes handoff + shared-render net (P1 of the Balistes lockstep). A full
  -- BalSim (engine + overlay) round-tripped through the codec, then 128 steps of
  -- stepBal digested by what renderStep (open-hat / note / Dilla-push / ratchet /
  -- accent) emits. The cross-runtime script proves the BEAM produces the same bytes,
  -- so reef_balistes_voice co-simulates the frontend from a pushed handoff.
  assertEqual' "Balistes handoff + render golden (codec + sim, 128 steps)"
    { actual: balistesSimRun, expected: balistesSimGolden }
  log "Reef Balistes handoff + render golden: OK"
  -- The Balistes live-input net (knob/gesture lockstep). A scripted tick-tagged
  -- session (every BInput family, incl. BReseed/BReset) round-tripped through the
  -- codec, interleaved with stepBal + renderStep over 200 steps. The cross-runtime
  -- script proves the BEAM produces the same bytes — so the frontend can broadcast
  -- tick-tagged knob edits and the rig applies them to the same model step.
  assertEqual' "Balistes input-protocol golden (codec + apply, 200 steps)"
    { actual: balistesInputRun, expected: balistesInputGolden }
  log "Reef Balistes input-protocol golden: OK"
  -- The Balistes fixed-rhythm net. A representative hand-written pattern (conditions,
  -- probabilities, ratchets) round-tripped through the codec and rendered by the
  -- shared renderFixed over 8 loops. The cross-runtime script proves the BEAM
  -- produces the same bytes, so reef_balistes_voice plays a pushed fixed rhythm in
  -- lockstep — the AFixed mode, not just Grids.
  assertEqual' "Balistes fixed-rhythm golden (codec + renderFixed, 128 steps)"
    { actual: fixedRun, expected: fixedGolden }
  log "Reef Balistes fixed-rhythm golden: OK"
  -- The drum kit's lane names, as a Tidal `drums $ s "…"` line addresses them.
  assertEqual' "Balistes kit: lanes by name and Dirt-Samples alias"
    { actual: map laneOfName [ "bd", "BD", "sn", "sd", "hh", "oh", "rim", "cb", "arpy" ]
    , expected: [ Just 0, Just 0, Just 1, Just 1, Just 4, Just 6, Just 3, Just 13, Nothing ] }
  log "Reef Balistes kit names: OK"
  -- The output's scale (odonus.out): sampled into SetSampled's chord on its own
  -- root, across the wire; harmony takes the output from it and back.
  assertEqual' "Odonus outScale golden (sample + wire + q2, 36 steps)"
    { actual: outScaleRun, expected: outScaleGolden }
  log "Reef Odonus outScale golden: OK"
  -- The duration clock: a head held on each cell for its dur (AC, 2026-10-03).
  assertEqual' "Odonus duration clock golden (56 steps)"
    { actual: durRun, expected: durGolden }
  log "Reef Odonus duration clock golden: OK"
  -- Chords as voiced: a chord's own set, the output snapping to it, the grid
  -- shaped by it (docs/kb/plans/harmony-routes-coherent.md).
  assertEqual' "Chords as voiced golden (Reef.Conformance.voicingRun)"
    { actual: voicingRun, expected: voicingGolden }
  log "Reef chords as voiced golden: OK"
  -- Harmony routes: parse, print, and the inputs a change makes.
  assertEqual' "Harmony routes golden (Reef.Route)"
    { actual: routeRun, expected: routeGolden }
  log "Reef harmony routes golden: OK"
  -- A voice's routing: every leg of the voice carries the note it is given.
  let
    vr = { voices: [ [ { port: "IAC Driver Tidal", channel: 1, note: -1, offsetMs: 0.0, rample: [], line: true }
                     , { port: "FH-2", channel: 1, note: -1, offsetMs: 2.0, rample: [], line: false } ]
                   , [] ]
         , lines: [], polys: [], samplers: [] }
    hit = { note: 60, velocity: 100, atMs: 0.0, durMs: 120.0, stepMs: 125.0 }
  assert' "Voice routing: both legs of voice 0, nothing from voice 1"
    ( Routing.voiceRoutingSends vr 0 hit ==
        [ Routing.Note { port: "IAC Driver Tidal", channel: 1, note: 60, velocity: 100, atMs: 0.0, durMs: 120.0 }
        , Routing.Note { port: "FH-2", channel: 1, note: 60, velocity: 100, atMs: 2.0, durMs: 120.0 } ]
        && Routing.voiceRoutingSends vr 1 hit == [] )
  assertEqual' "Voice routing round-trips its codec"
    { actual: map Routing.encodeVoiceRouting (hush (Routing.decodeVoiceRouting (Routing.encodeVoiceRouting vr)))
    , expected: Just (Routing.encodeVoiceRouting vr) }
  -- A routing written by a page from before legs said whether they are lines
  -- (a tab left open across the change) still reads: a voice leg is a line,
  -- a drum leg a trigger, and there are no ES-9 lines or instruments.
  let oldLeg = """{"rample":[],"port":"IAC Driver Tidal","offsetMs":0,"note":-1,"channel":1}"""
  assertEqual' "An older page's voice routing still decodes"
    { actual: map (\r -> { line: map _.line (join r.voices), extra: length r.lines + length r.polys + length r.samplers })
                (hush (Routing.decodeVoiceRouting ("{\"voices\":[[" <> oldLeg <> "]]}")))
    , expected: Just { line: [ true ], extra: 0 } }
  assertEqual' "An older page's drum routing still decodes"
    { actual: map (\r -> map _.line (join r.lanes))
                (hush (Routing.decodeDrumRouting ("{\"notes\":[36],\"lanes\":[[" <> oldLeg <> "]],\"voices\":[[]]}")))
    , expected: Just [ false ] }
  log "Reef voice routing: OK"
  -- The Vetula performance-scheduler net (Vetula lockstep V1). A representative
  -- performance (one progression fanned to voices with different per-chord dwell
  -- schedules + skips + phase offsets) round-tripped through Reef.Vetula.Protocol,
  -- then evaluated at each absolute pulse over 128 pulses: every voice's read-head
  -- plus the → odo conductor's held cursor + the pitch-class set it feeds Odonus.
  -- The cross-runtime script proves the BEAM produces the same bytes — so
  -- reef_vetula_voice conducts the rig's Odonus in lockstep with the browser.
  assertEqual' "Vetula performance-scheduler golden (codec + scheduler, 128 pulses)"
    { actual: vetulaRun, expected: vetulaGolden }
  log "Reef Vetula performance-scheduler golden: OK"
  -- The Vetula MIDI-render net (V2a): the shared renderVoiceMidiAt (block + arp)
  -- evaluated per pulse over the same performance. cross-runtime.sh proves node == BEAM,
  -- so reef_vetula_voice emits the same notes the browser's stepVoice does.
  assertEqual' "Vetula MIDI-render golden (block + arp, 128 pulses)"
    { actual: vetulaMidiRun, expected: vetulaMidiGolden }
  log "Reef Vetula MIDI-render golden: OK"
  -- an index past the progression's end wraps: a stream of itself (D3)
  assertEqual' "Vetula sequence indices wrap"
    { actual: map (wrapAt [ 10, 11, 12 ]) [ 0, 3, 4, -1, 7 ], expected: map Just [ 10, 10, 11, 12, 11 ] }
  log "Reef Vetula index wrap: OK"

-- | The frozen render of `Reef.Conformance.run`. Head 0 walks the default
-- | PitchSet (C minor, cells = discrete indices) — a clean ascending two-octave
-- | scale; octaves emerge from the set's period tiling (index 7 -> 72 = C5), not
-- | a +12. Regenerated 2026-06-30 when renderCell moved to index-space realize;
-- | identical under node and the BEAM.
golden :: String
golden = """  1 | h0 p50 d1 r1 v100
  2 | h0 p51 d1 r1 v100
  3 | h0 p55 d1 r1 v100
  4 | h0 p56 d1 r1 v100
  5 | h0 p58 d1 r1 v100
  6 | h0 p62 d1 r1 v100
  7 | h0 p63 d1 r1 v100
  8 | h0 p67 d1 r1 v100
  9 | h0 p68 d1 r1 v100
 10 | h0 p70 d1 r1 v100
 11 | h0 p74 d1 r1 v100
 12 | h0 p75 d1 r1 v100
 13 | h0 p79 d1 r1 v100
 14 | h0 p80 d1 r1 v100
 15 | h0 p82 d1 r1 v100
 16 | h0 p48 d1 r1 v100
 17 | h0 p50 d1 r1 v100
 18 | h0 p51 d1 r1 v100
 19 | h0 p55 d1 r1 v100
 20 | h0 p56 d1 r1 v100
 21 | h0 p58 d1 r1 v100
 22 | h0 p62 d1 r1 v100
 23 | h0 p63 d1 r1 v100
 24 | h0 p67 d1 r1 v100
 25 | h0 p68 d1 r1 v100
 26 | h0 p70 d1 r1 v100
 27 | h0 p74 d1 r1 v100
 28 | h0 p75 d1 r1 v100
 29 | h0 p79 d1 r1 v100
 30 | h0 p80 d1 r1 v100
 31 | h0 p82 d1 r1 v100
 32 | h0 p48 d1 r1 v100"""

-- | The frozen quantisation table — agreed with AC 2026-06-30. Captured from
-- | Reef.PitchSetGolden.tableRender; identical under node and the BEAM.
pitchSetGolden :: String
pitchSetGolden = """1 | C pentatonic | 1 octave | period 12 | span 1
  i0  0-19  -> 48 C3
  i1  20-39  -> 50 D3
  i2  40-59  -> 52 E3
  i3  60-79  -> 55 G3
  i4  80-99  -> 57 A3

2 | C pentatonic | 3 octaves | period 12 | span 3 (flat-equal over 15)
  i0  0-6  -> 36 C2
  i1  7-13  -> 38 D2
  i2  14-19  -> 40 E2
  i3  20-26  -> 43 G2
  i4  27-33  -> 45 A2
  i5  34-39  -> 48 C3
  i6  40-46  -> 50 D3
  i7  47-53  -> 52 E3
  i8  54-59  -> 55 G3
  i9  60-66  -> 57 A3
  i10  67-73  -> 60 C4
  i11  74-79  -> 62 D4
  i12  80-86  -> 64 E4
  i13  87-93  -> 67 G4
  i14  94-99  -> 69 A4

3 | extended chord | finite (don't lop the 9th)
  i0  0-12  -> 36 C2
  i1  13-24  -> 43 G2
  i2  25-37  -> 52 E3
  i3  38-49  -> 59 B3
  i4  50-62  -> 62 D4
  i5  63-74  -> 66 F#4
  i6  75-87  -> 69 A4
  i7  88-99  -> 72 C5

4 | exotic | period 19 | span 2 (non-octave repetition)
  i0  0-6  -> 36 C2
  i1  7-12  -> 39 D#2
  i2  13-18  -> 41 F2
  i3  19-24  -> 43 G2
  i4  25-31  -> 46 A#2
  i5  32-37  -> 48 C3
  i6  38-43  -> 51 D#3
  i7  44-49  -> 53 F3
  i8  50-56  -> 55 G3
  i9  57-62  -> 58 A#3
  i10  63-68  -> 60 C4
  i11  69-74  -> 62 D4
  i12  75-81  -> 65 F4
  i13  82-87  -> 67 G4
  i14  88-93  -> 70 A#4
  i15  94-99  -> 72 C5"""

-- | The frozen LONG generative determinism golden (P1, the lockstep safety
-- | net). `genRun` threads runGen (10 non-pow sources active) then stepEmit for
-- | 2000 steps from a fixed seed, sampling the full evolving state every 25
-- | steps. Proven BYTE-IDENTICAL under node and the BEAM (conformance/
-- | cross-runtime.sh) on 2026-06-30 — generation runs identically on both
-- | runtimes, the precondition for deterministic lockstep co-simulation.
genGolden :: String
genGolden = """  25 | k0 [0,2,3,5,7,8,10] s524102504 | 0/1/1/1,17/1/1/1,34/1/1/1,51/1/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/1/1/1,153/1/1/1,170/1/1/1,187/1/1/1,204/1/1/1,221/1/1/1,238/1/1/1,255/1/1/1 | 9:9:0:6:0:16  2:8:7:4:2:16m  10:14:-12:8:3:16m  14:11:3:5:2:16m
  50 | k0 [0,2,4,6,7,9,10] s1528073014 | 0/1/1/1,17/1/1/1,34/1/1/1,51/1/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/1/1/1,153/1/1/1,170/1/1/1,187/1/1/1,204/1/1/1,221/1/1/1,238/1/1/1,255/1/1/1 | 2:2:0:6:0:16  1:4:7:4:2:16m  5:12:-12:8:3:16m  2:8:3:5:2:16m
  75 | k0 [0,2,4,6,7,9,10] s778248382 | 0/1/1/1,17/1/1/1,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/1/1/1,153/1/1/1,170/1/1/1,187/1/1/1,204/1/1/1,221/1/1/1,238/1/1/1,255/1/1/1 | 11:11:0:6:0:16  6:13:7:4:3:16m  8:10:-12:8:3:16m  12:3:3:5:2:16m
 100 | k0 [0,2,4,6,7,9,10] s1949588295 | 0/1/1/1,17/1/1/1,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/1/1/1,153/1/1/1,170/1/1/1,187/1/1/1,204/1/1/0,221/1/1/1,238/1/1/1,255/1/1/1 | 4:4:0:6:0:16  10:11:7:4:4:16m  7:10:-12:8:4:16m  15:15:3:5:2:16m
 125 | k7 [0,2,4,6,7,9,10] s1977694286 | 0/1/1/1,17/1/1/1,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/1/1/1,153/1/1/1,170/1/1/1,187/1/4/1,204/1/1/0,221/1/1/1,238/1/1/1,255/1/1/1 | 13:13:0:6:0:16  9:9:7:4:0:16m  9:8:-12:8:4:16m  1:4:3:5:2:16m
 150 | k7 [0,2,4,6,7,9,10] s177376893 | 0/1/1/1,17/1/1/1,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/1/1/1,153/1/1/1,170/1/1/1,187/1/4/1,204/1/1/0,221/1/1/1,238/1/1/1,255/1/1/1 | 6:6:0:6:0:16  5:5:7:4:0:16m  3:6:-12:8:4:16m  6:13:3:5:3:16m
 175 | k2 [0,2,4,6,7,9,10] s618154670 | 0/1/1/1,17/1/1/0,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/1/1/1,153/1/1/1,170/1/1/5,187/1/4/1,204/1/1/0,221/1/1/1,238/1/1/1,255/1/1/1 | 15:15:0:6:0:16  2:2:7:4:0:16m  5:4:-12:8:4:16m  2:2:3:5:3:16m
 200 | k2 [0,2,4,6,7,9,10] s420725881 | 0/1/1/1,17/3/1/0,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/1/1/1,153/1/1/1,170/1/1/5,187/1/4/1,204/1/1/0,221/1/1/1,238/1/1/1,255/1/1/1 | 8:8:-5:6:0:16  14:14:7:4:0:16m  4:2:-12:8:4:16m  8:10:3:5:3:16m
 225 | k2 [0,2,4,6,7,9,10] s2147215785 | 0/1/1/1,17/3/1/0,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/1/1/1,153/1/1/1,170/1/1/5,187/1/4/1,204/3/1/0,221/3/1/1,238/1/1/1,255/1/1/1 | 1:1:-5:6:0:16  11:11:7:4:0:16m  0:0:-12:8:4:16m  12:9:3:5:3:16m
 250 | k2 [0,2,4,6,7,9,10] s1745596560 | 0/1/1/1,17/3/1/0,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/1/1/1,153/1/1/1,170/1/1/5,187/1/4/1,204/3/1/0,221/1/1/1,238/1/1/1,255/1/1/1 | 7:4:-10:6:1:16  7:7:7:4:0:16m  14:14:-12:8:4:16m  2:2:3:5:3:16m
 275 | k9 [0,2,4,6,7,9,10] s1187130808 | 0/1/1/1,17/3/1/0,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/1/1/1,153/1/1/1,170/1/1/5,187/1/4/1,204/3/1/0,221/1/1/1,238/1/1/1,255/1/1/1 | 14:13:-10:6:1:16  4:4:2:4:0:16  13:12:-12:8:4:16m  13:13:3:5:0:16
 300 | k9 [0,2,4,6,7,9,10] s1870942094 | 0/1/1/1,17/3/1/0,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/1/1/1,153/1/1/1,170/1/1/1,187/1/4/1,204/3/1/0,221/1/1/1,238/1/1/1,255/1/1/1 | 5:6:-10:6:1:16  0:0:2:4:0:16  7:10:-12:8:4:16m  3:3:3:3:0:16
 325 | k9 [0,2,4,6,7,9,10] s1773755621 | 0/1/1/1,17/3/1/0,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/1/1/1,153/1/1/1,170/1/1/1,187/1/4/1,204/3/1/0,221/1/1/1,238/1/1/1,255/1/1/1 | 14:13:-10:4:1:16  13:13:2:4:0:16  9:8:-12:8:4:16m  5:5:3:3:0:16
 350 | k9 [0,2,4,6,7,9,10] s2129491709 | 0/1/1/1,17/3/1/0,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/1/1/1,153/1/1/1,170/1/1/1,187/1/4/1,204/3/1/0,221/1/1/1,238/1/1/1,255/1/1/1 | 10:10:-10:4:1:16  9:9:2:4:0:16  3:6:-12:8:4:16m  13:13:3:3:0:16
 375 | k9 [0,2,4,6,7,9,10] s263717071 | 0/1/1/1,17/3/1/0,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/3/1/1,153/1/1/1,170/1/1/1,187/1/4/0,204/3/1/0,221/1/1/1,238/1/1/1,255/1/1/1 | 5:6:-10:4:1:16m  6:6:2:4:0:16m  5:4:-12:8:4:16m  8:8:3:3:0:16
 400 | k9 [0,2,3,5,7,8,10] s1580386395 | 0/1/1/1,17/3/1/0,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/3/1/1,153/1/1/1,170/1/1/1,187/1/4/0,204/3/1/0,221/1/1/1,238/1/1/1,255/1/1/1 | 3:3:-10:4:1:16m  2:2:2:4:0:16m  4:2:-12:8:4:16m  0:0:3:3:0:16
 425 | k9 [0,2,3,5,7,8,10] s1979075876 | 0/1/1/1,17/3/1/0,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/3/3/1,119/1/1/1,136/3/1/1,153/1/1/1,170/1/1/1,187/1/4/0,204/3/1/0,221/1/1/1,238/1/1/1,255/1/1/1 | 12:15:-10:4:1:16m  15:15:2:4:0:16m  0:0:-12:8:4:16m  8:8:3:3:0:16
 450 | k4 [0,2,3,5,7,8,10] s537857114 | 0/1/1/1,17/3/1/0,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/3/3/1,119/1/1/1,136/3/1/1,153/1/1/1,170/1/1/1,187/1/4/0,204/3/1/2,221/1/1/1,238/1/1/1,255/1/1/1 | 15:12:-10:4:1:16m  11:11:2:4:0:16m  11:13:-12:8:4:16m  13:13:3:3:0:16
 475 | k4 [0,2,3,5,7,8,10] s1288669851 | 0/1/1/1,17/3/1/0,34/1/1/1,51/3/1/1,68/1/1/1,85/1/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/1/1/1,170/1/1/1,187/1/4/0,204/3/1/2,221/1/1/1,238/1/1/1,255/1/1/1 | 9:9:-10:4:1:16m  9:9:2:4:0:16m  6:7:-12:8:4:16m  7:7:3:1:0:16
 500 | k4 [0,2,4,5,7,9,10] s1866879101 | 0/1/1/1,17/3/1/0,34/1/1/1,51/5/1/1,68/1/1/1,85/1/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/3/1/1,170/1/1/1,187/1/4/0,204/3/1/2,221/1/1/1,238/1/1/1,255/1/1/1 | 4:7:-10:4:1:16m  6:6:2:4:0:16m  4:2:-12:8:4:16m  3:3:3:1:0:16
 525 | k4 [0,2,4,5,7,9,10] s395373243 | 0/1/1/1,17/3/1/0,34/1/1/1,51/5/1/1,68/1/1/1,85/1/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/3/1/1,170/1/1/1,187/1/4/0,204/3/1/2,221/1/1/1,238/1/1/1,255/1/1/1 | 7:4:-10:4:1:16m  4:4:2:4:0:16m  11:13:-12:8:4:16m  1:1:3:1:0:16
 550 | k4 [0,2,4,5,7,9,10] s1670926612 | 0/1/1/1,17/3/1/0,34/1/1/1,51/5/1/1,68/3/1/1,85/1/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/3/1/1,170/1/1/1,187/1/4/0,204/3/1/2,221/1/1/1,238/1/1/1,255/1/1/1 | 2:2:-10:4:1:16m  1:1:2:4:0:16m  6:7:-12:8:4:16m  5:5:3:1:0:16
 575 | k4 [0,2,4,5,7,9,10] s539267748 | 0/1/1/1,17/3/1/0,34/1/1/1,51/5/1/1,68/3/1/1,85/1/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/3/1/1,170/1/1/1,187/1/4/0,204/3/1/2,221/1/1/1,238/1/1/1,255/1/1/1 | 13:14:-10:4:1:16m  15:15:2:4:0:16m  4:2:-12:8:4:16m  9:9:3:1:0:16
 600 | k4 [0,2,4,5,7,9,10] s1776360416 | 0/1/1/1,17/3/1/0,34/1/1/1,51/5/1/1,68/3/1/1,85/1/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/3/1/1,170/1/1/1,187/1/4/0,204/3/1/2,221/1/1/1,238/3/1/1,255/1/1/1 | 9:9:-5:6:1:16m  11:11:2:4:0:16m  11:13:-12:8:4:16m  15:15:3:1:0:16
 625 | k4 [0,2,4,5,7,9,10] s692213941 | 0/1/1/1,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/1/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/3/1/1,170/1/1/1,187/1/4/0,204/3/1/2,221/1/1/1,238/3/1/1,255/1/1/1 | 7:4:-5:6:1:16  5:5:2:6:0:16  6:7:-12:8:4:16m  1:1:3:1:1:16
 650 | k4 [0,2,4,5,7,9,10] s678791525 | 0/1/1/1,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/3/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/3/1/1,170/1/1/1,187/1/4/0,204/3/1/2,221/1/1/1,238/3/1/1,255/1/1/1 | 13:14:-5:6:1:16  0:0:2:6:0:16  4:2:-12:8:4:16m  6:5:3:1:1:16
 675 | k4 [0,2,4,5,7,9,10] s1932288229 | 0/1/1/1,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/3/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/3/1/1,170/1/1/1,187/1/4/0,204/3/1/2,221/1/1/1,238/3/1/1,255/1/1/1 | 9:9:-5:6:1:16  10:10:2:6:0:16m  11:13:-12:8:4:16m  9:9:3:1:1:16m
 700 | k4 [0,2,4,5,7,9,10] s437444698 | 0/1/1/1,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/3/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/3/1/0,170/1/1/1,187/1/4/0,204/3/1/2,221/1/1/1,238/3/1/1,255/1/1/1 | 7:4:-5:6:1:16  5:5:7:6:0:16m  2:3:-12:8:4:16m  14:13:3:1:1:16m
 725 | k4 [0,2,4,6,8,10] s349785721 | 0/1/1/1,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/3/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/3/1/0,170/1/1/1,187/1/4/0,204/3/1/2,221/1/1/1,238/3/1/1,255/1/1/1 | 13:14:-5:6:1:16  0:0:7:6:0:16m  14:14:-12:8:4:16m  2:2:3:1:1:16m
 750 | k4 [0,2,4,6,8,10] s1905727072 | 0/1/1/1,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/3/1/0,170/3/1/1,187/3/4/0,204/3/1/2,221/1/1/1,238/3/1/1,255/1/1/1 | 9:9:-5:6:1:16  8:8:2:8:0:16m  9:8:-12:8:4:16m  4:7:3:1:1:16m
 775 | k4 [0,2,4,6,8,10] s1530768787 | 0/1/1/1,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/3/1/0,170/3/1/1,187/3/4/0,204/3/1/2,221/1/1/1,238/3/1/1,255/1/1/1 | 7:4:-5:6:1:16  14:14:2:8:0:16m  2:3:-12:8:4:16m  11:11:3:1:1:16m
 800 | k4 [0,2,4,6,8,10] s1457710456 | 0/1/1/1,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/3/1/0,170/3/1/1,187/3/4/0,204/3/1/2,221/1/1/1,238/3/1/1,255/1/1/1 | 13:14:-5:6:1:16  3:3:2:8:0:16m  14:14:-12:8:4:16m  0:0:3:1:1:16m
 825 | k4 [0,2,4,6,8,10] s864344555 | 0/1/1/1,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/3/1/0,170/3/1/1,187/5/4/0,204/3/1/2,221/1/1/1,238/3/1/1,255/1/1/1 | 9:9:-5:6:1:16  8:8:2:8:0:16m  9:8:-12:8:4:16m  7:4:3:1:1:16m
 850 | k4 [0,2,4,6,8,10] s2032322566 | 0/1/1/1,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/5/3/1,119/1/1/1,136/3/1/1,153/3/1/0,170/3/1/1,187/5/4/0,204/3/1/2,221/1/1/1,238/3/1/1,255/1/1/1 | 7:4:-5:6:1:16  14:14:2:8:0:16m  2:3:-12:8:4:16m  8:8:3:1:1:16m
 875 | k4 [0,2,4,6,8,10] s464218769 | 0/1/1/1,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/5/1/1,119/1/1/1,136/3/1/1,153/3/1/0,170/3/1/1,187/5/4/0,204/3/1/2,221/1/1/1,238/3/1/1,255/1/1/1 | 13:14:0:6:1:16  3:3:2:8:0:16m  14:14:-12:8:4:16m  15:12:3:1:1:16m
 900 | k4 [0,2,4,6,8,10] s1114926878 | 0/1/1/1,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/5/1/1,119/1/1/1,136/3/1/1,153/3/1/0,170/3/1/1,187/5/4/0,204/3/1/0,221/1/1/1,238/3/1/1,255/1/1/1 | 9:9:0:6:1:16  7:7:2:8:0:16  12:9:-12:8:4:16m  5:5:3:1:2:16
 925 | k4 [0,2,4,6,8,10] s2020019372 | 0/1/1/1,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/5/1/1,119/1/1/1,136/3/1/1,153/5/1/0,170/3/1/1,187/5/4/0,204/3/1/0,221/1/1/1,238/3/1/1,255/1/1/1 | 2:2:0:6:1:16  9:9:2:8:0:16  6:7:-12:8:4:16m  6:9:3:1:2:16
 950 | k2 [0,2,4,6,8,10] s1878312858 | 0/1/1/1,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/5/1/1,119/1/1/1,136/3/1/1,153/5/1/1,170/3/1/1,187/5/4/0,204/3/1/0,221/1/1/1,238/3/1/1,255/1/1/1 | 11:11:-5:6:1:16  11:11:2:8:0:16  8:5:-12:8:4:16m  7:13:3:1:2:16
 975 | k2 [0,2,4,6,8,10] s2133799190 | 0/1/1/1,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/3/1/1,119/1/1/1,136/3/1/1,153/5/1/1,170/3/1/1,187/3/4/0,204/3/1/0,221/3/1/1,238/3/1/1,255/1/1/1 | 1:4:-5:6:2:16m  13:13:2:6:0:16  2:3:-12:8:4:16m  7:13:3:1:2:16m
1000 | k2 [0,2,4,6,8,10] s1697839348 | 0/1/1/3,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/3/1/1,119/1/1/1,136/3/1/1,153/5/1/1,170/3/1/1,187/3/4/0,204/5/1/0,221/3/1/1,238/3/1/1,255/1/1/1 | 7:13:-5:6:2:16m  6:6:2:6:0:16  1:1:-12:8:4:16m  6:9:3:1:2:16m
1025 | k2 [0,2,4,6,8,10] s285456031 | 0/1/1/3,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/3/1/1,119/1/1/1,136/3/1/1,153/5/1/1,170/3/1/1,187/3/4/0,204/5/1/1,221/3/1/1,238/3/1/1,255/1/1/1 | 2:8:-5:6:2:16m  1:1:2:6:0:16  10:11:-12:8:4:16m  5:5:3:1:2:16m
1050 | k2 [0,2,4,6,8,10] s1757586176 | 0/1/1/3,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/3/1/1,119/1/1/1,136/3/1/1,153/5/1/1,170/3/1/1,187/5/4/0,204/3/1/1,221/5/1/1,238/3/1/1,255/1/1/1 | 12:3:-5:6:2:16m  11:11:2:6:0:16  3:6:-12:8:4:16m  15:15:3:1:2:16m
1075 | k2 [0,2,4,6,8,10] s1694771291 | 0/1/1/3,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/1/1/1,119/1/1/1,136/3/1/1,153/5/1/1,170/3/1/1,187/5/4/0,204/3/1/1,221/5/1/1,238/3/1/1,255/3/1/1 | 7:13:-5:6:2:16m  6:6:2:6:0:16  4:4:-12:8:0:16m  14:11:3:1:2:16m
1100 | k2 [0,2,4,6,8,10] s846628748 | 0/1/1/3,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/1/1/1,119/1/1/1,136/3/1/1,153/5/1/1,170/3/1/1,187/5/4/0,204/5/1/1,221/3/1/1,238/3/1/1,255/3/1/1 | 2:8:-5:6:2:16m  1:1:2:6:0:16  14:14:-12:8:0:16m  13:7:3:1:2:16m
1125 | k2 [0,2,4,6,8,10] s1098519432 | 0/1/1/3,17/3/1/1,34/1/1/1,51/5/1/1,68/3/1/1,85/5/1/1,102/1/4/1,119/1/1/1,136/3/1/1,153/5/1/1,170/3/1/1,187/5/4/0,204/5/1/1,221/3/1/1,238/1/1/1,255/3/1/1 | 12:3:-5:6:2:16m  11:11:2:6:0:16  9:9:-12:8:0:16m  12:3:3:1:2:16m
1150 | k9 [0,2,4,6,8,10] s1972526773 | 0/1/1/3,17/3/1/1,34/3/1/1,51/5/1/1,68/1/1/1,85/5/1/1,102/1/4/1,119/1/1/1,136/3/1/1,153/5/1/1,170/3/1/1,187/5/4/0,204/5/1/1,221/3/1/1,238/1/1/1,255/3/2/1 | 7:13:-5:6:2:16m  6:6:2:6:0:16  4:4:-12:8:0:16m  11:14:3:1:2:16m
1175 | k9 [0,2,4,6,8,10] s1915847447 | 0/1/1/3,17/3/1/1,34/3/1/1,51/8/1/1,68/1/1/1,85/5/1/1,102/1/4/1,119/1/1/1,136/3/1/1,153/5/1/1,170/3/1/1,187/5/4/0,204/5/1/1,221/3/1/1,238/1/1/1,255/1/2/1 | 2:8:-5:6:2:16m  1:1:-3:6:0:16  14:14:-12:8:0:16m  10:10:3:1:2:16m
1200 | k9 [0,2,4,6,8,10] s143350733 | 0/1/1/3,17/1/1/1,34/3/1/1,51/8/1/1,68/1/1/1,85/5/1/1,102/1/4/1,119/1/1/1,136/3/1/1,153/5/1/1,170/3/1/1,187/5/4/0,204/5/1/1,221/3/1/1,238/1/1/1,255/1/2/1 | 12:3:-5:6:2:16  11:11:-3:6:0:16m  9:9:-17:8:0:16m  5:5:3:1:2:16m
1225 | k9 [0,2,4,6,8,10] s873291713 | 0/1/1/3,17/1/1/1,34/3/1/1,51/8/1/1,68/1/1/1,85/5/1/1,102/1/4/1,119/1/1/1,136/3/1/1,153/5/1/1,170/3/1/1,187/5/4/0,204/5/1/1,221/3/1/1,238/1/1/1,255/1/2/1 | 7:13:-5:6:2:16  6:6:-3:6:0:16m  4:4:-17:8:0:16m  2:2:3:1:3:16m
1250 | k9 [0,2,4,6,8,10] s1269132322 | 0/1/1/3,17/1/1/1,34/3/1/1,51/8/1/1,68/1/1/1,85/5/1/1,102/1/4/1,119/1/1/1,136/3/1/1,153/3/1/1,170/3/1/1,187/5/4/0,204/5/1/1,221/3/3/1,238/1/1/1,255/1/2/1 | 13:7:-5:8:2:16  1:1:-3:6:0:16m  5:6:-17:6:1:16m  15:6:3:1:3:16m
1275 | k4 [0,2,4,6,8,10] s1181121325 | 0/1/1/3,17/1/1/1,34/3/1/1,51/8/1/1,68/1/1/1,85/3/1/1,102/1/4/1,119/1/1/1,136/3/1/1,153/3/1/1,170/3/1/1,187/5/4/0,204/5/1/1,221/1/3/1,238/1/1/1,255/1/2/1 | 3:12:-5:8:2:16  11:11:-8:6:0:16m  11:11:-17:6:1:16m  8:10:3:1:3:16m
1300 | k4 [0,2,4,6,8,10] s85394475 | 0/1/1/3,17/1/1/1,34/3/1/1,51/8/1/1,68/1/1/1,85/3/1/1,102/1/4/1,119/1/1/1,136/3/1/1,153/3/1/1,170/3/1/1,187/5/4/0,204/5/1/1,221/1/3/1,238/1/1/1,255/1/2/1 | 8:2:-5:8:2:16  6:6:-8:6:0:16m  1:1:-17:6:1:16m  10:14:3:1:3:16m
1325 | k4 [0,2,4,6,8,10] s871386862 | 0/3/1/3,17/1/1/1,34/3/1/1,51/8/1/1,68/1/1/1,85/3/1/1,102/1/4/1,119/1/1/1,136/3/1/1,153/3/3/1,170/3/1/1,187/3/4/0,204/5/1/1,221/1/3/1,238/1/1/1,255/1/2/1 | 13:7:-5:8:2:16  1:1:-8:6:0:16m  5:6:-17:6:1:16m  5:12:3:1:3:16m
1350 | k4 [0,2,4,6,8,10] s608505840 | 0/3/1/3,17/1/1/1,34/3/1/1,51/8/1/1,68/1/1/1,85/3/1/1,102/1/4/1,119/1/1/1,136/5/1/1,153/3/3/1,170/3/1/1,187/3/4/0,204/5/1/1,221/1/3/1,238/1/1/1,255/1/2/1 | 3:12:-5:8:2:16  11:11:-3:6:0:16m  11:11:-17:6:1:16m  14:7:3:1:3:16m
1375 | k4 [0,2,4,6,8,10] s986044562 | 0/3/1/3,17/1/1/1,34/3/1/1,51/8/1/1,68/1/1/1,85/3/1/1,102/1/4/1,119/1/1/1,136/5/1/1,153/3/3/1,170/3/1/1,187/3/4/0,204/5/1/1,221/1/3/1,238/1/1/1,255/1/2/1 | 4:1:-5:6:2:16  6:6:-3:6:0:16m  1:1:-17:6:1:16m  3:3:3:1:3:16m
1400 | k4 [0,2,4,6,8,10] s1130001728 | 0/3/1/3,17/1/1/1,34/3/1/1,51/8/1/1,68/1/1/1,85/3/1/1,102/1/4/1,119/1/1/1,136/5/1/1,153/3/1/1,170/3/1/1,187/3/4/0,204/5/1/1,221/1/3/1,238/1/1/1,255/3/2/1 | 14:11:-5:6:2:16  1:1:-3:6:0:16m  5:6:-17:6:1:16m  10:14:3:1:3:16m
1425 | k4 [0,2,4,6,8,10] s1071101915 | 0/3/1/3,17/1/1/1,34/3/1/1,51/8/1/1,68/1/1/1,85/3/1/1,102/1/4/1,119/1/1/1,136/5/1/1,153/3/1/1,170/3/1/1,187/3/4/0,204/5/1/1,221/1/3/1,238/1/1/1,255/3/2/1 | 9:6:-5:6:2:16  11:11:-3:6:0:16m  11:11:-17:6:1:16m  8:10:8:1:3:16m
1450 | k4 [0,2,4,6,8,10] s1581356457 | 0/3/1/3,17/1/1/1,34/3/1/1,51/8/1/1,68/1/1/1,85/3/1/1,102/1/4/1,119/1/1/1,136/5/1/1,153/3/1/1,170/3/1/1,187/3/4/0,204/5/1/1,221/1/3/1,238/1/1/1,255/3/2/1 | 4:1:-5:6:2:16  6:6:-3:6:0:16m  1:1:-17:6:1:16m  15:6:8:1:3:16m
1475 | k4 [0,2,4,6,8,10] s781735477 | 0/3/1/3,17/1/1/1,34/3/1/1,51/8/1/1,68/1/1/1,85/3/1/1,102/1/4/1,119/1/1/1,136/5/1/1,153/3/1/1,170/3/1/1,187/3/4/0,204/5/1/1,221/1/3/1,238/1/1/1,255/3/1/1 | 14:11:-5:6:2:16  12:15:-3:6:1:16m  5:6:-17:6:1:16m  2:2:8:1:3:16m
1500 | k4 [0,2,4,6,8,10] s1959779214 | 0/3/1/3,17/1/1/1,34/3/1/1,51/8/1/1,68/3/1/1,85/3/1/1,102/1/4/1,119/1/1/1,136/5/1/1,153/3/1/1,170/3/1/1,187/3/4/0,204/5/1/1,221/1/3/1,238/1/1/1,255/3/1/1 | 9:6:-5:6:2:16  10:10:-3:6:1:16m  11:11:-17:6:1:16m  5:12:8:1:3:16m
1525 | k4 [0,2,4,6,8,10] s978294757 | 0/3/1/3,17/1/1/1,34/3/1/1,51/8/1/1,68/1/1/1,85/3/1/1,102/1/4/1,119/1/1/1,136/5/1/1,153/3/1/1,170/3/1/1,187/3/4/0,204/5/1/1,221/1/3/1,238/1/1/1,255/3/1/1 | 4:1:-5:6:2:16  6:5:-3:6:1:16  1:1:-17:6:1:16  13:8:8:1:3:16m
1550 | k4 [0,2,4,6,8,10] s161828998 | 0/3/1/3,17/1/1/1,34/3/1/1,51/8/1/1,68/1/1/1,85/3/1/1,102/1/4/1,119/1/1/1,136/5/1/1,153/3/1/1,170/3/1/1,187/1/4/0,204/5/1/1,221/1/3/1,238/1/2/1,255/3/1/1 | 14:11:-5:6:2:16  1:4:-3:6:2:16  5:6:-17:6:1:16  7:4:8:1:3:16m
1575 | k4 [0,2,4,6,8,10] s1746130918 | 0/3/1/3,17/1/1/1,34/5/1/1,51/8/1/1,68/1/1/1,85/3/1/1,102/1/4/1,119/1/1/1,136/5/1/1,153/3/1/1,170/3/1/1,187/1/4/0,204/5/1/1,221/1/3/1,238/1/2/1,255/3/2/1 | 9:6:-5:6:2:16  11:14:-3:6:2:16  11:11:-17:6:1:16  9:15:3:1:3:16m
1600 | k11 [0,2,4,6,8,10] s656783586 | 0/3/1/3,17/1/1/1,34/5/1/1,51/8/1/1,68/1/1/1,85/3/1/1,102/1/4/1,119/1/1/1,136/5/3/1,153/3/1/1,170/3/1/1,187/1/4/0,204/5/1/1,221/1/3/1,238/1/2/1,255/3/2/1 | 4:1:-5:6:2:16  6:9:-3:6:2:16  1:1:-12:6:1:16  4:11:3:1:3:16m
1625 | k6 [0,2,4,6,8,10] s852027863 | 0/3/1/3,17/1/1/1,34/5/1/1,51/8/1/1,68/1/1/1,85/1/1/1,102/1/4/1,119/1/1/1,136/5/3/1,153/3/1/1,170/3/1/1,187/1/4/0,204/5/1/1,221/1/3/1,238/1/2/1,255/3/2/1 | 14:11:-5:6:2:16  1:4:-8:6:2:16  5:6:-12:6:1:16  14:7:3:1:3:16m
1650 | k6 [0,2,4,6,8,10] s609050764 | 0/5/1/3,17/1/1/1,34/5/1/1,51/8/1/1,68/1/1/1,85/1/1/1,102/1/4/1,119/1/1/1,136/5/3/1,153/5/1/1,170/3/1/1,187/1/4/0,204/5/1/1,221/1/3/1,238/1/2/1,255/3/2/1 | 9:6:-5:6:2:16  11:14:-8:6:2:16  1:1:-12:8:1:16  2:2:3:1:3:16m
1675 | k6 [0,2,4,6,8,10] s1412308781 | 0/5/1/3,17/1/1/1,34/5/1/1,51/8/1/1,68/1/1/1,85/1/1/1,102/1/4/1,119/1/1/1,136/5/3/1,153/5/1/1,170/3/1/1,187/1/4/0,204/5/1/1,221/1/3/1,238/1/2/1,255/3/2/1 | 4:1:-5:6:2:16  6:9:-8:6:2:16  11:11:-12:8:1:16  6:13:3:1:3:16m
1700 | k6 [0,2,4,6,8,10] s1366882701 | 0/5/1/3,17/1/1/1,34/5/1/1,51/8/1/1,68/1/1/1,85/1/1/1,102/1/4/1,119/1/1/1,136/5/3/1,153/5/1/1,170/3/1/1,187/1/4/0,204/5/1/1,221/1/3/1,238/1/2/1,255/3/2/1 | 14:11:-5:6:2:16  1:4:-8:6:2:16  5:6:-12:8:1:16  12:9:3:1:3:16m
1725 | k1 [0,2,4,6,8,10] s595214930 | 0/5/1/3,17/1/1/1,34/5/1/1,51/8/1/1,68/1/1/1,85/1/1/1,102/1/4/1,119/1/1/1,136/5/3/1,153/5/1/1,170/3/1/1,187/1/4/0,204/5/1/1,221/1/3/1,238/1/2/1,255/3/2/1 | 9:6:-5:6:2:16  2:8:-8:4:2:16  1:1:-12:8:1:16  11:5:3:1:3:16m
1750 | k1 [0,2,4,6,8,10] s434810178 | 0/5/1/3,17/1/1/1,34/5/1/1,51/8/1/1,68/1/1/1,85/1/3/1,102/1/1/3,119/1/1/1,136/5/3/1,153/5/1/1,170/3/1/1,187/1/4/0,204/7/1/1,221/1/3/1,238/1/1/1,255/3/2/1 | 8:2:-5:6:2:16  5:5:-8:4:2:16  10:10:-12:8:1:16  1:1:3:1:3:16m
1775 | k1 [0,2,4,6,8,10] s1933754080 | 0/7/1/3,17/1/1/1,34/5/1/1,51/8/1/1,68/1/1/1,85/1/3/1,102/1/1/3,119/1/1/1,136/5/3/1,153/3/1/1,170/3/1/1,187/1/4/0,204/8/1/1,221/1/3/1,238/1/1/1,255/3/2/1 | 11:14:-5:6:2:16  1:4:-8:4:2:16m  1:1:-12:8:1:16  4:11:3:1:3:16
1800 | k1 [0,2,4,6,8,10] s610713504 | 0/7/1/3,17/3/1/1,34/5/1/1,51/8/1/1,68/1/1/1,85/1/3/1,102/1/1/3,119/1/1/1,136/5/3/1,153/3/1/1,170/3/1/1,187/1/4/0,204/8/1/1,221/1/3/1,238/1/1/1,255/3/2/1 | 14:11:-5:6:2:16  8:2:-8:4:2:16m  8:8:-12:8:1:16  15:6:3:1:3:16
1825 | k6 [0,2,4,6,8,10] s502268088 | 0/7/1/3,17/3/1/1,34/5/1/1,51/8/1/1,68/1/1/1,85/1/3/1,102/1/1/3,119/1/1/1,136/5/3/1,153/3/1/1,170/3/1/1,187/1/4/0,204/8/1/1,221/1/3/1,238/1/1/1,255/3/2/1 | 11:5:-5:6:3:16  4:1:-8:4:2:16m  11:14:-12:8:2:16  2:2:3:1:3:16
1850 | k6 [0,2,4,6,8,10] s1554053751 | 0/7/1/3,17/3/1/1,34/5/1/1,51/8/1/1,68/1/1/1,85/1/3/1,102/1/1/3,119/1/1/1,136/5/3/1,153/3/1/1,170/3/1/1,187/1/4/0,204/8/1/1,221/1/3/1,238/1/1/1,255/3/2/1 | 2:2:-5:6:3:16  11:14:-8:4:2:16m  7:13:-12:6:2:16  5:12:8:1:3:16
1875 | k6 [0,2,4,6,8,10] s1517446884 | 0/7/1/3,17/3/1/1,34/7/1/1,51/8/1/1,68/1/1/1,85/1/1/1,102/1/1/3,119/1/1/1,136/5/3/1,153/3/1/1,170/3/1/1,187/1/4/0,204/8/1/1,221/1/3/1,238/1/1/1,255/3/2/1 | 10:14:-5:6:3:16  7:13:-8:4:2:16m  4:1:-12:6:2:16  13:8:8:1:3:16
1900 | k6 [0,2,4,6,8,10] s292212913 | 0/7/1/3,17/3/1/1,34/7/1/1,51/8/1/1,68/1/1/1,85/1/1/1,102/1/1/3,119/1/1/1,136/5/3/1,153/3/1/1,170/3/1/1,187/1/4/0,204/8/1/1,221/1/3/1,238/1/1/1,255/3/2/1 | 12:9:-5:4:3:16  14:11:-8:4:2:16m  7:4:-12:6:3:16  7:4:8:1:3:16
1925 | k6 [0,2,4,6,8,10] s2016089506 | 0/7/1/3,17/3/1/1,34/7/1/1,51/8/1/1,68/1/1/1,85/1/1/1,102/1/1/3,119/1/1/1,136/5/3/1,153/3/1/1,170/3/1/1,187/1/4/0,204/8/1/1,221/1/3/1,238/1/1/1,255/3/2/1 | 13:8:-5:4:3:16  13:7:-8:2:2:16m  14:7:-17:6:3:16  9:15:3:1:3:16
1950 | k11 [0,2,4,6,8,10] s1354237392 | 0/7/1/3,17/3/1/1,34/7/1/1,51/8/1/1,68/1/1/1,85/1/1/1,102/1/1/3,119/1/1/1,136/5/3/1,153/3/1/1,170/3/1/1,187/1/4/0,204/8/1/1,221/1/3/1,238/1/1/1,255/3/2/1 | 15:6:-5:4:3:16  11:14:-8:2:2:16m  8:10:-12:6:3:16m  8:10:3:0:3:16m
1975 | k11 [0,2,4,6,8,10] s1509912826 | 0/7/1/3,17/3/1/1,34/7/1/1,51/8/1/1,68/1/1/1,85/1/1/1,102/1/1/3,119/1/1/1,136/7/3/1,153/3/1/1,170/3/1/1,187/1/4/0,204/8/1/1,221/1/3/1,238/1/1/1,255/3/2/1 | 8:10:-5:6:3:16  9:6:-8:2:2:16m  14:7:-12:4:3:16m  14:7:3:0:3:16m
2000 | k11 [0,2,4,6,8,10] s937482770 | 0/7/1/3,17/3/1/1,34/7/1/1,51/8/1/1,68/1/1/1,85/1/1/1,102/1/1/3,119/1/1/1,136/7/3/1,153/3/1/1,170/3/1/1,187/1/4/0,204/8/1/1,221/1/3/1,238/1/1/1,255/3/2/1 | 14:7:-5:6:3:16  2:2:-8:2:3:16m  12:9:-12:4:3:16m  7:4:3:0:3:16m"""

-- | The frozen Beta/`pow` diagnostic. Isolates the one transcendental in the
-- | engine (Marbles betaWeights, reached via GNotes): repeated rollValue draws
-- | at four (bias,spread) settings. Proven identical node == BEAM — V8 Math.pow
-- | and BEAM math:pow agree on these inputs, so the GNotes path is safe too.
betaGolden :: String
betaGolden = """probe0 | 2,6,4,4,5,5,4,5,5,5,5,5,5,4,4,5,5,3,5,4,6,5,3,4,6,5,5,5,6,6,4,4,5,4,4,4,4,4,6,5,5,6,3,4,5,4,4,4,6,3,4,4,5,6,4,4,4,4,4,4
probe1 | 3,16,10,10,14,12,11,14,14,12,13,13,14,10,11,13,13,8,13,12,16,14,9,12,16,13,13,13,16,16,11,11,15,11,9,11,12,11,16,12,14,15,8,10,12,12,11,10,16,8,11,11,13,16,12,9,11,10,11,11
probe2 | 1,24,18,16,23,21,18,23,23,21,23,22,24,17,19,23,23,10,23,20,24,23,14,21,24,23,22,22,24,24,19,18,24,18,14,18,20,18,24,21,23,24,11,17,21,21,20,16,24,10,19,19,22,24,21,15,19,18,19,20
probe3 | 0,24,3,1,21,14,5,22,22,14,20,18,23,2,6,21,20,0,20,10,24,23,0,13,24,20,18,16,24,24,6,5,23,5,1,4,9,3,24,15,21,24,0,3,15,12,8,1,24,0,6,5,19,24,13,1,8,3,6,8"""

-- | The frozen input-protocol determinism golden (P3 + P4a). `inputRun` replays a
-- | scripted tick-tagged stream of user actions (every Input family + the three
-- | seed-threading rolls), each round-tripped through the Reef.Protocol codec,
-- | interleaved with the SHARED per-tick composite `Reef.Engine.stepTick`
-- | (runGen → tickChord → stepEmit) over 400 steps. The digest captures the whole
-- | SimState (Odonus + the chord clock `chON·ix:phase` + gen config + Marbles pad +
-- | seed), sampled every 25 steps. Proven BYTE-IDENTICAL under node and the BEAM
-- | (conformance/cross-runtime.sh) — the input protocol, its codec, AND the whole
-- | tick composite apply identically on both runtimes, the precondition for lockstep
-- | co-simulation (reef/docs/PLAN-lockstep-cosimulation.md). Regenerated at P4a when
-- | stepTick folded in `tickChord` (the chord overlay engages at tick 130 via
-- | FollowChord — ch1, phase advancing — which the old inline conformance never ran).
inputGolden :: String
inputGolden = """  25 | k2 [0,2,3,5,7,8,10] s524102504 sp500 bi500 h- ch- | 7/1/1/1,17/1/1/1,34/1/1/1,51/1/1/1,68/1/1/1,85/1/1/1,102/1/3/1,119/1/1/1,136/1/1/1,153/1/1/1,170/1/1/1,187/1/1/1,204/1/1/1,221/1/1/1,238/1/1/1,255/1/1/1 | 9:9:0:6:0:16  2:8:5:4:2:16  7:4:-12:6:3:16  14:11:3:5:2:16m | 0:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:96:30,1:96:40,1:96:30,1:96:30,1:96:25,1:72:30
  50 | k2 [0,2,3,5,7,8,10] s1617428359 sp300 bi700 h- ch- | 187/1/1/1,177/1/1/1,178/1/1/1,178/1/1/1,170/1/1/1,175/1/1/1,198/1/3/1,162/1/1/1,180/1/1/1,179/1/1/1,190/1/1/1,177/1/1/1,185/1/1/1,191/1/3/1,189/1/1/1,176/1/1/1 | 2:2:0:6:0:16m  13:8:5:4:3:16  4:11:-12:6:3:16m  2:8:3:5:2:16m | 1:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:96:30,1:96:40,1:96:30,1:96:30,1:96:25,1:72:30
  75 | k2 [0,2,3,5,7,8,10] s1808275514 sp300 bi700 h- ch- | 187/1/1/1,177/1/1/1,178/1/1/1,178/1/1/1,170/1/1/1,175/1/1/1,198/1/3/1,162/1/1/1,180/1/1/1,179/1/1/1,190/1/1/1,177/1/1/1,185/1/1/1,191/1/3/1,189/1/1/1,176/1/1/1 | 1:1:0:6:1:16m  11:5:5:4:3:16  2:2:-12:6:3:16m  12:3:3:5:2:16m | 1:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:96:30,1:96:40,1:96:30,1:96:30,1:96:25,1:72:30
 100 | k2 [0,2,3,5,7,8,10] s1379382951 sp300 bi700 h<c'maj d'min> ch0,4,7 | 187/1/1/1,177/1/1/1,178/1/1/1,178/1/1/1,170/1/1/1,175/1/1/1,198/1/3/1,162/1/1/1,180/1/1/1,179/1/1/1,190/1/1/1,177/1/1/1,185/1/1/1,191/1/3/1,189/1/1/1,176/1/1/1 | 10:10:0:6:1:16m  1:1:0:4:3:16  15:6:-12:8:3:16m  6:9:3:3:2:16m | 1:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:96:30,1:96:40,1:96:30,1:96:30,1:96:25,1:72:30
 125 | k2 [0,2,3,5,7,8,10] s805392551 sp300 bi700 h<c'maj d'min> ch2,5,9 | 187/1/1/1,177/1/1/1,178/1/1/1,178/1/1/1,170/1/1/1,175/1/1/1,198/1/3/1,162/1/1/1,180/1/1/1,179/1/1/1,190/1/1/1,177/1/1/1,185/1/1/1,191/1/3/1,189/1/1/1,176/1/1/1 | 14:11:0:6:2:16m  10:14:0:4:3:16  7:4:-12:8:3:16m  7:13:-2:3:2:16m | 1:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:96:30,1:96:40,1:96:30,1:96:30,1:96:25,1:72:30
 150 | k2 [0,2,3,5,7,8,10] s777512110 sp300 bi700 hc'min ch0,3,7 | 253/1/1/1,19/1/1/1,108/1/1/1,215/1/1/1,162/1/1/1,241/1/1/1,110/1/3/1,242/1/1/1,204/1/1/1,96/1/1/1,63/1/1/1,212/1/1/1,10/1/1/1,58/1/3/1,24/1/1/1,220/1/1/1 | 1:4:-5:6:2:16m  8:10:0:4:3:16  2:2:-12:8:3:16m  5:5:-2:3:2:16m | 1:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:96:30,1:96:40,1:96:30,1:96:30,1:96:25,1:72:30
 175 | k2 [0,2,3,5,7,8,10] s1994802600 sp300 bi700 h- ch- | 253/1/1/1,19/1/1/1,108/1/1/1,215/1/1/1,162/1/1/1,241/1/1/1,110/3/3/1,242/1/1/1,204/3/1/1,96/3/1/1,63/1/1/1,212/1/1/1,10/1/1/1,58/3/3/1,24/1/1/1,220/1/1/1 | 7:13:-5:6:2:16  12:7:0:4:3:16  7:0:-12:8:3:16  10:4:-2:3:2:16m | 1:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:96:30,1:96:40,1:96:30,1:96:30,1:96:25,1:72:30
 200 | k2 [0,2,3,5,7,8,10] s1339878053 sp300 bi700 h- ch- | 253/1/1/1,19/1/1/1,108/1/1/1,215/1/1/1,162/1/1/1,241/1/1/1,110/3/3/1,242/1/1/1,204/3/1/1,96/3/1/1,63/1/1/1,212/1/1/1,10/1/1/1,58/3/3/1,24/1/1/1,220/1/1/1 | 9:6:-5:6:2:16  11:3:0:4:3:16m  2:14:-12:8:3:16  8:12:-2:3:2:16m | 1:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:120:30,1:96:60,1:96:30,1:96:30,1:96:25,1:72:30
 225 | k2 [0,2,3,5,7,9,10] s1179650736 sp300 bi700 h- ch- | 253/1/1/1,199/1/1/1,108/1/1/1,215/1/1/1,162/1/1/1,241/1/1/1,182/3/3/1,242/1/1/1,204/3/1/1,96/3/1/1,63/1/1/1,212/1/1/1,10/3/1/1,58/3/3/1,24/1/1/1,220/1/1/1 | 15:15:-5:6:2:16  11:11:0:4:4:16m  0:12:-12:8:3:16  0:10:-2:3:2:16m | 1:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:120:30,1:96:60,1:96:30,1:96:30,1:96:25,1:72:30
 250 | k7 [0,2,3,5,7,9,10] s1986176768 sp300 bi700 h- ch- | 253/1/1/1,199/1/1/1,108/1/1/1,215/1/1/1,162/1/1/1,186/1/1/1,182/3/3/1,242/1/1/1,204/3/1/1,96/3/1/1,63/1/1/1,212/1/1/1,10/3/1/1,58/3/3/1,24/1/1/1,177/1/1/1 | 6:8:-5:6:2:16  7:7:0:4:4:16m  9:10:-12:8:3:16  2:1:-10:3:2:16m | 1:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:120:30,1:96:60,1:96:30,1:96:30,1:96:25,1:72:30
 275 | k7 [0,2,3,5,7,9,10] s619397011 sp300 bi700 h- ch- | 253/1/1/1,199/1/1/1,108/1/1/1,215/1/1/1,162/1/1/1,186/1/1/1,182/3/3/1,242/1/1/1,204/3/1/1,96/3/1/1,63/1/1/1,212/1/1/1,10/3/1/1,58/3/3/1,24/1/1/1,177/1/1/1 | 8:1:-5:6:2:16  11:10:0:6:4:16m  6:8:-12:8:3:16  11:7:-10:3:2:16m | 1:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:120:30,1:96:60,1:96:30,1:96:30,1:96:25,1:72:30
 300 | k7 [0,2,4,6,7,9,11] s1808466296 sp300 bi700 h- ch- | 253/1/1/1,199/1/1/1,108/1/1/1,215/1/1/1,162/1/1/1,186/1/1/1,182/3/3/1,242/1/1/1,204/3/1/1,96/3/1/1,63/1/1/1,212/1/1/1,10/3/1/1,58/3/3/1,24/1/1/1,177/1/1/1 | 8:1:-5:6:2:16  8:1:-5:6:2:16  8:1:-5:6:2:16  8:1:-5:6:2:16 | 1:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:120:30,1:96:60,1:96:30,1:96:30,1:96:25,1:72:30
 325 | k7 [0,2,4,6,7,9,11] s1307061696 sp300 bi700 h- ch- | 253/1/1/1,199/1/1/1,108/1/1/1,215/1/1/1,162/1/1/1,186/1/1/1,182/1/3/1,242/1/1/1,204/3/1/1,96/3/1/1,63/1/1/1,212/1/1/1,10/3/1/1,58/3/3/1,24/1/1/1,177/1/1/1 | 3:2:-5:6:3:16  14:10:-5:6:2:16  2:1:-5:6:3:16  14:10:-5:6:2:16 | 1:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:120:30,1:96:60,1:96:30,1:96:30,1:96:25,1:72:30
 350 | k7 [0,4,5,7,9] s1629888615 sp300 bi700 h- ch- | 253/1/1/1,199/1/1/1,108/1/1/1,215/1/1/1,162/1/1/1,186/1/1/1,182/1/3/1,242/1/1/1,204/3/1/1,96/3/1/1,63/1/1/1,164/1/1/1,10/1/1/1,58/3/3/1,192/1/1/1,177/1/1/1 | 5:11:-5:6:3:16  1:3:-5:6:2:16  1:0:-5:8:3:16  1:3:3:6:2:16 | 1:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:120:30,1:96:60,1:96:30,1:96:30,1:96:25,1:72:30
 375 | k7 [0,1,4,5,7,8,10] s1593559689 sp300 bi700 h- ch- | 253/1/1/1,199/1/1/1,108/1/1/1,215/1/1/1,162/1/1/1,186/1/1/1,182/1/3/1,242/1/1/1,204/3/1/1,96/3/1/1,63/1/1/1,164/1/1/1,10/1/1/1,58/3/3/1,192/1/1/1,177/1/1/1 | 11:4:-5:6:3:16  7:12:-5:6:2:16  3:2:-13:8:3:16  10:9:3:8:2:16 | 1:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:120:30,1:96:60,1:96:30,1:96:30,1:96:25,1:72:30
 400 | k7 [0,1,4,5,7,8,10] s1342496919 sp300 bi700 h- ch- | 253/1/1/1,199/1/1/1,108/1/1/1,215/1/1/1,162/1/1/1,186/1/1/1,182/1/3/1,242/1/1/1,204/3/1/1,96/3/1/1,63/1/1/1,164/1/1/1,10/1/1/1,58/3/3/1,192/1/1/1,177/1/1/1 | 10:13:-5:6:3:16  9:5:-5:6:2:16  11:4:-13:8:3:16  3:11:3:8:2:16 | 1:96:20,1:96:30,1:96:30,1:96:30,1:72:25,1:72:25,1:120:30,1:96:60,1:96:30,1:96:30,1:96:25,1:72:30"""

-- | The frozen SimState handoff golden (P4d). `simRun` DECODES a real handoff JSON
-- | — the string `Reef.Protocol.encodeSim` produced in the JS frontend for a
-- | SimState with 10 gen sources on (GNotes off), seed 7 — then steps `stepTick`
-- | from the reconstructed state, digesting every 25 of 400 steps. Proven
-- | BYTE-IDENTICAL under node and the BEAM (conformance/cross-runtime.sh): the
-- | BEAM's decodeSim rebuilds exactly the state the browser encoded and evolves it
-- | identically — the lockstep handoff lands the rig on the frontend's state
-- | (reef/docs/PLAN-lockstep-cosimulation.md, P4d).
simGolden :: String
simGolden = """  25 | k0 [0,2,3,5,7,8,10] s2066833345 | 0/1/1/1,1/1/1/1,2/1/1/1,3/1/1/1,4/3/1/1,5/1/1/1,6/1/1/1,7/1/1/1,8/1/1/1,9/1/1/1,10/1/1/1,11/1/1/1,12/1/1/1,13/1/1/1,14/1/1/1,15/1/1/1 | 13:13:0:4:0:16  4:7:7:2:1:16m  10:11:-12:6:4:16m  6:9:3:3:2:16m
  50 | k0 [0,2,3,5,7,8,10] s855074095 | 0/1/1/1,1/1/1/1,2/1/1/1,3/1/1/1,4/3/1/1,5/1/1/1,6/1/1/1,7/1/1/1,8/1/1/1,9/1/1/1,10/1/1/1,11/1/1/1,12/1/1/1,13/3/1/1,14/1/1/1,15/1/1/1 | 9:9:0:4:0:16m  14:13:7:2:1:16m  4:2:-12:6:4:16  7:13:3:3:2:16m
  75 | k0 [0,2,3,5,7,8,10] s1720826093 | 0/1/1/1,1/1/1/5,2/1/1/1,3/1/1/1,4/5/1/1,5/1/1/1,6/1/1/1,7/1/1/1,8/1/1/1,9/1/1/1,10/1/1/1,11/1/1/1,12/1/1/1,13/3/1/1,14/1/1/1,15/1/1/1 | 6:6:0:4:0:16m  3:3:7:2:1:16m  12:9:-12:6:4:16  5:5:3:3:2:16m
 100 | k0 [0,2,3,5,7,8,10] s2116085955 | 0/1/1/1,1/1/2/5,2/1/1/1,3/3/1/5,4/5/1/1,5/1/1/1,6/1/1/1,7/1/1/1,8/1/1/1,9/1/1/1,10/1/1/1,11/1/1/1,12/1/1/1,13/3/1/1,14/1/1/1,15/1/1/1 | 10:10:0:6:0:16m  9:9:7:2:1:16  1:1:-12:4:4:16  1:4:3:3:2:16
 125 | k0 [0,2,3,5,7,8,10] s650677892 | 0/1/1/1,1/1/2/5,2/1/1/1,3/3/1/5,4/5/1/1,5/1/1/1,6/1/1/1,7/1/1/1,8/1/1/5,9/1/1/1,10/1/1/1,11/1/1/1,12/1/1/1,13/1/1/1,14/1/1/1,15/1/1/1 | 3:3:0:6:0:16m  1:4:7:0:2:16  8:5:-12:4:4:16  3:12:3:3:2:16
 150 | k0 [0,2,3,5,7,8,10] s2024740166 | 0/1/1/1,1/1/2/5,2/1/1/1,3/3/1/5,4/5/1/1,5/1/1/1,6/1/1/1,7/1/1/1,8/1/1/5,9/1/1/1,10/1/1/1,11/1/1/1,12/1/1/1,13/1/1/1,14/1/1/1,15/3/1/1 | 12:12:0:6:0:16m  13:7:7:0:2:16  9:8:-12:4:4:16  10:10:3:3:2:16
 175 | k0 [0,2,3,5,7,8,10] s573035493 | 0/1/1/1,1/1/2/5,2/1/2/1,3/3/1/5,4/5/1/1,5/1/1/1,6/1/1/1,7/1/1/1,8/1/1/5,9/1/1/1,10/1/1/1,11/1/1/1,12/3/1/1,13/1/1/1,14/1/1/3,15/3/1/1 | 6:6:0:6:0:16m  10:10:7:0:2:16  15:15:-12:4:0:16  4:1:-2:3:2:16
 200 | k0 [0,2,3,5,7,8,10] s1273971213 | 0/1/1/1,1/1/2/5,2/1/2/1,3/3/1/5,4/5/1/1,5/1/1/1,6/1/1/1,7/1/1/1,8/1/1/5,9/3/1/1,10/1/1/1,11/1/1/1,12/3/1/1,13/1/1/1,14/1/1/3,15/3/1/1 | 1:1:0:6:0:16m  11:14:7:0:2:16  1:1:-12:4:0:16  13:7:-2:3:2:16
 225 | k0 [0,2,3,5,7,8,10] s273685954 | 0/1/1/1,1/1/2/5,2/1/2/1,3/3/1/5,4/5/1/1,5/1/1/1,6/1/1/1,7/1/1/1,8/1/1/5,9/3/1/1,10/1/1/1,11/3/1/1,12/3/1/3,13/1/1/1,14/1/1/3,15/3/1/1 | 11:11:0:6:0:16m  4:1:12:0:2:16  4:4:-12:4:0:16  11:14:-2:3:2:16
 250 | k2 [0,2,3,5,7,8,10] s1707112389 | 0/3/1/1,1/1/2/5,2/1/2/1,3/3/1/5,4/5/1/1,5/1/1/1,6/1/1/1,7/1/1/1,8/1/1/5,9/3/1/1,10/1/1/1,11/3/1/1,12/3/1/3,13/1/1/1,14/1/1/3,15/3/1/1 | 9:9:0:8:0:16m  5:5:12:0:2:16  5:5:-12:4:0:16  1:4:-2:3:2:16
 275 | k2 [0,2,3,5,7,8,10] s633466313 | 0/3/1/1,1/1/2/5,2/1/2/1,3/3/1/5,4/5/1/1,5/1/1/1,6/1/1/1,7/1/1/1,8/1/1/4,9/3/1/1,10/1/1/1,11/3/1/1,12/3/1/3,13/1/1/1,14/1/1/3,15/3/1/1 | 3:3:0:8:0:16m  2:8:12:0:2:16  7:7:-7:4:0:16  1:4:-2:1:2:16
 300 | k2 [0,2,3,5,7,8,10] s1910955785 | 0/5/1/1,1/1/1/5,2/1/2/1,3/3/1/5,4/5/1/1,5/1/1/1,6/1/1/1,7/1/1/1,8/1/1/4,9/1/1/1,10/1/1/1,11/3/1/1,12/3/1/3,13/1/1/1,14/1/1/3,15/3/1/1 | 11:11:0:8:0:16m  3:12:12:0:2:16  11:11:-7:4:1:16  2:8:-2:1:2:16
 325 | k2 [0,2,3,5,7,8,10] s1270918581 | 0/5/1/1,1/1/1/5,2/1/2/1,3/3/1/5,4/5/1/1,5/1/1/1,6/1/1/1,7/3/1/1,8/1/1/4,9/1/1/1,10/1/1/1,11/3/1/1,12/3/1/3,13/1/1/1,14/1/1/3,15/3/1/1 | 5:5:-5:8:0:16m  0:0:12:0:2:16  13:14:-7:4:1:16  7:13:-7:1:2:16
 350 | k2 [0,2,3,5,7,8,10] s166813907 | 0/5/1/1,1/1/1/5,2/1/2/1,3/3/1/5,4/5/1/1,5/1/1/1,6/1/1/1,7/3/1/1,8/1/1/4,9/1/1/1,10/1/1/1,11/3/1/1,12/3/1/3,13/1/1/1,14/1/1/3,15/3/1/1 | 15:15:-5:8:0:16m  1:4:12:0:2:16  0:0:-7:4:1:16  7:13:-7:1:2:16
 375 | k2 [0,2,3,5,7,8,10] s403641825 | 0/5/1/1,1/1/1/5,2/1/2/1,3/1/1/5,4/5/1/1,5/1/1/1,6/1/1/1,7/3/1/1,8/1/1/4,9/1/1/1,10/1/1/1,11/3/1/1,12/3/1/3,13/3/1/1,14/1/1/3,15/3/1/1 | 4:4:-5:6:0:16m  2:2:12:0:3:16  2:2:-7:4:1:16  2:8:-7:1:2:16
 400 | k7 [0,2,3,5,7,8,10] s758200914 | 0/5/1/1,1/1/1/5,2/1/2/1,3/1/1/5,4/5/1/1,5/1/1/1,6/1/1/1,7/3/1/1,8/1/1/4,9/1/1/1,10/1/1/1,11/3/4/1,12/1/1/3,13/3/1/1,14/1/1/3,15/3/1/1 | 1:1:-5:6:0:16m  11:5:12:0:3:16  3:3:-2:4:1:16  8:2:-7:3:2:16"""

-- | The frozen render of `Reef.Conformance.balistesRun` — the shared Grids
-- | engine over 256 steps. Identical under node and the BEAM (cross-runtime.sh).
balistesGolden :: String
balistesGolden = """   0 x  0 y  0 p0,0,0 | 0 2!
   1 x  5 y  3 p0,0,0 | -
   2 x 10 y  6 p0,0,0 | -
   3 x 15 y  9 p0,0,0 | -
   4 x 20 y 12 p0,0,0 | 2
   5 x 25 y 15 p0,0,0 | -
   6 x 30 y 18 p0,0,0 | -
   7 x 35 y 21 p0,0,0 | -
   8 x 40 y 24 p0,0,0 | 1! 2
   9 x 45 y 27 p0,0,0 | -
  10 x 50 y 30 p0,0,0 | -
  11 x 55 y 33 p0,0,0 | -
  12 x 60 y 36 p0,0,0 | 0! 1
  13 x 65 y 39 p0,0,0 | -
  14 x 70 y 42 p0,0,0 | -
  15 x 75 y 45 p0,0,0 | -
  16 x 80 y 48 p0,0,0 | 0! 2!
  17 x 85 y 51 p0,0,0 | -
  18 x 90 y 54 p0,0,0 | -
  19 x 95 y 57 p0,0,0 | -
  20 x100 y 60 p0,0,0 | 0 2
  21 x105 y 63 p0,0,0 | -
  22 x110 y 66 p0,0,0 | -
  23 x115 y 69 p0,0,0 | -
  24 x120 y 72 p0,0,0 | 1 2
  25 x125 y 75 p0,0,0 | -
  26 x130 y 78 p0,0,0 | -
  27 x135 y 81 p0,0,0 | -
  28 x140 y 84 p0,0,0 | 1! 2
  29 x145 y 87 p0,0,0 | -
  30 x150 y 90 p0,0,0 | -
  31 x155 y 93 p0,0,0 | -
  32 x160 y 96 p17,45,45 | 0! 2
  33 x165 y 99 p17,45,45 | -
  34 x170 y102 p17,45,45 | 2
  35 x175 y105 p17,45,45 | -
  36 x180 y108 p17,45,45 | 0 1 2!
  37 x185 y111 p17,45,45 | -
  38 x190 y114 p17,45,45 | -
  39 x195 y117 p17,45,45 | -
  40 x200 y120 p17,45,45 | 0 1! 2
  41 x205 y123 p17,45,45 | -
  42 x210 y126 p17,45,45 | -
  43 x215 y129 p17,45,45 | -
  44 x220 y132 p17,45,45 | 1! 2!
  45 x225 y135 p17,45,45 | -
  46 x230 y138 p17,45,45 | 1
  47 x235 y141 p17,45,45 | -
  48 x240 y144 p17,45,45 | 0! 1 2
  49 x245 y147 p17,45,45 | -
  50 x250 y150 p17,45,45 | 2!
  51 x255 y153 p17,45,45 | -
  52 x  4 y156 p17,45,45 | 0! 1 2!
  53 x  9 y159 p17,45,45 | -
  54 x 14 y162 p17,45,45 | 2
  55 x 19 y165 p17,45,45 | -
  56 x 24 y168 p17,45,45 | 1! 2!
  57 x 29 y171 p17,45,45 | -
  58 x 34 y174 p17,45,45 | 1 2
  59 x 39 y177 p17,45,45 | -
  60 x 44 y180 p17,45,45 | 0 1 2!
  61 x 49 y183 p17,45,45 | -
  62 x 54 y186 p17,45,45 | 1 2
  63 x 59 y189 p17,45,45 | -
  64 x 64 y192 p4,33,6 | 0! 2!
  65 x 69 y195 p4,33,6 | -
  66 x 74 y198 p4,33,6 | -
  67 x 79 y201 p4,33,6 | -
  68 x 84 y204 p4,33,6 | 0 2!
  69 x 89 y207 p4,33,6 | -
  70 x 94 y210 p4,33,6 | 2
  71 x 99 y213 p4,33,6 | -
  72 x104 y216 p4,33,6 | 1! 2
  73 x109 y219 p4,33,6 | -
  74 x114 y222 p4,33,6 | -
  75 x119 y225 p4,33,6 | -
  76 x124 y228 p4,33,6 | 2
  77 x129 y231 p4,33,6 | -
  78 x134 y234 p4,33,6 | -
  79 x139 y237 p4,33,6 | -
  80 x144 y240 p4,33,6 | 0 2
  81 x149 y243 p4,33,6 | -
  82 x154 y246 p4,33,6 | 2
  83 x159 y249 p4,33,6 | -
  84 x164 y252 p4,33,6 | 0! 2
  85 x169 y255 p4,33,6 | -
  86 x174 y  2 p4,33,6 | 0 1 2
  87 x179 y  5 p4,33,6 | 1 2
  88 x184 y  8 p4,33,6 | 0 1 2
  89 x189 y 11 p4,33,6 | -
  90 x194 y 14 p4,33,6 | -
  91 x199 y 17 p4,33,6 | -
  92 x204 y 20 p4,33,6 | 1! 2
  93 x209 y 23 p4,33,6 | -
  94 x214 y 26 p4,33,6 | -
  95 x219 y 29 p4,33,6 | -
  96 x224 y 32 p18,31,8 | 0! 2!
  97 x229 y 35 p18,31,8 | -
  98 x234 y 38 p18,31,8 | -
  99 x239 y 41 p18,31,8 | -
 100 x244 y 44 p18,31,8 | -
 101 x249 y 47 p18,31,8 | -
 102 x254 y 50 p18,31,8 | 0 2
 103 x  3 y 53 p18,31,8 | -
 104 x  8 y 56 p18,31,8 | 1! 2
 105 x 13 y 59 p18,31,8 | -
 106 x 18 y 62 p18,31,8 | -
 107 x 23 y 65 p18,31,8 | -
 108 x 28 y 68 p18,31,8 | 0! 2
 109 x 33 y 71 p18,31,8 | -
 110 x 38 y 74 p18,31,8 | -
 111 x 43 y 77 p18,31,8 | -
 112 x 48 y 80 p18,31,8 | 0! 2!
 113 x 53 y 83 p18,31,8 | -
 114 x 58 y 86 p18,31,8 | -
 115 x 63 y 89 p18,31,8 | -
 116 x 68 y 92 p18,31,8 | 0 1 2
 117 x 73 y 95 p18,31,8 | -
 118 x 78 y 98 p18,31,8 | -
 119 x 83 y101 p18,31,8 | -
 120 x 88 y104 p18,31,8 | 1! 2
 121 x 93 y107 p18,31,8 | -
 122 x 98 y110 p18,31,8 | 1
 123 x103 y113 p18,31,8 | -
 124 x108 y116 p18,31,8 | 1! 2
 125 x113 y119 p18,31,8 | -
 126 x118 y122 p18,31,8 | -
 127 x123 y125 p18,31,8 | -
 128 x128 y128 p17,21,30 | 0! 2
 129 x133 y131 p17,21,30 | -
 130 x138 y134 p17,21,30 | -
 131 x143 y137 p17,21,30 | -
 132 x148 y140 p17,21,30 | 2!
 133 x153 y143 p17,21,30 | -
 134 x158 y146 p17,21,30 | 2
 135 x163 y149 p17,21,30 | -
 136 x168 y152 p17,21,30 | 0 1! 2
 137 x173 y155 p17,21,30 | -
 138 x178 y158 p17,21,30 | 2
 139 x183 y161 p17,21,30 | -
 140 x188 y164 p17,21,30 | 1 2!
 141 x193 y167 p17,21,30 | -
 142 x198 y170 p17,21,30 | -
 143 x203 y173 p17,21,30 | -
 144 x208 y176 p17,21,30 | 0! 2
 145 x213 y179 p17,21,30 | -
 146 x218 y182 p17,21,30 | 1 2
 147 x223 y185 p17,21,30 | -
 148 x228 y188 p17,21,30 | 0 2!
 149 x233 y191 p17,21,30 | -
 150 x238 y194 p17,21,30 | -
 151 x243 y197 p17,21,30 | -
 152 x248 y200 p17,21,30 | 0 1! 2
 153 x253 y203 p17,21,30 | -
 154 x  2 y206 p17,21,30 | 0! 2
 155 x  7 y209 p17,21,30 | -
 156 x 12 y212 p17,21,30 | 0 1 2!
 157 x 17 y215 p17,21,30 | -
 158 x 22 y218 p17,21,30 | 1 2
 159 x 27 y221 p17,21,30 | -
 160 x 32 y224 p16,0,0 | 0! 2
 161 x 37 y227 p16,0,0 | -
 162 x 42 y230 p16,0,0 | 1 2
 163 x 47 y233 p16,0,0 | -
 164 x 52 y236 p16,0,0 | 0! 2!
 165 x 57 y239 p16,0,0 | -
 166 x 62 y242 p16,0,0 | 2!
 167 x 67 y245 p16,0,0 | -
 168 x 72 y248 p16,0,0 | 1! 2
 169 x 77 y251 p16,0,0 | -
 170 x 82 y254 p16,0,0 | 1
 171 x 87 y  1 p16,0,0 | -
 172 x 92 y  4 p16,0,0 | 0 1
 173 x 97 y  7 p16,0,0 | -
 174 x102 y 10 p16,0,0 | 2
 175 x107 y 13 p16,0,0 | -
 176 x112 y 16 p16,0,0 | 0!
 177 x117 y 19 p16,0,0 | -
 178 x122 y 22 p16,0,0 | 2
 179 x127 y 25 p16,0,0 | -
 180 x132 y 28 p16,0,0 | 0 2
 181 x137 y 31 p16,0,0 | -
 182 x142 y 34 p16,0,0 | -
 183 x147 y 37 p16,0,0 | -
 184 x152 y 40 p16,0,0 | 0 2
 185 x157 y 43 p16,0,0 | -
 186 x162 y 46 p16,0,0 | -
 187 x167 y 49 p16,0,0 | -
 188 x172 y 52 p16,0,0 | 0 1 2
 189 x177 y 55 p16,0,0 | -
 190 x182 y 58 p16,0,0 | 1
 191 x187 y 61 p16,0,0 | -
 192 x192 y 64 p9,1,3 | 0! 2
 193 x197 y 67 p9,1,3 | -
 194 x202 y 70 p9,1,3 | -
 195 x207 y 73 p9,1,3 | -
 196 x212 y 76 p9,1,3 | 1
 197 x217 y 79 p9,1,3 | -
 198 x222 y 82 p9,1,3 | 2
 199 x227 y 85 p9,1,3 | -
 200 x232 y 88 p9,1,3 | 0
 201 x237 y 91 p9,1,3 | -
 202 x242 y 94 p9,1,3 | -
 203 x247 y 97 p9,1,3 | -
 204 x252 y100 p9,1,3 | 1 2
 205 x  1 y103 p9,1,3 | -
 206 x  6 y106 p9,1,3 | -
 207 x 11 y109 p9,1,3 | -
 208 x 16 y112 p9,1,3 | 0 2
 209 x 21 y115 p9,1,3 | -
 210 x 26 y118 p9,1,3 | -
 211 x 31 y121 p9,1,3 | -
 212 x 36 y124 p9,1,3 | 0 2!
 213 x 41 y127 p9,1,3 | -
 214 x 46 y130 p9,1,3 | -
 215 x 51 y133 p9,1,3 | -
 216 x 56 y136 p9,1,3 | 0 1! 2
 217 x 61 y139 p9,1,3 | -
 218 x 66 y142 p9,1,3 | 1
 219 x 71 y145 p9,1,3 | -
 220 x 76 y148 p9,1,3 | 1 2
 221 x 81 y151 p9,1,3 | -
 222 x 86 y154 p9,1,3 | -
 223 x 91 y157 p9,1,3 | -
 224 x 96 y160 p5,7,2 | 0! 2
 225 x101 y163 p5,7,2 | -
 226 x106 y166 p5,7,2 | -
 227 x111 y169 p5,7,2 | -
 228 x116 y172 p5,7,2 | 2!
 229 x121 y175 p5,7,2 | -
 230 x126 y178 p5,7,2 | 1
 231 x131 y181 p5,7,2 | -
 232 x136 y184 p5,7,2 | 1! 2
 233 x141 y187 p5,7,2 | -
 234 x146 y190 p5,7,2 | 2
 235 x151 y193 p5,7,2 | -
 236 x156 y196 p5,7,2 | 2!
 237 x161 y199 p5,7,2 | -
 238 x166 y202 p5,7,2 | -
 239 x171 y205 p5,7,2 | -
 240 x176 y208 p5,7,2 | 0! 2
 241 x181 y211 p5,7,2 | -
 242 x186 y214 p5,7,2 | 1 2
 243 x191 y217 p5,7,2 | -
 244 x196 y220 p5,7,2 | 2!
 245 x201 y223 p5,7,2 | -
 246 x206 y226 p5,7,2 | 2
 247 x211 y229 p5,7,2 | -
 248 x216 y232 p5,7,2 | 0 1!
 249 x221 y235 p5,7,2 | -
 250 x226 y238 p5,7,2 | -
 251 x231 y241 p5,7,2 | -
 252 x236 y244 p5,7,2 | 0 2!
 253 x241 y247 p5,7,2 | -
 254 x246 y250 p5,7,2 | 1
 255 x251 y253 p5,7,2 | -"""

-- | The frozen render of `Reef.Conformance.balistesSimRun` — a BalSim through the
-- | codec, then 128 steps digested by renderStep. Identical node ↔ BEAM.
balistesSimGolden :: String
balistesSimGolden = """   0 | 36/120/0/30x1  46/78/-8/200x1
   1 | -
   2 | -
   3 | -
   4 | 40/120/12/30x1
   5 | -
   6 | 36/78/0/30x1  42/78/-8/30x1
   7 | -
   8 | 42/78/-8/30x1
   9 | -
  10 | 36/78/0/30x1  40/78/12/30x1
  11 | -
  12 | 40/120/12/30x1
  13 | -
  14 | -
  15 | -
  16 | 36/120/0/30x1  46/120/-8/200x1
  17 | -
  18 | 36/78/0/30x1
  19 | -
  20 | 36/78/0/30x1  40/78/12/30x1  46/120/-8/200x1
  21 | -
  22 | 36/78/0/30x1
  23 | -
  24 | 36/78/0/30x1  46/120/-8/200x1
  25 | -
  26 | -
  27 | -
  28 | 36/78/0/30x1  40/78/12/30x1  46/78/-8/200x1
  29 | -
  30 | 40/78/12/30x1  42/78/-8/30x1
  31 | -
  32 | 36/120/0/30x1  46/78/-8/200x1
  33 | -
  34 | 42/78/-8/30x1
  35 | -
  36 | 40/120/12/30x1  42/78/-8/30x1
  37 | -
  38 | 36/78/0/30x1  42/78/-8/30x1
  39 | -
  40 | 42/78/-8/30x1
  41 | -
  42 | 36/78/0/30x1  40/78/12/30x1
  43 | -
  44 | 40/120/12/30x1
  45 | -
  46 | -
  47 | -
  48 | 36/120/0/30x1  46/120/-8/200x1
  49 | -
  50 | 36/78/0/30x1
  51 | -
  52 | 36/78/0/30x1  40/120/12/30x1  46/120/-8/200x1
  53 | -
  54 | 36/78/0/30x1
  55 | -
  56 | 36/78/0/30x1  46/120/-8/200x1
  57 | -
  58 | 42/78/-8/30x1
  59 | -
  60 | 36/78/0/30x1  40/78/12/30x1  46/78/-8/200x1
  61 | -
  62 | 40/78/12/30x1  42/78/-8/30x1
  63 | -
  64 | 36/120/0/30x1  46/78/-8/200x1
  65 | -
  66 | 42/78/-8/30x1
  67 | -
  68 | 36/78/0/30x1  40/120/12/30x1  42/78/-8/30x1
  69 | -
  70 | 36/78/0/30x1  42/78/-8/30x1
  71 | -
  72 | 36/78/0/30x1  42/78/-8/30x1
  73 | -
  74 | 36/78/0/30x1  40/78/12/30x1
  75 | -
  76 | 40/120/12/30x1
  77 | -
  78 | 36/78/0/30x1
  79 | -
  80 | 36/120/0/30x1  46/120/-8/200x1
  81 | -
  82 | 36/78/0/30x1  40/78/12/30x1
  83 | -
  84 | 36/78/0/30x1  40/120/12/30x1  46/120/-8/200x1
  85 | -
  86 | 36/78/0/30x1
  87 | -
  88 | 36/120/0/30x1  46/120/-8/200x1
  89 | -
  90 | 36/78/0/30x1  42/78/-8/30x1
  91 | -
  92 | 36/78/0/30x1  40/120/12/30x1  46/78/-8/200x1
  93 | -
  94 | 36/78/0/30x1  40/78/12/30x1  42/78/-8/30x1
  95 | -
  96 | 36/120/0/30x1  46/78/-8/200x1
  97 | -
  98 | 42/78/-8/30x1
  99 | -
 100 | 40/120/12/30x1
 101 | -
 102 | 36/78/0/30x1  42/78/-8/30x1
 103 | -
 104 | 36/78/0/30x1  42/78/-8/30x1
 105 | -
 106 | 36/78/0/30x1  40/78/12/30x1
 107 | -
 108 | 40/120/12/30x1
 109 | -
 110 | 36/78/0/30x1
 111 | -
 112 | 36/120/0/30x1  46/120/-8/200x1
 113 | -
 114 | 36/78/0/30x1  40/78/12/30x1
 115 | -
 116 | 36/78/0/30x1  40/120/12/30x1  46/120/-8/200x1
 117 | -
 118 | 36/78/0/30x1
 119 | -
 120 | 36/120/0/30x1  46/120/-8/200x1
 121 | -
 122 | 42/78/-8/30x1
 123 | -
 124 | 36/78/0/30x1  40/120/12/30x1  46/78/-8/200x1
 125 | -
 126 | 40/78/12/30x1  42/78/-8/30x1
 127 | -"""


-- | The frozen render of `Reef.Conformance.balistesInputRun` — a scripted
-- | tick-tagged session through the codec + stepBal + renderStep. Identical node ↔ BEAM.
balistesInputGolden :: String
balistesInputGolden = """   0 x128 y128 r  0 o 70 | 36/120/0/30x1
   1 x128 y128 r  0 o 70 | -
   2 x128 y128 r  0 o 70 | -
   3 x128 y128 r  0 o 70 | -
   4 x128 y128 r  0 o 70 | 46/120/0/200x1
   5 x200 y 60 r  0 o 70 | -
   6 x200 y 60 r  0 o 70 | 36/78/0/30x1
   7 x200 y 60 r  0 o 70 | -
   8 x200 y 60 r  0 o 70 | 42/78/0/30x1
   9 x200 y 60 r  0 o 70 | -
  10 x200 y 60 r  0 o 70 | 36/78/0/30x1
  11 x200 y 60 r  0 o 70 | -
  12 x200 y 60 r  0 o 70 | 38/120/0/30x1
  13 x200 y 60 r  0 o 70 | -
  14 x200 y 60 r  0 o 70 | -
  15 x200 y 60 r  0 o 70 | -
  16 x200 y 60 r  0 o 70 | 36/120/0/30x1  46/120/0/200x1
  17 x200 y 60 r  0 o 70 | -
  18 x200 y 60 r  0 o 70 | 36/78/0/30x1
  19 x200 y 60 r  0 o 70 | -
  20 x200 y 60 r180 o120 | 36/78/0/30x1  38/78/0/30x1  46/120/0/200x1
  21 x200 y 60 r180 o120 | -
  22 x200 y 60 r180 o120 | 36/78/0/30x1
  23 x200 y 60 r180 o120 | -
  24 x200 y 60 r180 o120 | 36/78/0/30x1  46/120/0/200x1
  25 x200 y 60 r180 o120 | -
  26 x200 y 60 r180 o120 | -
  27 x200 y 60 r180 o120 | -
  28 x200 y 60 r180 o120 | 36/78/0/30x1  38/78/0/30x1  46/78/0/200x1
  29 x200 y 60 r180 o120 | -
  30 x200 y 60 r180 o120 | 38/78/0/30x1
  31 x200 y 60 r180 o120 | -
  32 x200 y 60 r180 o120 | 36/120/0/30x1  46/78/0/200x1
  33 x200 y 60 r180 o120 | -
  34 x200 y 60 r180 o120 | -
  35 x200 y 60 r180 o120 | -
  36 x200 y 60 r180 o120 | 38/120/0/30x1
  37 x200 y 60 r180 o120 | -
  38 x200 y 60 r180 o120 | 36/78/0/30x1
  39 x200 y 60 r180 o120 | -
  40 x200 y 60 r180 o120 | 36/78/0/30x1  42/78/0/30x1
  41 x200 y 60 r180 o120 | -
  42 x200 y 60 r180 o120 | 36/78/0/30x1  38/78/12/30x1
  43 x200 y 60 r180 o120 | -
  44 x200 y 60 r180 o120 | 38/120/12/30x1
  45 x200 y 60 r180 o120 | -
  46 x200 y 60 r180 o120 | -
  47 x200 y 60 r180 o120 | -
  48 x200 y 60 r180 o120 | 36/120/0/30x1  46/120/0/200x1
  49 x200 y 60 r180 o120 | -
  50 x200 y 60 r180 o120 | 36/78/0/30x1
  51 x200 y 60 r180 o120 | -
  52 x200 y 60 r180 o120 | 36/78/0/30x1  38/120/12/30x1  46/120/0/200x1
  53 x200 y 60 r180 o120 | -
  54 x200 y 60 r180 o120 | 36/78/0/30x1
  55 x200 y 60 r180 o120 | -
  56 x200 y 60 r180 o120 | 36/78/0/30x1  46/120/0/200x1
  57 x200 y 60 r180 o120 | -
  58 x200 y 60 r180 o120 | -
  59 x200 y 60 r180 o120 | -
  60 x200 y 60 r180 o120 | 36/78/0/30x1  40/120/12/30x1  46/78/0/200x1
  61 x200 y 60 r180 o120 | -
  62 x200 y 60 r180 o120 | 40/78/12/30x1
  63 x200 y 60 r180 o120 | -
  64 x200 y 60 r180 o120 | 36/120/0/30x1  46/78/0/200x1
  65 x200 y 60 r180 o120 | -
  66 x200 y 60 r180 o120 | -
  67 x200 y 60 r180 o120 | -
  68 x200 y 60 r180 o120 | 40/120/12/30x1
  69 x200 y 60 r180 o120 | -
  70 x200 y 60 r180 o120 | 36/78/0/30x1
  71 x200 y 60 r180 o120 | -
  72 x200 y 60 r180 o120 | 36/78/0/30x1  42/78/0/30x1
  73 x200 y 60 r180 o120 | -
  74 x200 y 60 r180 o120 | 36/78/0/30x1
  75 x200 y 60 r180 o120 | -
  76 x200 y 60 r180 o120 | 40/120/12/30x1
  77 x200 y 60 r180 o120 | -
  78 x200 y 60 r180 o120 | -
  79 x200 y 60 r180 o120 | -
  80 x200 y 60 r180 o120 | 36/120/0/30x1  46/120/0/200x1
  81 x200 y 60 r180 o120 | -
  82 x200 y 60 r180 o120 | 36/78/0/30x1
  83 x200 y 60 r180 o120 | -
  84 x200 y 60 r180 o120 | 36/78/0/30x1  40/78/12/30x1  46/120/0/200x1
  85 x200 y 60 r180 o120 | -
  86 x200 y 60 r180 o120 | 36/78/0/30x1
  87 x200 y 60 r180 o120 | -
  88 x200 y 60 r180 o120 | 36/78/0/30x1  46/120/0/200x1
  89 x200 y 60 r180 o120 | -
  90 x200 y 60 r180 o120 | 36/120/0/30x1  46/78/0/200x1
  91 x200 y 60 r180 o120 | -
  92 x200 y 60 r180 o120 | -
  93 x200 y 60 r180 o120 | -
  94 x200 y 60 r180 o120 | 36/78/0/30x1  40/120/12/30x1
  95 x200 y 60 r180 o120 | -
  96 x200 y 60 r180 o120 | 36/78/0/30x1
  97 x200 y 60 r180 o120 | -
  98 x200 y 60 r180 o120 | 36/78/0/30x1  42/78/0/30x1
  99 x200 y 60 r180 o120 | -
 100 x200 y 60 r180 o120 | 36/78/0/30x1
 101 x200 y 60 r180 o120 | -
 102 x200 y 60 r180 o120 | 40/120/12/30x1
 103 x200 y 60 r180 o120 | -
 104 x200 y 60 r180 o120 | 36/78/0/30x1
 105 x200 y 60 r180 o120 | -
 106 x200 y 60 r180 o120 | 36/120/0/30x1  46/120/0/200x1
 107 x200 y 60 r180 o120 | -
 108 x200 y 60 r180 o120 | 36/78/0/30x1
 109 x200 y 60 r180 o120 | -
 110 x200 y 60 r180 o120 | 36/78/0/30x1  40/78/12/30x1  46/120/0/200x1
 111 x200 y 60 r180 o120 | -
 112 x200 y 60 r180 o120 | 36/78/0/30x1
 113 x200 y 60 r180 o120 | -
 114 x200 y 60 r180 o120 | 36/120/0/30x1  46/120/0/200x1
 115 x200 y 60 r180 o120 | -
 116 x200 y 60 r180 o120 | 36/78/0/30x1
 117 x200 y 60 r180 o120 | -
 118 x200 y 60 r180 o120 | 36/78/0/30x1  40/78/12/30x1  46/78/0/200x1
 119 x200 y 60 r180 o120 | 36/78/0/30x1
 120 x200 y 60 r180 o120 | 36/78/0/30x1  40/78/12/30x1  42/78/0/30x1
 121 x200 y 60 r180 o120 | -
 122 x200 y 60 r180 o120 | 36/120/0/30x1  46/78/0/200x1
 123 x200 y 60 r180 o120 | -
 124 x200 y 60 r180 o120 | 42/78/0/30x1
 125 x200 y 60 r180 o120 | -
 126 x200 y 60 r180 o120 | 40/120/12/30x1  42/78/0/30x3
 127 x200 y 60 r180 o120 | 42/78/0/30x1
 128 x200 y 60 r180 o120 | 36/78/0/30x1  42/78/0/30x1
 129 x200 y 60 r180 o120 | 42/78/0/30x1
 130 x200 y 60 r180 o120 | 36/78/0/30x1  42/78/0/30x1
 131 x200 y 60 r180 o120 | -
 132 x200 y 60 r180 o120 | 36/78/0/30x1  40/78/12/30x1  42/78/0/30x1
 133 x200 y 60 r180 o120 | -
 134 x200 y 60 r180 o120 | 40/120/12/30x1  42/78/0/30x1
 135 x200 y 60 r180 o120 | -
 136 x200 y 60 r180 o120 | -
 137 x200 y 60 r180 o120 | -
 138 x200 y 60 r180 o120 | 36/120/0/30x1  46/120/0/200x1
 139 x200 y 60 r180 o120 | -
 140 x200 y 60 r180 o120 | 36/78/0/30x1  42/78/0/30x1
 141 x200 y 60 r180 o120 | -
 142 x200 y 60 r180 o120 | 36/78/0/30x1  40/120/12/30x1  46/120/0/200x1
 143 x200 y 60 r180 o120 | 42/78/0/30x1
 144 x200 y 60 r180 o120 | 36/78/0/30x1  42/78/0/30x1
 145 x200 y 60 r180 o120 | 42/78/0/30x1
 146 x200 y 60 r180 o120 | 36/78/0/30x1  46/120/0/200x1
 147 x200 y 60 r180 o120 | -
 148 x200 y 60 r180 o120 | 42/78/0/30x1
 149 x200 y 60 r180 o120 | -
 150 x200 y200 r180 o120 | 36/78/0/30x1  46/120/0/200x1
 151 x200 y200 r180 o120 | -
 152 x200 y200 r180 o120 | 36/78/0/30x1  40/78/12/30x1  42/78/0/30x1
 153 x200 y200 r180 o120 | -
 154 x200 y200 r180 o120 | 36/120/0/30x1  46/78/0/200x1
 155 x200 y200 r180 o120 | 36/78/0/30x1
 156 x200 y200 r180 o120 | 36/78/0/30x1  40/78/12/30x1  42/78/0/30x1
 157 x200 y200 r180 o120 | 36/78/0/30x1
 158 x200 y200 r180 o120 | 36/78/0/30x1  46/120/0/200x3
 159 x200 y200 r180 o120 | 36/78/0/30x1
 160 x200 y200 r180 o120 | 36/78/0/30x1  42/78/0/30x1
 161 x200 y200 r180 o120 | 36/78/0/30x1
 162 x200 y200 r180 o120 | 36/120/0/30x1  40/120/12/30x1  46/78/0/200x1
 163 x200 y200 r180 o120 | 36/78/0/30x1
 164 x200 y200 r180 o120 | 36/78/0/30x1  46/78/0/200x1
 165 x200 y200 r180 o120 | 36/78/0/30x1
 166 x200 y200 r180 o120 | 36/78/0/30x1  40/78/12/30x1  46/78/0/200x1
 167 x200 y200 r180 o120 | 36/78/0/30x1
 168 x200 y200 r180 o120 | 36/78/0/30x1  42/78/0/30x1
 169 x200 y200 r180 o120 | 36/78/0/30x1
 170 x200 y200 r180 o120 | 36/120/0/30x1  46/78/0/200x1
 171 x200 y200 r180 o120 | 36/78/0/30x1
 172 x200 y200 r180 o120 | 36/78/0/30x1  40/78/12/30x1  42/78/0/30x1
 173 x200 y200 r180 o120 | 36/78/0/30x1
 174 x200 y200 r180 o120 | 36/78/0/30x1  46/120/0/200x1
 175 x200 y200 r180 o120 | 36/78/0/30x1
 176 x200 y200 r180 o120 | 36/78/0/30x1  42/78/0/30x1
 177 x200 y200 r180 o120 | 36/78/0/30x1
 178 x200 y200 r180 o120 | 36/120/0/30x1  40/120/12/30x1  46/78/0/200x1
 179 x200 y200 r180 o120 | 36/78/0/30x1
 180 x200 y200 r180 o120 | 36/78/0/30x1  40/78/12/30x1  42/78/0/30x1
 181 x200 y200 r180 o120 | 36/78/0/30x1
 182 x200 y200 r180 o120 | 36/78/0/30x1  46/120/0/200x1
 183 x200 y200 r180 o120 | 36/78/0/30x1
 184 x200 y200 r180 o120 | 36/78/0/30x1  40/78/12/30x1  42/78/0/30x1
 185 x200 y200 r180 o120 | 36/78/0/30x1
 186 x200 y200 r180 o120 | 36/120/0/30x1  46/120/0/200x1
 187 x200 y200 r180 o120 | 36/78/0/30x1  42/78/0/30x1
 188 x200 y200 r180 o120 | 36/78/0/30x1  40/78/12/30x1  42/78/0/30x1
 189 x200 y200 r180 o120 | 36/78/0/30x1  42/78/0/30x1
 190 x200 y200 r180 o120 | 36/78/0/30x1  46/120/0/200x3
 191 x200 y200 r180 o120 | 36/78/0/30x1  42/78/0/30x1
 192 x200 y200 r180 o120 | 36/78/0/30x1  42/78/0/30x1
 193 x200 y200 r180 o120 | 36/78/0/30x1  42/78/0/30x1
 194 x200 y200 r180 o120 | 36/120/0/30x1  40/120/12/30x1  46/78/0/200x1
 195 x200 y200 r180 o120 | 36/78/0/30x1  42/78/0/30x1
 196 x200 y200 r180 o120 | 36/78/0/30x1  46/78/0/200x1
 197 x200 y200 r180 o120 | 36/78/0/30x1  42/78/0/30x1
 198 x200 y200 r180 o120 | 36/78/0/30x1  40/78/12/30x1  46/120/0/200x1
 199 x200 y200 r180 o120 | 36/78/0/30x1  42/78/0/30x1"""

-- | The frozen render of `Reef.Conformance.fixedRun` — a fixed rhythm through the
-- | codec + renderFixed over 8 loops. Identical node ↔ BEAM.
fixedGolden :: String
fixedGolden = """   0 | 36/110/0/55x1  40/70/0/55x1
   1 | -
   2 | 40/70/0/55x1
   3 | -
   4 | 36/110/0/55x1  37/100/0/55x1  40/70/0/55x1
   5 | -
   6 | 40/70/0/55x1
   7 | -
   8 | 36/110/0/55x1  40/70/0/55x1
   9 | -
  10 | 40/70/0/55x1
  11 | -
  12 | 36/110/0/55x1  37/100/0/55x3  40/70/0/55x1
  13 | -
  14 | 40/70/0/55x1
  15 | -
  16 | 36/110/0/55x1  40/70/0/55x1
  17 | -
  18 | 40/70/0/55x1
  19 | -
  20 | 36/110/0/55x1  37/100/0/55x1  40/70/0/55x1
  21 | -
  22 | 40/70/0/55x1
  23 | -
  24 | 36/110/0/55x1  40/70/0/55x1
  25 | -
  26 | 40/70/0/55x1
  27 | -
  28 | 36/110/0/55x1  37/100/0/55x3  40/70/0/55x1
  29 | -
  30 | 40/70/0/55x1  42/90/0/180x1
  31 | -
  32 | 36/110/0/55x1  40/70/0/55x1
  33 | -
  34 | 40/70/0/55x1
  35 | -
  36 | 36/110/0/55x1  37/100/0/55x1  40/70/0/55x1
  37 | -
  38 | 40/70/0/55x1
  39 | -
  40 | 36/110/0/55x1  40/70/0/55x1
  41 | -
  42 | 40/70/0/55x1
  43 | -
  44 | 36/110/0/55x1  37/100/0/55x3  40/70/0/55x1
  45 | -
  46 | 40/70/0/55x1
  47 | -
  48 | 36/110/0/55x1  40/70/0/55x1
  49 | -
  50 | 40/70/0/55x1
  51 | -
  52 | 36/110/0/55x1  37/100/0/55x1  40/70/0/55x1
  53 | -
  54 | 40/70/0/55x1
  55 | -
  56 | 36/110/0/55x1  40/70/0/55x1
  57 | -
  58 | 40/70/0/55x1
  59 | -
  60 | 36/110/0/55x1  37/100/0/55x3  40/70/0/55x1
  61 | -
  62 | 40/70/0/55x1  42/90/0/180x1
  63 | -
  64 | 36/110/0/55x1  40/70/0/55x1
  65 | -
  66 | 40/70/0/55x1
  67 | -
  68 | 36/110/0/55x1  37/100/0/55x1  40/70/0/55x1
  69 | -
  70 | 40/70/0/55x1
  71 | -
  72 | 36/110/0/55x1  40/70/0/55x1
  73 | -
  74 | 40/70/0/55x1
  75 | -
  76 | 36/110/0/55x1  37/100/0/55x3  40/70/0/55x1
  77 | -
  78 | 40/70/0/55x1
  79 | -
  80 | 36/110/0/55x1  40/70/0/55x1
  81 | -
  82 | 40/70/0/55x1
  83 | -
  84 | 36/110/0/55x1  37/100/0/55x1  40/70/0/55x1
  85 | -
  86 | 40/70/0/55x1
  87 | -
  88 | 36/110/0/55x1  40/70/0/55x1
  89 | -
  90 | 40/70/0/55x1
  91 | -
  92 | 36/110/0/55x1  37/100/0/55x3  40/70/0/55x1
  93 | -
  94 | 40/70/0/55x1  42/90/0/180x1
  95 | -
  96 | 36/110/0/55x1  40/70/0/55x1
  97 | -
  98 | 40/70/0/55x1
  99 | -
 100 | 36/110/0/55x1  37/100/0/55x1  40/70/0/55x1
 101 | -
 102 | 40/70/0/55x1
 103 | -
 104 | 36/110/0/55x1  40/70/0/55x1
 105 | -
 106 | 40/70/0/55x1
 107 | -
 108 | 36/110/0/55x1  37/100/0/55x3  40/70/0/55x1
 109 | -
 110 | -
 111 | -
 112 | 36/110/0/55x1  40/70/0/55x1
 113 | -
 114 | 40/70/0/55x1
 115 | -
 116 | 36/110/0/55x1  37/100/0/55x1  40/70/0/55x1
 117 | -
 118 | 40/70/0/55x1
 119 | -
 120 | 36/110/0/55x1  40/70/0/55x1
 121 | -
 122 | 40/70/0/55x1
 123 | -
 124 | 36/110/0/55x1  37/100/0/55x3
 125 | -
 126 | 42/90/0/180x1
 127 | -"""

-- | The frozen Vetula performance-scheduler golden (V1). `vetulaRun` round-trips a
-- | representative performance through the codec then evaluates the shared
-- | Reef.Vetula.Perf scheduler at each absolute pulse over 128 pulses. Pure integer
-- | arithmetic (no seed/float/bits), proven byte-identical under node and the BEAM
-- | (conformance/cross-runtime.sh) — the precondition for reef_vetula_voice to
-- | conduct the rig's Odonus in lockstep with the browser.
vetulaGolden :: String
vetulaGolden = """   0 | 0 0 0 0 0 | odo0 [0,4,7]
   1 | 0 0 0 0 0 | odo0 [0,4,7]
   2 | 0 0 0 0 0 | odo0 [0,4,7]
   3 | 0 0 0 0 0 | odo0 [0,4,7]
   4 | 0 0 0 0 0 | odo0 [0,4,7]
   5 | 0 0 0 0 0 | odo0 [0,4,7]
   6 | 0 0 0 0 0 | odo0 [0,4,7]
   7 | 0 0 0 0 0 | odo0 [0,4,7]
   8 | 0 0 0 0 1 | odo0 [0,4,7]
   9 | 0 0 0 0 1 | odo0 [0,4,7]
  10 | 0 0 0 0 1 | odo0 [0,4,7]
  11 | 0 0 0 0 1 | odo0 [0,4,7]
  12 | 0 0 0 0 1 | odo0 [0,4,7]
  13 | 0 0 0 0 1 | odo0 [0,4,7]
  14 | 0 0 0 0 1 | odo0 [0,4,7]
  15 | 0 0 0 0 1 | odo0 [0,4,7]
  16 | 0 0 1 1 1 | odo1 [4,7,11]
  17 | 0 0 1 1 1 | odo1 [4,7,11]
  18 | 0 0 1 1 1 | odo1 [4,7,11]
  19 | 0 0 1 1 1 | odo1 [4,7,11]
  20 | 0 0 1 1 1 | odo1 [4,7,11]
  21 | 0 0 1 1 1 | odo1 [4,7,11]
  22 | 0 0 1 1 1 | odo1 [4,7,11]
  23 | 0 0 1 1 1 | odo1 [4,7,11]
  24 | 0 0 1 1 2 | odo1 [4,7,11]
  25 | 0 0 1 1 2 | odo1 [4,7,11]
  26 | 0 0 1 1 2 | odo1 [4,7,11]
  27 | 0 0 1 1 2 | odo1 [4,7,11]
  28 | 0 0 1 1 2 | odo1 [4,7,11]
  29 | 0 0 1 1 2 | odo1 [4,7,11]
  30 | 0 0 1 1 2 | odo1 [4,7,11]
  31 | 0 0 1 1 2 | odo1 [4,7,11]
  32 | 1 2 2 2 2 | odo2 [9,0,4]
  33 | 1 2 2 2 2 | odo2 [9,0,4]
  34 | 1 2 2 2 2 | odo2 [9,0,4]
  35 | 1 2 2 2 2 | odo2 [9,0,4]
  36 | 1 2 2 2 2 | odo2 [9,0,4]
  37 | 1 2 2 2 2 | odo2 [9,0,4]
  38 | 1 2 2 2 2 | odo2 [9,0,4]
  39 | 1 2 2 2 2 | odo2 [9,0,4]
  40 | 1 2 2 2 3 | odo2 [9,0,4]
  41 | 1 2 2 2 3 | odo2 [9,0,4]
  42 | 1 2 2 2 3 | odo2 [9,0,4]
  43 | 1 2 2 2 3 | odo2 [9,0,4]
  44 | 1 2 2 2 3 | odo2 [9,0,4]
  45 | 1 2 2 2 3 | odo2 [9,0,4]
  46 | 1 2 2 2 3 | odo2 [9,0,4]
  47 | 1 2 2 2 3 | odo2 [9,0,4]
  48 | 1 3 3 3 3 | odo3 [11,2,6]
  49 | 1 3 3 3 3 | odo3 [11,2,6]
  50 | 1 3 3 3 3 | odo3 [11,2,6]
  51 | 1 3 3 3 3 | odo3 [11,2,6]
  52 | 1 3 3 3 3 | odo3 [11,2,6]
  53 | 1 3 3 3 3 | odo3 [11,2,6]
  54 | 1 3 3 3 3 | odo3 [11,2,6]
  55 | 1 3 3 3 3 | odo3 [11,2,6]
  56 | 1 3 3 3 0 | odo3 [11,2,6]
  57 | 1 3 3 3 0 | odo3 [11,2,6]
  58 | 1 3 3 3 0 | odo3 [11,2,6]
  59 | 1 3 3 3 0 | odo3 [11,2,6]
  60 | 1 3 3 3 0 | odo3 [11,2,6]
  61 | 1 3 3 3 0 | odo3 [11,2,6]
  62 | 1 3 3 3 0 | odo3 [11,2,6]
  63 | 1 3 3 3 0 | odo3 [11,2,6]
  64 | 2 0 0 0 0 | odo0 [0,4,7]
  65 | 2 0 0 0 0 | odo0 [0,4,7]
  66 | 2 0 0 0 0 | odo0 [0,4,7]
  67 | 2 0 0 0 0 | odo0 [0,4,7]
  68 | 2 0 0 0 0 | odo0 [0,4,7]
  69 | 2 0 0 0 0 | odo0 [0,4,7]
  70 | 2 0 0 0 0 | odo0 [0,4,7]
  71 | 2 0 0 0 0 | odo0 [0,4,7]
  72 | 2 0 0 0 1 | odo0 [0,4,7]
  73 | 2 0 0 0 1 | odo0 [0,4,7]
  74 | 2 0 0 0 1 | odo0 [0,4,7]
  75 | 2 0 0 0 1 | odo0 [0,4,7]
  76 | 2 0 0 0 1 | odo0 [0,4,7]
  77 | 2 0 0 0 1 | odo0 [0,4,7]
  78 | 2 0 0 0 1 | odo0 [0,4,7]
  79 | 2 0 0 0 1 | odo0 [0,4,7]
  80 | 2 0 1 1 1 | odo1 [4,7,11]
  81 | 2 0 1 1 1 | odo1 [4,7,11]
  82 | 2 0 1 1 1 | odo1 [4,7,11]
  83 | 2 0 1 1 1 | odo1 [4,7,11]
  84 | 2 0 1 1 1 | odo1 [4,7,11]
  85 | 2 0 1 1 1 | odo1 [4,7,11]
  86 | 2 0 1 1 1 | odo1 [4,7,11]
  87 | 2 0 1 1 1 | odo1 [4,7,11]
  88 | 2 0 1 1 2 | odo1 [4,7,11]
  89 | 2 0 1 1 2 | odo1 [4,7,11]
  90 | 2 0 1 1 2 | odo1 [4,7,11]
  91 | 2 0 1 1 2 | odo1 [4,7,11]
  92 | 2 0 1 1 2 | odo1 [4,7,11]
  93 | 2 0 1 1 2 | odo1 [4,7,11]
  94 | 2 0 1 1 2 | odo1 [4,7,11]
  95 | 2 0 1 1 2 | odo1 [4,7,11]
  96 | 3 2 2 2 2 | odo2 [9,0,4]
  97 | 3 2 2 2 2 | odo2 [9,0,4]
  98 | 3 2 2 2 2 | odo2 [9,0,4]
  99 | 3 2 2 2 2 | odo2 [9,0,4]
 100 | 3 2 2 2 2 | odo2 [9,0,4]
 101 | 3 2 2 2 2 | odo2 [9,0,4]
 102 | 3 2 2 2 2 | odo2 [9,0,4]
 103 | 3 2 2 2 2 | odo2 [9,0,4]
 104 | 3 2 2 2 3 | odo2 [9,0,4]
 105 | 3 2 2 2 3 | odo2 [9,0,4]
 106 | 3 2 2 2 3 | odo2 [9,0,4]
 107 | 3 2 2 2 3 | odo2 [9,0,4]
 108 | 3 2 2 2 3 | odo2 [9,0,4]
 109 | 3 2 2 2 3 | odo2 [9,0,4]
 110 | 3 2 2 2 3 | odo2 [9,0,4]
 111 | 3 2 2 2 3 | odo2 [9,0,4]
 112 | 3 3 3 3 3 | odo3 [11,2,6]
 113 | 3 3 3 3 3 | odo3 [11,2,6]
 114 | 3 3 3 3 3 | odo3 [11,2,6]
 115 | 3 3 3 3 3 | odo3 [11,2,6]
 116 | 3 3 3 3 3 | odo3 [11,2,6]
 117 | 3 3 3 3 3 | odo3 [11,2,6]
 118 | 3 3 3 3 3 | odo3 [11,2,6]
 119 | 3 3 3 3 3 | odo3 [11,2,6]
 120 | 3 3 3 3 0 | odo3 [11,2,6]
 121 | 3 3 3 3 0 | odo3 [11,2,6]
 122 | 3 3 3 3 0 | odo3 [11,2,6]
 123 | 3 3 3 3 0 | odo3 [11,2,6]
 124 | 3 3 3 3 0 | odo3 [11,2,6]
 125 | 3 3 3 3 0 | odo3 [11,2,6]
 126 | 3 3 3 3 0 | odo3 [11,2,6]
 127 | 3 3 3 3 0 | odo3 [11,2,6]"""

-- | The frozen scale-pattern golden (`scaleRun`): dorian and lydian alternating
-- | every eight steps, the root to D at 12, a rest from 20, a hand toggle at 26
-- | (adds the 11), the pattern again at 30, let go at 36.
scaleGolden :: String
scaleGolden = """  1 | h0 p50 d1 r1 v100 | k0 [0,2,3,5,7,9,10] <dorian lydian>
  2 | h0 p51 d1 r1 v100 | k0 [0,2,3,5,7,9,10] <dorian lydian>
  3 | h0 p55 d1 r1 v100 | k0 [0,2,3,5,7,9,10] <dorian lydian>
  4 | h0 p57 d1 r1 v100 | k0 [0,2,3,5,7,9,10] <dorian lydian>
  5 | h0 p58 d1 r1 v100 | k0 [0,2,3,5,7,9,10] <dorian lydian>
  6 | h0 p62 d1 r1 v100 | k0 [0,2,3,5,7,9,10] <dorian lydian>
  7 | h0 p63 d1 r1 v100 | k0 [0,2,3,5,7,9,10] <dorian lydian>
  8 | h0 p67 d1 r1 v100 | k0 [0,2,4,6,7,9,11] <dorian lydian>
  9 | h0 p69 d1 r1 v100 | k0 [0,2,4,6,7,9,11] <dorian lydian>
 10 | h0 p71 d1 r1 v100 | k0 [0,2,4,6,7,9,11] <dorian lydian>
 11 | h0 p74 d1 r1 v100 | k0 [0,2,4,6,7,9,11] <dorian lydian>
 12 | h0 p78 d1 r1 v100 | k2 [0,2,4,6,7,9,11] <dorian lydian>
 13 | h0 p81 d1 r1 v100 | k2 [0,2,4,6,7,9,11] <dorian lydian>
 14 | h0 p83 d1 r1 v100 | k2 [0,2,4,6,7,9,11] <dorian lydian>
 15 | h0 p85 d1 r1 v100 | k2 [0,2,4,6,7,9,11] <dorian lydian>
 16 | h0 p50 d1 r1 v100 | k2 [0,2,3,5,7,9,10] <dorian lydian>
 17 | h0 p52 d1 r1 v100 | k2 [0,2,3,5,7,9,10] <dorian lydian>
 18 | h0 p53 d1 r1 v100 | k2 [0,2,3,5,7,9,10] <dorian lydian>
 19 | h0 p57 d1 r1 v100 | k2 [0,2,3,5,7,9,10] <dorian lydian>
 20 | h0 p59 d1 r1 v100 | k2 [0,2,3,5,7,9,10] ~
 21 | h0 p60 d1 r1 v100 | k2 [0,2,3,5,7,9,10] ~
 22 | h0 p64 d1 r1 v100 | k2 [0,2,3,5,7,9,10] ~
 23 | h0 p65 d1 r1 v100 | k2 [0,2,3,5,7,9,10] ~
 24 | h0 p69 d1 r1 v100 | k2 [0,2,3,5,7,9,10] ~
 25 | h0 p71 d1 r1 v100 | k2 [0,2,3,5,7,9,10] ~
 26 | h0 p73 d1 r1 v100 | k2 [0,2,3,5,7,9,10,11] -
 27 | h0 p76 d1 r1 v100 | k2 [0,2,3,5,7,9,10,11] -
 28 | h0 p79 d1 r1 v100 | k2 [0,2,3,5,7,9,10,11] -
 29 | h0 p81 d1 r1 v100 | k2 [0,2,3,5,7,9,10,11] -
 30 | h0 p83 d1 r1 v100 | k2 [0,2,4,6,7,9,11] <dorian lydian>
 31 | h0 p85 d1 r1 v100 | k2 [0,2,4,6,7,9,11] <dorian lydian>
 32 | h0 p50 d1 r1 v100 | k2 [0,2,3,5,7,9,10] <dorian lydian>
 33 | h0 p52 d1 r1 v100 | k2 [0,2,3,5,7,9,10] <dorian lydian>
 34 | h0 p53 d1 r1 v100 | k2 [0,2,3,5,7,9,10] <dorian lydian>
 35 | h0 p57 d1 r1 v100 | k2 [0,2,3,5,7,9,10] <dorian lydian>
 36 | h0 p60 d1 r1 v100 | k2 [0,2,3,5,7,9,10,11] -
 37 | h0 p61 d1 r1 v100 | k2 [0,2,3,5,7,9,10,11] -
 38 | h0 p64 d1 r1 v100 | k2 [0,2,3,5,7,9,10,11] -
 39 | h0 p67 d1 r1 v100 | k2 [0,2,3,5,7,9,10,11] -
 40 | h0 p69 d1 r1 v100 | k2 [0,2,3,5,7,9,10,11] -"""

-- | The frozen harmony golden (`harmonyRun`): C major and E minor alternating
-- | every eight steps, a rest on the C-minor scale at 20-25, cleared from 34.
harmonyGolden :: String
harmonyGolden = """  1 | h0 p52 d1 r1 v100
  2 | h0 p52 d1 r1 v100
  3 | h0 p55 d1 r1 v100
  4 | h0 p55 d1 r1 v100
  5 | h0 p60 d1 r1 v100
  6 | h0 p64 d1 r1 v100
  7 | h0 p64 d1 r1 v100
  8 | h0 p67 d1 r1 v100
  9 | h0 p67 d1 r1 v100
 10 | h0 p71 d1 r1 v100
 11 | h0 p76 d1 r1 v100
 12 | h0 p76 d1 r1 v100
 13 | h0 p79 d1 r1 v100
 14 | h0 p79 d1 r1 v100
 15 | h0 p83 d1 r1 v100
 16 | h0 p48 d1 r1 v100
 17 | h0 p52 d1 r1 v100
 18 | h0 p52 d1 r1 v100
 19 | h0 p55 d1 r1 v100
 20 | h0 p56 d1 r1 v100
 21 | h0 p58 d1 r1 v100
 22 | h0 p62 d1 r1 v100
 23 | h0 p63 d1 r1 v100
 24 | h0 p67 d1 r1 v100
 25 | h0 p68 d1 r1 v100
 26 | h0 p71 d1 r1 v100
 27 | h0 p76 d1 r1 v100
 28 | h0 p76 d1 r1 v100
 29 | h0 p79 d1 r1 v100
 30 | h0 p79 d1 r1 v100
 31 | h0 p83 d1 r1 v100
 32 | h0 p48 d1 r1 v100
 33 | h0 p52 d1 r1 v100
 34 | h0 p51 d1 r1 v100
 35 | h0 p55 d1 r1 v100
 36 | h0 p56 d1 r1 v100
 37 | h0 p58 d1 r1 v100
 38 | h0 p62 d1 r1 v100
 39 | h0 p63 d1 r1 v100
 40 | h0 p67 d1 r1 v100"""

-- | The frozen chord-quantised render golden. `chordRun` feeds a C-major triad via
-- | mkFollowChord (chord overlay on) and renders 32 steps of sounding pitch. Sane
-- | octaves (E3/G3/C4/E4), proven byte-identical node == BEAM (conformance/
-- | cross-runtime.sh). Regression guard for the index-space chord-quantise fix.
chordGolden :: String
chordGolden = """  1 | h0 p52 d1 r1 v100
  2 | h0 p52 d1 r1 v100
  3 | h0 p55 d1 r1 v100
  4 | h0 p55 d1 r1 v100
  5 | h0 p60 d1 r1 v100
  6 | h0 p64 d1 r1 v100
  7 | h0 p64 d1 r1 v100
  8 | h0 p67 d1 r1 v100
  9 | h0 p67 d1 r1 v100
 10 | h0 p72 d1 r1 v100
 11 | h0 p76 d1 r1 v100
 12 | h0 p76 d1 r1 v100
 13 | h0 p79 d1 r1 v100
 14 | h0 p79 d1 r1 v100
 15 | h0 p84 d1 r1 v100
 16 | h0 p48 d1 r1 v100
 17 | h0 p52 d1 r1 v100
 18 | h0 p52 d1 r1 v100
 19 | h0 p55 d1 r1 v100
 20 | h0 p55 d1 r1 v100
 21 | h0 p60 d1 r1 v100
 22 | h0 p64 d1 r1 v100
 23 | h0 p64 d1 r1 v100
 24 | h0 p67 d1 r1 v100
 25 | h0 p67 d1 r1 v100
 26 | h0 p72 d1 r1 v100
 27 | h0 p76 d1 r1 v100
 28 | h0 p76 d1 r1 v100
 29 | h0 p79 d1 r1 v100
 30 | h0 p79 d1 r1 v100
 31 | h0 p84 d1 r1 v100
 32 | h0 p48 d1 r1 v100"""

-- | The frozen Vetula MIDI-render golden (V2a). Block + arp gated notes per pulse,
-- | proven byte-identical node == BEAM (conformance/cross-runtime.sh). Strummed voices
-- | render nothing yet (V2b).
vetulaMidiGolden :: String
vetulaMidiGolden = """   0 | v0:36/82/3136,60/82/3136,64/82/3136,67/82/3136 | v1:36/80/90 | v2:36/84/1568,60/84/1568,64/84/4704,67/84/3136 | v3:- | v4:-
   1 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
   2 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
   3 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
   4 | v0:- | v1:36/80/90 | v2:- | v3:- | v4:-
   5 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
   6 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
   7 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
   8 | v0:- | v1:36/80/90 | v2:- | v3:- | v4:40/82/1568,64/82/1568,67/82/1568,71/82/1568
   9 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  10 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  11 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
  12 | v0:- | v1:36/80/90 | v2:- | v3:- | v4:-
  13 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  14 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  15 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
  16 | v0:- | v1:36/80/90 | v2:40/84/1568,71/84/1568 | v3:- | v4:-
  17 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  18 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  19 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
  20 | v0:- | v1:36/80/90 | v2:- | v3:- | v4:-
  21 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  22 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  23 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
  24 | v0:- | v1:36/80/90 | v2:- | v3:- | v4:33/82/1568,57/82/1568,60/82/1568,64/82/1568
  25 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  26 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  27 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
  28 | v0:- | v1:36/80/90 | v2:- | v3:- | v4:-
  29 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  30 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  31 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
  32 | v0:40/82/3136,64/82/3136,67/82/3136,71/82/3136 | v1:33/80/90 | v2:33/84/1568,57/84/1568,60/84/1568 | v3:- | v4:-
  33 | v0:- | v1:57/80/90 | v2:- | v3:- | v4:-
  34 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  35 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  36 | v0:- | v1:33/80/90 | v2:- | v3:- | v4:-
  37 | v0:- | v1:57/80/90 | v2:- | v3:- | v4:-
  38 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  39 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  40 | v0:- | v1:33/80/90 | v2:- | v3:- | v4:35/82/1568,59/82/1568,62/82/1568,66/82/1568
  41 | v0:- | v1:57/80/90 | v2:- | v3:- | v4:-
  42 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  43 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  44 | v0:- | v1:33/80/90 | v2:- | v3:- | v4:-
  45 | v0:- | v1:57/80/90 | v2:- | v3:- | v4:-
  46 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  47 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  48 | v0:- | v1:35/80/90 | v2:35/84/1568,59/84/1568,62/84/1568,66/84/1568 | v3:- | v4:-
  49 | v0:- | v1:59/80/90 | v2:- | v3:- | v4:-
  50 | v0:- | v1:62/80/90 | v2:- | v3:- | v4:-
  51 | v0:- | v1:66/80/90 | v2:- | v3:- | v4:-
  52 | v0:- | v1:35/80/90 | v2:- | v3:- | v4:-
  53 | v0:- | v1:59/80/90 | v2:- | v3:- | v4:-
  54 | v0:- | v1:62/80/90 | v2:- | v3:- | v4:-
  55 | v0:- | v1:66/80/90 | v2:- | v3:- | v4:-
  56 | v0:- | v1:35/80/90 | v2:- | v3:- | v4:36/82/1568,60/82/1568,64/82/1568,67/82/1568
  57 | v0:- | v1:59/80/90 | v2:- | v3:- | v4:-
  58 | v0:- | v1:62/80/90 | v2:- | v3:- | v4:-
  59 | v0:- | v1:66/80/90 | v2:- | v3:- | v4:-
  60 | v0:- | v1:35/80/90 | v2:- | v3:- | v4:-
  61 | v0:- | v1:59/80/90 | v2:- | v3:- | v4:-
  62 | v0:- | v1:62/80/90 | v2:- | v3:- | v4:-
  63 | v0:- | v1:66/80/90 | v2:- | v3:- | v4:-
  64 | v0:33/82/3136,57/82/3136,60/82/3136,64/82/3136 | v1:36/80/90 | v2:36/84/1568,60/84/1568,64/84/4704,67/84/3136 | v3:- | v4:-
  65 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  66 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  67 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
  68 | v0:- | v1:36/80/90 | v2:- | v3:- | v4:-
  69 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  70 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  71 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
  72 | v0:- | v1:36/80/90 | v2:- | v3:- | v4:40/82/1568,64/82/1568,67/82/1568,71/82/1568
  73 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  74 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  75 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
  76 | v0:- | v1:36/80/90 | v2:- | v3:- | v4:-
  77 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  78 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  79 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
  80 | v0:- | v1:36/80/90 | v2:40/84/1568,71/84/1568 | v3:- | v4:-
  81 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  82 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  83 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
  84 | v0:- | v1:36/80/90 | v2:- | v3:- | v4:-
  85 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  86 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  87 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
  88 | v0:- | v1:36/80/90 | v2:- | v3:- | v4:33/82/1568,57/82/1568,60/82/1568,64/82/1568
  89 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  90 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  91 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
  92 | v0:- | v1:36/80/90 | v2:- | v3:- | v4:-
  93 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  94 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
  95 | v0:- | v1:67/80/90 | v2:- | v3:- | v4:-
  96 | v0:35/82/3136,59/82/3136,62/82/3136,66/82/3136 | v1:33/80/90 | v2:33/84/1568,57/84/1568,60/84/1568 | v3:- | v4:-
  97 | v0:- | v1:57/80/90 | v2:- | v3:- | v4:-
  98 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
  99 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
 100 | v0:- | v1:33/80/90 | v2:- | v3:- | v4:-
 101 | v0:- | v1:57/80/90 | v2:- | v3:- | v4:-
 102 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
 103 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
 104 | v0:- | v1:33/80/90 | v2:- | v3:- | v4:35/82/1568,59/82/1568,62/82/1568,66/82/1568
 105 | v0:- | v1:57/80/90 | v2:- | v3:- | v4:-
 106 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
 107 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
 108 | v0:- | v1:33/80/90 | v2:- | v3:- | v4:-
 109 | v0:- | v1:57/80/90 | v2:- | v3:- | v4:-
 110 | v0:- | v1:60/80/90 | v2:- | v3:- | v4:-
 111 | v0:- | v1:64/80/90 | v2:- | v3:- | v4:-
 112 | v0:- | v1:35/80/90 | v2:35/84/1568,59/84/1568,62/84/1568,66/84/1568 | v3:- | v4:-
 113 | v0:- | v1:59/80/90 | v2:- | v3:- | v4:-
 114 | v0:- | v1:62/80/90 | v2:- | v3:- | v4:-
 115 | v0:- | v1:66/80/90 | v2:- | v3:- | v4:-
 116 | v0:- | v1:35/80/90 | v2:- | v3:- | v4:-
 117 | v0:- | v1:59/80/90 | v2:- | v3:- | v4:-
 118 | v0:- | v1:62/80/90 | v2:- | v3:- | v4:-
 119 | v0:- | v1:66/80/90 | v2:- | v3:- | v4:-
 120 | v0:- | v1:35/80/90 | v2:- | v3:- | v4:36/82/1568,60/82/1568,64/82/1568,67/82/1568
 121 | v0:- | v1:59/80/90 | v2:- | v3:- | v4:-
 122 | v0:- | v1:62/80/90 | v2:- | v3:- | v4:-
 123 | v0:- | v1:66/80/90 | v2:- | v3:- | v4:-
 124 | v0:- | v1:35/80/90 | v2:- | v3:- | v4:-
 125 | v0:- | v1:59/80/90 | v2:- | v3:- | v4:-
 126 | v0:- | v1:62/80/90 | v2:- | v3:- | v4:-
 127 | v0:- | v1:66/80/90 | v2:- | v3:- | v4:-"""


conspicillumGolden :: String
conspicillumGolden = """0 495198 507698
4 258097 264764
0 260347 272847
4 422812 429479
4 482473 489140
2 303406 316226
1 483049 494954
4 291999 298666
2 255104 267925
1 295465 307370
4 310074 316740
4 318892 325558
4 528061 534728
1 434115 446020
2 304238 317059
4 347773 354440
4 532160 538827
4 262790 269456
4 513568 520235
2 276639 289459
4 422318 428985
1 298573 310478
4 339501 346168
2 457722 470543
3 397141 403391
2 424463 437284
2 462355 475175
0 404417 416917
0 415057 427557
0 444987 457487
4 347874 354541
3 385998 392248
0 287653 300153
2 274148 286968
0 374700 387200
1 315683 327588
2 267768 280589
4 422446 429113
2 330905 343726
4 299872 306539
2 435540 448361
4 541541 548208
2 458920 471740
4 310670 317336
1 372431 384336
4 410235 416901
1 354394 366299
1 437946 449851
0 379653 392153
4 261292 267959
0 376974 389474
3 384152 390402
4 270925 277592
4 261913 268579
4 468454 475121
4 449166 455833
2 291134 303955
4 541038 547705
2 370107 382928
0 282364 294864
1 482443 494348
0 380101 392601
4 464196 470862
2 454537 467358"""


conspicillumCloudGolden :: String
conspicillumCloudGolden = """c0 g0 at 0 n 2 b 430248 sp -1000000 gn 800000 lpf 2200 cr 4000000 ps 1500000
c0 g1 at 125000 n 4 b 354495 sp 1000000 gn 800000 lpf 2200 cr 0 ps 0
c0 g2 at 187500 n 4 b 383641 sp 1000000 gn 800000 lpf 2200 cr 0 ps 0
c0 g3 at 375000 n 4 b 374649 sp -1000000 gn 800000 lpf 2200 cr 0 ps 0
c0 g4 at 500000 n 4 b 517943 sp 1000000 gn 800000 lpf 2200 cr 4000000 ps 0
c0 g5 at 625000 n 1 b 466702 sp 1000000 gn 400000 lpf 2200 cr 0 ps 0
c0 g6 at 687500 n 1 b 349481 sp -1000000 gn 400000 lpf 2200 cr 0 ps 1500000
c0 g7 at 875000 n 0 b 464406 sp 1000000 gn 800000 lpf 2200 cr 0 ps 0
c1 g0 at 0 n 4 b 406957 sp 1000000 gn 800000 lpf 2200 cr 4000000 ps 0
c1 g1 at 125000 n 4 b 338590 sp -1000000 gn 800000 lpf 2200 cr 0 ps 0
c1 g2 at 187500 n 2 b 478990 sp 1000000 gn 400000 lpf 2200 cr 0 ps 0
c1 g3 at 375000 n 2 b 346201 sp 1000000 gn 800000 lpf 2200 cr 0 ps 0
c1 g4 at 500000 n 1 b 534282 sp -1000000 gn 400000 lpf 2200 cr 4000000 ps 1500000
c1 g5 at 625000 n 4 b 506075 sp 1000000 gn 800000 lpf 2200 cr 0 ps 0
c1 g6 at 687500 n 0 b 305984 sp 1000000 gn 800000 lpf 2200 cr 0 ps 0
c1 g7 at 875000 n 4 b 417872 sp -1000000 gn 400000 lpf 2200 cr 0 ps 0
c2 g0 at 0 n 4 b 417771 sp 1000000 gn 800000 lpf 2200 cr 4000000 ps 0
c2 g1 at 125000 n 3 b 480057 sp 1000000 gn 800000 lpf 2200 cr 0 ps 0
c2 g2 at 187500 n 0 b 314565 sp -1000000 gn 800000 lpf 2200 cr 0 ps 1500000
c2 g3 at 375000 n 0 b 409771 sp 1000000 gn 400000 lpf 2200 cr 0 ps 0
c2 g4 at 500000 n 1 b 386861 sp 1000000 gn 400000 lpf 2200 cr 4000000 ps 0
c2 g5 at 625000 n 4 b 431375 sp -1000000 gn 800000 lpf 2200 cr 0 ps 0
c2 g6 at 687500 n 0 b 404010 sp 1000000 gn 800000 lpf 2200 cr 0 ps 0
c2 g7 at 875000 n 4 b 388724 sp 1000000 gn 400000 lpf 2200 cr 0 ps 0
c7 g0 at 0 n 2 b 411553 sp 1000000 gn 400000 lpf 2200 cr 4000000 ps 0
c7 g1 at 125000 n 3 b 416989 sp -1000000 gn 800000 lpf 2200 cr 0 ps 0
c7 g2 at 187500 n 3 b 442105 sp 1000000 gn 800000 lpf 2200 cr 0 ps 0
c7 g3 at 375000 n 2 b 431632 sp 1000000 gn 800000 lpf 2200 cr 0 ps 0
c7 g4 at 500000 n 2 b 483461 sp -1000000 gn 800000 lpf 2200 cr 4000000 ps 1500000
c7 g5 at 625000 n 2 b 297198 sp 1000000 gn 800000 lpf 2200 cr 0 ps 0
c7 g6 at 687500 n 4 b 289450 sp 1000000 gn 800000 lpf 2200 cr 0 ps 0
c7 g7 at 875000 n 4 b 420659 sp -1000000 gn 800000 lpf 2200 cr 0 ps 0"""


conspicillumHarmonicGolden :: String
conspicillumHarmonicGolden = """Em7b5  |  502  456  187    0   69  123  126  103  149    0
A7     |  117  312   59    0  208  863  151  135  149    0
Dm     |  140    0  362  251    0   80 1000  902  110    0
Dm6    |  144  102  289  202   45   80  918 1000  149    0"""

-- Every fourth element, starting with the first: the grains that start each
-- beat of a sixteen-grain bar.
everyFourth :: Array Number -> Array Number
everyFourth xs = map _.x (filter (\r -> r.i `mod` 4 == 0) (mapWithIndex (\i x -> { i, x }) xs))

-- | The frozen golden for `outScaleRun` (conformance/outscale-golden.txt).
outScaleGolden :: String
outScaleGolden = """  1 | h0 p50 d1 r1 v100 | out [2,4,5,7,9,11,0] <dorian lydian>@2 -
  2 | h0 p52 d1 r1 v100 | out [2,4,5,7,9,11,0] <dorian lydian>@2 -
  3 | h0 p55 d1 r1 v100 | out [2,4,5,7,9,11,0] <dorian lydian>@2 -
  4 | h0 p57 d1 r1 v100 | out [2,4,5,7,9,11,0] <dorian lydian>@2 -
  5 | h0 p59 d1 r1 v100 | out [2,4,5,7,9,11,0] <dorian lydian>@2 -
  6 | h0 p62 d1 r1 v100 | out [2,4,5,7,9,11,0] <dorian lydian>@2 -
  7 | h0 p64 d1 r1 v100 | out [2,4,5,7,9,11,0] <dorian lydian>@2 -
  8 | h0 p68 d1 r1 v100 | out [2,4,6,8,9,11,1] <dorian lydian>@2 -
  9 | h0 p68 d1 r1 v100 | out [2,4,6,8,9,11,1] <dorian lydian>@2 -
 10 | h0 p71 d1 r1 v100 | out [2,4,6,8,9,11,1] <dorian lydian>@2 -
 11 | h0 p74 d1 r1 v100 | out [2,4,6,8,9,11,1] <dorian lydian>@2 -
 12 | h0 p76 d1 r1 v100 | out [2,4,6,8,9,11,1] <dorian lydian>@2 -
 13 | h0 p80 d1 r1 v100 | out [2,4,6,8,9,11,1] <dorian lydian>@2 -
 14 | h0 p79 d1 r1 v100 | out [0,4,7] - c'maj
 15 | h0 p84 d1 r1 v100 | out [0,4,7] - c'maj
 16 | h0 p48 d1 r1 v100 | out [0,4,7] - c'maj
 17 | h0 p52 d1 r1 v100 | out [0,4,7] - c'maj
 18 | h0 p52 d1 r1 v100 | out [0,4,7] - c'maj
 19 | h0 p55 d1 r1 v100 | out [0,4,7] - c'maj
 20 | h0 p55 d1 r1 v100 | out [0,4,7] - c'maj
 21 | h0 p60 d1 r1 v100 | out [0,4,7] - c'maj
 22 | h0 p62 d1 r1 v100 | out [7,9,10,0,2,4,5] <dorian lydian>@7 -
 23 | h0 p64 d1 r1 v100 | out [7,9,10,0,2,4,5] <dorian lydian>@7 -
 24 | h0 p67 d1 r1 v100 | out [7,9,11,1,2,4,6] <dorian lydian>@7 -
 25 | h0 p69 d1 r1 v100 | out [7,9,11,1,2,4,6] <dorian lydian>@7 -
 26 | h0 p71 d1 r1 v100 | out [7,9,11,1,2,4,6] <dorian lydian>@7 -
 27 | h0 p74 d1 r1 v100 | out [7,9,11,1,2,4,6] <dorian lydian>@7 -
 28 | h0 p76 d1 r1 v100 | out [7,9,11,1,2,4,6] <dorian lydian>@7 -
 29 | h0 p79 d1 r1 v100 | out [7,9,11,1,2,4,6] <dorian lydian>@7 -
 30 | h0 p80 d1 r1 v100 | out [] - -
 31 | h0 p82 d1 r1 v100 | out [] - -
 32 | h0 p48 d1 r1 v100 | out [] - -
 33 | h0 p50 d1 r1 v100 | out [] - -
 34 | h0 p51 d1 r1 v100 | out [] - -
 35 | h0 p55 d1 r1 v100 | out [] - -
 36 | h0 p56 d1 r1 v100 | out [] - -"""

-- | The frozen golden for `routeRun` (conformance/route-golden.txt).
routeGolden :: String
routeGolden = """   0 |  | 
   1 | odonus.grid <- scale "<dorian lydian>/4" d ; odonus.out <- vetula R | SetGridHarmony,ClearPitchSet,SetScalePattern(<dorian lydian>/4),SetRoot@2
   2 | odonus.grid <- scale "dorian" d ; odonus.out <- harmony "<c'maj7 a'min7>/2" | SetGridHarmony,ClearPitchSet,SetScalePattern(dorian),SetRoot@2,SetOutScale@0,SetHarmony(<c'maj7 a'min7>/2)
   3 | odonus.out <- scale "major" g | SetGridHarmony,ClearPitchSet,SetScalePattern,SetHarmony,SetOutScale(major)@7
   4 | odonus.grid <- harmony "c'maj" | SetScalePattern,SetGridHarmony(c'maj),SetHarmony,SetOutScale@0
   5 | refused: no input 'odonus.side' (odonus.grid, odonus.out)
   6 | odonus.grid <- vetula key | 
key d 0 2 3 5 7 9 10 | d 0 2 3 5 7 9 10
key fs 4 7 16 | fs 0 4 7
key c | c 0
key  | refused: a key is ROOT STEPS…: d 0 2 3 5 7 9 10
key q 0 2 | refused: 'q' is not a note name or a pitch class
key a 0 x | refused: a key's steps are numbers: 'x'
fed none->ctx | SetGridHarmony,SetScalePattern,SetPitchSet,SetOutScale@0,SetHarmony(<c'maj e'min>)
fed ctx->ctx' | SetOutScale@0,SetHarmony(<f'maj7 g'dom7>/2)
fed ctx->none | SetGridHarmony,ClearPitchSet,SetScalePattern,SetHarmony,SetOutScale@0
set | odonus.grid <- vetula key ; odonus.out <- vetula Q
feeds odonus.grid <- key d 0 2 3 5 7 9 10 ; odonus.out <- harmony "<c'maj e'min>" | odonus.grid <- key d 0 2 3 5 7 9 10 ; odonus.out <- harmony "<c'maj e'min>"
feeds odonus.grid <- scale "dorian" d ; odonus.out <- harmony "<c'maj7 a'min7>/2" | odonus.grid <- scale "dorian" d ; odonus.out <- harmony "<c'maj7 a'min7>/2"
feeds  | 
feeds odonus.grid <- vetula key | refused: a feed is resolved, not a Vetula source: 'vetula key'
feeds odonus.out <- key q 0 | refused: 'q' is not a note name or a pitch class
feeds odonus.grid <- scale "minor" e | odonus.grid <- scale "minor" e
line odonus.grid <- vetula 2 | odonus.grid <- vetula Q ; odonus.out <- vetula R
line odonus.out <- none | odonus.grid <- vetula key
line odonus.grid <- none | odonus.out <- vetula R
line odonus.out <- harmony "<c'maj9 a'min11>/2" | odonus.grid <- vetula key ; odonus.out <- harmony "<c'maj9 a'min11>/2"
line odonus.side <- vetula 1 | refused: no input 'odonus.side' (odonus.grid, odonus.out)
line odonus.grid <- nothing | refused: no source 'nothing' (scale "…" ROOT, harmony "…", chromatic, vetula key, vetula P..W)
line odonus.grid | refused: a route is INPUT <- SOURCE: 'odonus.grid'
line odonus.out <- chromatic | odonus.grid <- vetula key ; odonus.out <- chromatic
line odonus.grid <- scale "chromatic" | odonus.grid <- chromatic ; odonus.out <- vetula R"""

-- | The frozen golden for `durRun` (conformance/dur-golden.txt).
durGolden :: String
durGolden = """  1 | h0 p50 d2 r1 v100 | 1/2 1/0
  2 | - | 1/1 1/0
  3 | h0 p51 d1 r1 v100 | 2/1 2/0
  4 | h0 p55 d4 r1 v100 | 3/4 2/0
  5 | - | 3/3 3/0
  6 | - | 3/2 3/0
  7 | - | 3/1 7/0
  8 | h0 p56 d1 r1 v100 | 4/1 7/0
  9 | - | 5/3 6/0
 10 | - | 5/2 6/0
 11 | - | 5/1 5/0
 12 | h0 p62 d2 r1 v100 | 6/2 5/0
 13 | - | 6/1 4/0
 14 | h0 p63 d1 r1 v100 | 7/1 4/0
 15 | h0 p67 d2 r1 v100 | 8/2 8/0
 16 | - | 8/1 8/0
 17 | h0 p70 d1 r1 v100 | 10/1 10/0
 18 | h0 p74 d2 r1 v100 | 11/2 10/0
 19 | - | 11/1 11/0
 20 | h0 p75 d4 r1 v100 | 12/4 11/0
 21 | h1 p89 d2 r1 v100 | 12/3 15/2
 22 | - | 12/2 15/2
 23 | - | 12/1 15/1
 24 | h0 p79 d1 r1 v100 | 13/1 15/1
 25 | h0 p80 d1 r1 v100  h1 p87 d1 r1 v100 | 14/1 14/1
 26 | h0 p82 d2 r1 v100 | 15/2 14/1
 27 | h1 p86 d1 r1 v100 | 15/1 13/1
 28 | h0 p48 d1 r1 v100 | 0/1 13/1
 29 | h0 p50 d2 r1 v100  h1 p82 d4 r1 v100 | 1/2 12/4
 30 | - | 1/1 12/4
 31 | h0 p51 d1 r1 v100 | 2/1 12/3
 32 | h0 p55 d4 r1 v100 | 3/4 12/3
 33 | - | 3/3 12/2
 34 | - | 3/2 12/2
 35 | - | 3/1 12/1
 36 | h0 p56 d1 r1 v100 | 4/1 12/1
 37 | h1 p55 d1 r1 v100 | 5/3 0/1
 38 | - | 5/2 0/1
 39 | h1 p58 d2 r1 v100 | 5/1 1/2
 40 | h0 p62 d2 r1 v100 | 6/0 1/2
 41 | h0 p63 d1 r1 v100 | 7/0 1/1
 42 | h0 p67 d2 r1 v100 | 8/0 1/1
 43 | h0 p70 d1 r1 v100  h1 p58 d1 r1 v100 | 10/0 2/1
 44 | h0 p74 d2 r1 v100 | 11/0 2/1
 45 | h0 p75 d4 r1 v100  h1 p62 d4 r1 v100 | 12/0 3/4
 46 | h0 p79 d1 r1 v100 | 13/0 3/4
 47 | h0 p80 d1 r1 v100 | 14/0 3/3
 48 | h0 p82 d2 r1 v100 | 15/0 3/3
 49 | h0 p48 d1 r1 v100 | 0/0 3/2
 50 | h0 p50 d2 r1 v100 | 1/0 3/2
 51 | h0 p51 d1 r1 v100 | 2/0 3/1
 52 | h0 p55 d4 r1 v100 | 3/0 3/1
 53 | h0 p56 d1 r1 v100  h1 p70 d1 r1 v100 | 4/0 7/1
 54 | - | 5/0 7/1
 55 | h0 p62 d2 r1 v100  h1 p70 d2 r1 v100 | 6/0 6/2
 56 | h0 p63 d1 r1 v100 | 7/0 6/2"""

seleneGolden :: String
seleneGolden = """== the default rack
-- SELENE · edit the numbers; the rack follows.
-- <kind> <target> [range] opens a group of 8 · one slot per line · -- mutes a slot.
-- range is optional (unipolar5v / unipolar8v / bipolar5v …); omitted = the rig's default for that kind.

lfo es9main
  0.5 @0 sin 0.8   -- 1
  0.5 @0.125 sin 0.8   -- 2
  0.5 @0.25 sin 0.8   -- 3
  0.5 @0.375 sin 0.8   -- 4
  0.5 @0.5 sin 0.8   -- 5
  0.5 @0.625 sin 0.8   -- 6
  0.5 @0.75 sin 0.8   -- 7
  0.5 @0.875 sin 0.8   -- 8

euclid es9gt0
  3 8 @4   -- 1
  4 8 @4   -- 2
  5 8 @4   -- 3
  6 8 @4   -- 4
  7 16 @4   -- 5
  3 16 @4   -- 6
  4 16 @4   -- 7
  5 16 @4   -- 8

clock es9gt1
  1/4 x1 50%   -- 1
  1/4 x2 50%   -- 2
  1/4 x3 50%   -- 3
  1/4 x4 50%   -- 4
  1/4 x5 50%   -- 5
  1/4 x6 50%   -- 6
  1/4 x7 50%   -- 7
  1/4 x8 50%   -- 8

note midi1
  C2   -- 1
  G2   -- 2
  C3   -- 3
  D3   -- 4
  E3   -- 5
  G3   -- 6
  C4   -- 7
  E4   -- 8
rack round-trip true
== lines
> lfo es9main # rate "0.5 1 2 4" # phase "0 0.25"
  lfo es9main # rate "0.5 1 2 4 0.5 1 2 4" # phase "0 0.25 0 0.25 0 0.25 0 0.25"
> lfo es9main # tri 0.6 # sin 0
  lfo es9main # rate "0.5 1 2 4 0.5 1 2 4" # phase "0 0.25 0 0.25 0 0.25 0 0.25" # sin 0 # tri 0.6
> euclid es9gt0 # hits 5
  euclid es9gt0 # hits 5 # steps "8 8 8 8 16 16 16 16"
> euclid es9gt0 # hits "3 4 5 6 7 3 4 5" # steps "8 8 8 8 16 16 16 16" # acc 2
  euclid es9gt0 # hits "3 4 5 6 7 3 4 5" # steps "8 8 8 8 16 16 16 16" # acc 2
> clock es9gt1 # div 1/8 # mult "1 2 3 4 5 6 7 8" # pw 25
  clock es9gt1 # div 1/8 # mult "1 2 3 4 5 6 7 8" # pw 25
> note es98cv0 # note "C3 E3 G3 B3"
  note es98cv0 # note "C3 E3 G3 B3 C3 E3 G3 B3"
> env fh2_0 # a 10 # d 200 # s 64 # r 400
  env fh2_0 # a 10 # d 200 # s 64 # r 400
> lfo es9gt0 # rate 2
  lfo es9gt0 # rate 2 # phase "0 0.125 0.25 0.375 0.5 0.625 0.75 0.875"
> ochd es9main # rate 0.03 # spread 40
  lfo es9main # rate "0.03 0.052 0.084 0.153 0.238 0.427 0.697 1.2" # phase 0 # sin 0 # tri 0.8
> ochd es9main # shape sin # tri 0.2
  lfo es9main # rate "0.03 0.052 0.084 0.153 0.238 0.427 0.697 1.2" # phase 0 # tri 0.2
> quadrature es9main # rate 0.5
  lfo es9main # rate "0.5 0.5 0.5 0.5 0.75 0.75 0.75 0.75" # phase "0 0.25 0.5 0.75 0 0.25 0.5 0.75"
> drift es98cv0
  lfo es98cv0 # rate "0.05 0.069 0.086 0.107 0.131 0.165 0.204 0.256" # phase "0 0.125 0.25 0.375 0.5 0.625 0.75 0.875" # sin 0 # rnd 0.6
> divider es9gt0 # by "1 2 3 4 6 8 12 16"
  euclid es9gt0 # hits 1 # steps "1 2 3 4 6 8 12 16" # rate 1
> toussaint es9gt1
  euclid es9gt1 # hits "3 5 4 2 7 5 5 9" # steps "8 8 7 5 12 12 16 16" # rate "2 2 2 2 3 3 4 4"
> polymeter es9gt0 # hits 4
  euclid es9gt0 # hits 4 # steps "5 6 7 8 9 10 11 12"
> scatter es9gt1 # seed 7
  euclid es9gt1 # hits "6 2 3 4 4 6 4 6" # steps "16 8 9 11 8 16 11 13"
> chord es98cv0 # root D3 # quality min9
  note es98cv0 # note "D3 F3 A3 C4 E4 D4 F4 A4"
> lfo es9main # tri 0.4
  lfo es9main # rate "0.5 0.5 0.5 0.5 0.75 0.75 0.75 0.75" # phase "0 0.25 0.5 0.75 0 0.25 0.5 0.75" # tri 0.4
> lfo es9main # fresh # rate 2
  lfo es9main # rate 2 # phase "0 0.125 0.25 0.375 0.5 0.625 0.75 0.875"
> euclid fh2_1 # hits 3
  euclid fh2_1 # hits 3 # steps "8 8 8 8 16 16 16 16"
== refusals
squiggle es9main # rate 1 | refused: not a kind of polysignal or a block: squiggle (lfo, euclid, clock, note, env; or a block: ochd, quadrature, drift, divider, toussaint, polymeter, scatter, chord)
lfo es9main # hits 3 | refused: lfo has no hits (it has rate phase level sin sqr tri saw rnd nse)
euclid es9gt0 # hits many | refused: hits wants a whole number, not many
lfo | refused: a line starts with a kind and a bank, as in lfo es9main: lfo
ochd es9main # shape blob | refused: ochd: shape is sin, tri, saw or sqr, not blob
chord es98cv0 # quality fancy | refused: chord: no quality fancy (maj min maj7 min7 dom7 min9 sus2 sus4 dim aug)
divider es9gt0 # by "2 x" | refused: divider: by wants whole numbers, not x
divider fh2gt0 | refused: no bank fh2gt0: es9main, es9gt<n> or es98cv<n> (the ES-9 and its expanders), fh2_0 (the FH-2), fh2_<n> (its FHX-8GT expanders, from 1), midi<n>, or virtual:<name>
lfo virtual:bus # rate 1 | accepted
== off
off es9gt1 | es9 gt1 | still in the rack: false
off es9gt1 | refused: es9gt1 is already free
off fh2_1 | fh2 gt0 | still in the rack: false
off fh2gt0 | refused: no bank fh2gt0: es9main, es9gt<n> or es98cv<n> (the ES-9 and its expanders), fh2_0 (the FH-2), fh2_<n> (its FHX-8GT expanders, from 1), midi<n>, or virtual:<name>
== the rack after
-- SELENE · edit the numbers; the rack follows.
-- <kind> <target> [range] opens a group of 8 · one slot per line · -- mutes a slot.
-- range is optional (unipolar5v / unipolar8v / bipolar5v …); omitted = the rig's default for that kind.

lfo es9main
  2 @0 sin 0.8   -- 1
  2 @0.125 sin 0.8   -- 2
  2 @0.25 sin 0.8   -- 3
  2 @0.375 sin 0.8   -- 4
  2 @0.5 sin 0.8   -- 5
  2 @0.625 sin 0.8   -- 6
  2 @0.75 sin 0.8   -- 7
  2 @0.875 sin 0.8   -- 8

euclid es9gt0
  4 5 @4   -- 1
  4 6 @4   -- 2
  4 7 @4   -- 3
  4 8 @4   -- 4
  4 9 @4   -- 5
  4 10 @4   -- 6
  4 11 @4   -- 7
  4 12 @4   -- 8

euclid es9gt1
  6 16 @4   -- 1
  2 8 @4   -- 2
  3 9 @4   -- 3
  4 11 @4   -- 4
  4 8 @4   -- 5
  6 16 @4   -- 6
  4 11 @4   -- 7
  6 13 @4   -- 8

note midi1
  C2   -- 1
  G2   -- 2
  C3   -- 3
  D3   -- 4
  E3   -- 5
  G3   -- 6
  C4   -- 7
  E4   -- 8

note es98cv0
  D3   -- 1
  F3   -- 2
  A3   -- 3
  C4   -- 4
  E4   -- 5
  D4   -- 6
  F4   -- 7
  A4   -- 8

env fh2_0
  a 10 d 200 s 64 r 400 vel 96 time 2   -- 1
  a 10 d 200 s 64 r 400 vel 96 time 2   -- 2
  a 10 d 200 s 64 r 400 vel 96 time 2   -- 3
  a 10 d 200 s 64 r 400 vel 96 time 2   -- 4
  a 10 d 200 s 64 r 400 vel 96 time 2   -- 5
  a 10 d 200 s 64 r 400 vel 96 time 2   -- 6
  a 10 d 200 s 64 r 400 vel 96 time 2   -- 7
  a 10 d 200 s 64 r 400 vel 96 time 2   -- 8

euclid fh2_1
  3 8 @4   -- 1
  3 8 @4   -- 2
  3 8 @4   -- 3
  3 8 @4   -- 4
  3 16 @4   -- 5
  3 16 @4   -- 6
  3 16 @4   -- 7
  3 16 @4   -- 8"""

-- | The frozen golden for `voicingRun` (conformance/voicing-golden.txt).
voicingGolden :: String
voicingGolden = """voicing [] -> none
voicing [0,4,7] -> [0,4,7] root 48 period (Just 12)
voicing [0,4,7,11,14] -> [0,4,7,11,14] root 48 period (Just 24)
voicing [-12,-5,4,11,14] -> [0,7,16,23,26] root 48 period (Just 36)
voicing [0,4,7,12] -> [0,4,7,12] root 48 period (Just 24)
voicing [7,0,4,0] -> [0,4,7] root 48 period (Just 12)
snap c'maj9 46>48 47>48 48>48 49>48 50>52 51>52 52>52 53>52 54>55 55>55 56>55 57>59 58>59 59>59 60>59 61>62 62>62 63>62 64>62 65>62 66>62 67>72 68>72 69>72 70>72 71>72 72>72 73>72 74>76 75>76 76>76
  1 | h0 p48 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out -
  2 | h0 p48 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out -
  3 | h0 p48 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out -
  4 | h0 p52 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out -
  5 | h0 p52 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out -
  6 | h0 p52 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out -
  7 | h0 p55 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out -
  8 | h0 p55 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out -
  9 | h0 p55 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out -
 10 | h0 p59 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out -
 11 | h0 p59 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out [0,4,7,11,14]
 12 | h0 p59 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out [0,4,7,11,14]
 13 | h0 p62 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out [0,4,7,11,14]
 14 | h0 p62 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out [0,4,7,11,14]
 15 | h0 p62 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out [0,4,7,11,14]
 16 | h0 p48 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out [0,4,7,11,14]
 17 | h0 p48 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out [0,4,7,11,14]
 18 | h0 p48 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out [0,4,7,11,14]
 19 | h0 p48 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out [0,4,7,11,14]
 20 | h0 p52 d1 r1 v100 | grid [0,4,7,11,14]/(Just 24) out [0,4,7,11,14]
 21 | h0 p58 d1 r1 v100 | grid scale out -
 22 | h0 p62 d1 r1 v100 | grid scale out -
 23 | h0 p63 d1 r1 v100 | grid scale out -
 24 | h0 p67 d1 r1 v100 | grid scale out -
 25 | h0 p68 d1 r1 v100 | grid scale out -
 26 | h0 p70 d1 r1 v100 | grid scale out -
 27 | h0 p74 d1 r1 v100 | grid scale out -
 28 | h0 p75 d1 r1 v100 | grid scale out -
 29 | h0 p79 d1 r1 v100 | grid scale out -
 30 | h0 p80 d1 r1 v100 | grid scale out -"""

-- | The frozen golden for `headsRun` (conformance/heads-golden.txt).
headsGolden :: String
headsGolden = """  1 | h0 p50 d1 r1 v100
  2 | h0 p51 d1 r1 v100
  3 | h0 p55 d1 r1 v100
  4 | h0 p56 d1 r1 v100
  5 | h0 p58 d1 r1 v100
  6 | h0 p62 d1 r1 v100
  7 | h0 p63 d1 r1 v100
  8 | h0 p67 d1 r1 v100
  9 | h0 p68 d1 r1 v100
 10 | h0 p70 d1 r1 v100
 11 | h0 p74 d1 r1 v100
 12 | h0 p75 d1 r1 v100
 13 | h0 p79 d1 r1 v100
 14 | h0 p80 d1 r1 v100
 15 | h0 p82 d1 r1 v100
 16 | h0 p48 d1 r1 v100
 17 | h1 p75 d1 r1 v100
 18 | -
 19 | h1 p77 d1 r1 v100
 20 | -
 21 | h1 p82 d1 r1 v100
 22 | -
 23 | h1 p89 d1 r1 v100
 24 | -
 25 | h1 p87 d1 r1 v100
 26 | -
 27 | h1 p86 d1 r1 v100
 28 | -
 29 | h1 p82 d1 r1 v100
 30 | -
 31 | h1 p55 d1 r1 v100
 32 | -
 33 | h2 p56 d1 r1 v100  h2 p58 d1 r1 v100  h3 p62 d1 r1 v100
 34 | h2 p50 d1 r1 v100  h2 p46 d1 r1 v100  h3 p53 d1 r1 v100
 35 | h2 p44 d1 r1 v100  h2 p55 d1 r1 v100  h3 p79 d1 r1 v100
 36 | h2 p63 d1 r1 v100  h2 p67 d1 r1 v100
 37 | h2 p68 d1 r1 v100  h2 p70 d1 r1 v100  h3 p70 d1 r1 v100
 38 | h2 p62 d1 r1 v100  h2 p51 d1 r1 v100  h3 p60 d1 r1 v100
 39 | h2 p43 d1 r1 v100  h2 p39 d1 r1 v100  h3 p51 d1 r1 v100
 40 | h2 p38 d1 r1 v100  h2 p36 d1 r1 v100
 41 | h2 p56 d1 r1 v100  h2 p58 d1 r1 v100  h3 p60 d1 r1 v100
 42 | h2 p50 d1 r1 v100  h2 p46 d1 r1 v100  h3 p70 d1 r1 v100
 43 | h2 p44 d1 r1 v100  h2 p55 d1 r1 v100  h3 p79 d1 r1 v100
 44 | h2 p63 d1 r1 v100  h2 p67 d1 r1 v100
 45 | h2 p68 d1 r1 v100  h2 p70 d1 r1 v100  h3 p53 d1 r1 v100
 46 | h2 p62 d1 r1 v100  h2 p51 d1 r1 v100  h3 p62 d1 r1 v100
 47 | h2 p43 d1 r1 v100  h2 p39 d1 r1 v100  h3 p72 d1 r1 v100
 48 | h2 p38 d1 r1 v100  h2 p36 d1 r1 v100
 49 | -
 50 | -
 51 | -
 52 | -
 53 | -
 54 | -
 55 | -
 56 | -
 57 | -
 58 | -
 59 | -
 60 | -
 61 | -
 62 | -
 63 | -
 64 | -
 65 | h0 p50 d1 r1 v100
 66 | h0 p51 d1 r1 v100
 67 | h0 p55 d1 r1 v100
 68 | h0 p56 d1 r1 v100
 69 | h0 p58 d1 r1 v100
 70 | h0 p62 d1 r1 v100
 71 | h0 p63 d1 r1 v100
 72 | h0 p67 d1 r1 v100
 73 | h0 p68 d1 r1 v100
 74 | h0 p70 d1 r1 v100
 75 | h0 p74 d1 r1 v100
 76 | h0 p75 d1 r1 v100
 77 | h0 p79 d1 r1 v100
 78 | h0 p80 d1 r1 v100
 79 | h0 p82 d1 r1 v100
 80 | h0 p48 d1 r1 v100"""

-- | The frozen golden for `articulationRun` (conformance/articulation-golden.txt).
articulationGolden :: String
articulationGolden = """   0 | cc AUDIO4c USB2/1 65=0 @0  note AUDIO4c USB2/1 48 v100 @0 d112500  note FH-2/3 48 v100 @1500 d112500  slew 8=99 lag5 @0  pulse 9=500 d112500 @2000  cc Rample/2 14=20 @-40000  note Rample/2 61 v100 @0 d225000  cc Rample/4 14=20 @-40000  note Rample/4 36 v100 @0 d225000 | held -,-,-,-
   1 | cc AUDIO4c USB2/1 65=0 @0  note AUDIO4c USB2/1 51 v100 @0 d112500  note FH-2/3 51 v100 @1500 d112500  slew 8=124 lag5 @0  pulse 9=500 d112500 @2000 | held -,-,-,-
   2 | cc AUDIO4c USB2/1 65=0 @0  on AUDIO4c USB2/1 53 v100 @0  note FH-2/3 53 v100 @1500 d112500  slew 8=140 lag5 @0  cv 9=500 @0  cc Rample/2 14=20 @-40000  note Rample/2 61 v100 @0 d225000  cc Rample/4 24=20 @-40000  note Rample/4 37 v100 @0 d225000 | held 53,50,-,-
   3 | cc AUDIO4c USB2/1 65=127 @0  note AUDIO4c USB2/1 55 v100 @0 d112500  off AUDIO4c USB2/1 53 @56250  note FH-2/3 55 v100 @1500 d112500  slew 8=157 lag20 @0  pulse 9=500 d112500 @0  cc Rample/2 14=20 @-40000  note Rample/2 61 v100 @0 d225000  cc Rample/4 34=20 @-40000  note Rample/4 38 v100 @0 d225000 | held -,-,-,-
   4 | cc AUDIO4c USB2/1 65=0 @0  on AUDIO4c USB2/1 55 v100 @0  note FH-2/3 55 v100 @1500 d112500  slew 8=157 lag5 @0  cv 9=500 @0 | held 55,-,-,-
   5 | note FH-2/3 55 v100 @1500 d112500  cv 9=500 @0 | held 55,-,-,-
   6 | cc AUDIO4c USB2/1 65=127 @0  on AUDIO4c USB2/1 58 v100 @0  off AUDIO4c USB2/1 55 @60000  note FH-2/3 58 v100 @1500 d112500  slew 8=182 lag20 @0  cv 9=500 @0 | held 58,-,-,-
   7 | off AUDIO4c USB2/1 58 @0  cv 9=0 @0 | held -,-,-,-
   8 | cc AUDIO4c USB2/1 65=0 @0  note AUDIO4c USB2/1 48 v100 @0 d31875  note AUDIO4c USB2/1 48 v100 @37500 d31875  note AUDIO4c USB2/1 48 v100 @75000 d31875  note FH-2/3 48 v100 @1500 d112500  slew 8=99 lag5 @0  pulse 9=500 d112500 @2000  cc Rample/2 14=99 @-40000  note Rample/2 61 v100 @0 d95625  note Rample/2 61 v100 @112500 d95625  cc Rample/4 44=99 @-40000  note Rample/4 39 v100 @0 d225000 | held -,-,-,-
   9 | cc AUDIO4c USB2/1 65=0 @0  note AUDIO4c USB2/1 50 v100 @0 d112500  note FH-2/3 50 v100 @1500 d112500  slew 8=115 lag5 @0  pulse 9=500 d112500 @2000  cc AUDIO4c USB2/1 65=0 @62500  on AUDIO4c USB2/1 52 v100 @62500  note FH-2/3 52 v100 @64000 d112500  slew 8=132 lag5 @62500  cv 9=500 @62500 | held 52,-,-,-
  10 | - | held 52,-,-,-
  11 | cv 16=99 @0  slew 20=68 lag0 @0  cv 17=158 @0  slew 20=130 lag0 @0  cv 13=200 @0  pulse 14=500 d5000 @4000 | held 52,-,-,-
  12 | cc Rample/2 14=20 @-40000  note Rample/2 61 v100 @0 d225000  slew 20=13 lag25 @0  cv 16=132 @0  slew 20=68 lag0 @0  cc Rample/4 14=20 @-40000  note Rample/4 36 v100 @0 d225000 | held 52,-,-,-
  13 | slew 20=13 lag25 @0 | held 52,-,-,-
  14 | cv 16=198 @0  slew 20=68 lag0 @0 | held 52,-,-,-
stop | off AUDIO4c USB2/1 52 @0  cv 9=0 @0  slew 20=13 lag0 @0"""
