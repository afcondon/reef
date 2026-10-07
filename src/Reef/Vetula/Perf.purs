-- | `Reef.Vetula.Perf` — the shared Vetula "Performance" scheduler: one saved chord
-- | progression fanned to several VOICES, each reading the SAME chords on its own
-- | clock (its own per-chord dwell schedule + phase offset), and each sounding them
-- | a different way (block / arp / strum → MIDI) or conducting Odonus's quantiser
-- | (→ odo). This is the exact scheduler the Triggerfish frontend runs (Vetula.App
-- | `timeline`/`cursorAt`/`stepVoice`), lifted into reef so it compiles to BOTH the
-- | JS frontend and the BEAM (`reef_vetula_perf@ps`) and co-simulates byte-for-byte.
-- |
-- | The whole thing is a PURE FUNCTION OF THE ABSOLUTE PULSE (the shared Link
-- | 1/16-note index — `stepBeats 0.25`, the same grid Odonus and Balistes ride).
-- | There is no seed and no accumulating engine state: `pos = (pulse + phase) mod
-- | loopLen`, `timeline` turns the bars-per-chord `durs` (0 = skip) into cumulative
-- | segments, and the segment covering `pos` names the chord. So — even more than
-- | Balistes' fixed rhythm — a performance needs no handoff-phase machinery: push
-- | the definition once and both runtimes agree forever, with no per-event wire.
-- |
-- | V1 lights up the `→ odo` path: a `VToOdonus` voice sounds no MIDI, it just
-- | advances a read-head; `odoCursorAt`/`odoPcsAt` give the BEAM the pitch-class set
-- | that voice is conducting so it can feed Odonus's chord overlay (a tick-tagged
-- | `FollowChord`). The block/arp/strum → MIDI realisations (V2) reuse the same
-- | `timeline`/`cursorAt` here.
module Reef.Vetula.Perf
  ( VRenderer(..)
  , VDest(..)
  , VChord
  , VVoice
  , Perf
  , padDurs
  , timeline
  , Seg
  , PerfClock
  , clockOfDurs
  , cursorAt
  , cursorAtClock
  , segAtClock
  , renderNoteClockMidiAt
  , renderAlphaClockMidiAt
  , firstOdoIx
  , odoCursorAt
  , odoPcsAt
  , VMidiNote
  , renderVoiceMidiAt
  , renderClockMidiAt
  , renderAlphaBlockMidiAt
  , VMidiOut
  , renderMidiAt
  , midiChannels
  , wrapAt
  ) where

import Prelude

import Data.Array (elem, filter, find, findIndex, foldl, length, mapWithIndex, null, replicate, sort, take, (!!))
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), snd)

-- | How a voice sounds the chord it is currently on. Block = the whole chord held
-- | for the segment; Arp = one chord note per pulse, cycling; Strummed = re-trigger
-- | only the notes that changed (common tones ring on). (V1's odo path ignores this;
-- | it's carried so the V2 MIDI voices share one model + wire.)
data VRenderer = VBlock | VArp | VStrummed

derive instance eqVRenderer :: Eq VRenderer

-- | Where a voice's chord goes. `VToMidi` sounds it on the voice's MIDI channel per
-- | its renderer; `VToOdonus` sends NO MIDI and instead conducts Odonus's quantiser
-- | (the `channel` field is reused as the Odonus id).
data VDest = VToMidi | VToOdonus

derive instance eqVDest :: Eq VDest

-- | One progression chord in the serialisable subset both runtimes need: `pcs` (the
-- | pitch classes 0..11, what the → odo quantiser follows) and `notes` (the concrete
-- | ascending MIDI of `playNotes` = `[bassPc+36] <> voicing`, what the V2 MIDI voices
-- | sound). Name/voicing-graph metadata stay frontend-side.
type VChord =
  { pcs :: Array Int
  , notes :: Array Int
  }

-- | A performance voice: its own read-head into the shared progression. `durs` is
-- | bars-per-chord (one entry per progression chord; 0 = skip that chord), `phase`
-- | a pulse offset so identical columns can phase apart. `cursor`/`held` are runtime
-- | state derived on each runtime, not pushed — so a `VVoice` is the pushed shape.
type VVoice =
  { dest :: VDest
  , renderer :: VRenderer
  , channel :: Int
  , durs :: Array Int
  , phase :: Int
  , muted :: Boolean
  }

-- | A whole performance: the shared progression + the voices reading it.
type Perf =
  { chords :: Array VChord
  , voices :: Array VVoice
  }

-- | Fit a voice's duration column to the current chord count (pad new chords with
-- | one bar, drop trailing extras) — keeps the clock robust if the progression
-- | length and the stored column ever disagree. Identical to the frontend.
padDurs :: Int -> Array Int -> Array Int
padDurs n ds = take n (ds <> replicate n 1)

-- | A voice's timeline: one segment per NON-skipped chord, in chord order, each at
-- | its cumulative pulse offset. 1 bar = 16 pulses (16th notes). Skipped chords (0
-- | bars) contribute nothing, so a voice plays only the chords it dwells on. Pure
-- | integer arithmetic — trivially identical across runtimes.
timeline :: Array Int -> Array Seg
timeline ds = snd (foldl step (Tuple 0 []) (mapWithIndex Tuple ds))
  where
  step (Tuple off segs) (Tuple i d) =
    if d <= 0 then Tuple off segs
    else Tuple (off + d * 16) (segs <> [ { ix: i, start: off, len: d * 16 } ])

-- | One dwell segment of a voice's read-head: it sits on chord `ix` for `len`
-- | pulses starting at pulse `start` (within the loop). This is the ONLY thing
-- | the realiser needs — where the segments come from (a `durs` array, today, or
-- | a queried Tidal pattern, tomorrow) is not its concern. 16 pulses = 1 bar.
type Seg = { ix :: Int, start :: Int, len :: Int }

-- | A voice's clock: the loop's segments + its total length in pulses. Everything
-- | the scheduler does is a pure function of `(pulse + phase) mod loopLen` against
-- | these segments. `clockOfDurs` reproduces the historical bars-per-chord clock;
-- | a pattern-driven clock (built frontend-side from the Tidal engine, and — once
-- | the rig catches up — on the BEAM) is just a different way to fill the SAME
-- | shape, so the realiser below stays byte-identical across both sources.
type PerfClock = { segs :: Array Seg, loopLen :: Int }

-- | The historical clock: bars-per-chord `durs` (padded to the chord count) turned
-- | into cumulative segments, loop = 16 * total bars. `cursorAt`/`renderVoiceMidiAt`
-- | are exactly `*Clock (clockOfDurs …)`, so nothing about the durs path changes.
clockOfDurs :: Int -> Array Int -> PerfClock
clockOfDurs nChords durs =
  let ds = padDurs nChords durs
  in { segs: timeline ds, loopLen: 16 * sum ds }

-- | The chord index a voice's read-head is on at this pulse (Nothing if its loop is
-- | empty or it is resting between dwell segments). The exact frontend function.
cursorAt :: Int -> VVoice -> Int -> Maybe Int
cursorAt nChords v pulse = cursorAtClock (clockOfDurs nChords v.durs) v.phase pulse

-- | The segment covering this pulse (with its onset + length), given any clock +
-- | phase. `Nothing` if the loop is empty or the read-head is resting in a gap.
segAtClock :: PerfClock -> Int -> Int -> Maybe Seg
segAtClock clock phase pulse =
  if clock.loopLen <= 0 then Nothing
  else let pos = mod (pulse + phase) clock.loopLen
       in find (\seg -> pos >= seg.start && pos < seg.start + seg.len) clock.segs

-- | The chord index a read-head is on at this pulse, given any clock + phase. The
-- | clock-source-agnostic core of `cursorAt` — a pattern clock queries here too.
cursorAtClock :: PerfClock -> Int -> Int -> Maybe Int
cursorAtClock clock phase pulse = _.ix <$> segAtClock clock phase pulse

-- | Index of the first `→ odo` voice, if any. (V1 conducts one Odonus; multi-Odonus
-- | routing by `channel`-as-id is a later concern.)
firstOdoIx :: Perf -> Maybe Int
firstOdoIx perf = findIndex (\v -> v.dest == VToOdonus) perf.voices

-- | The chord index the first → odo voice conducts at this pulse, HOLDING the given
-- | previous cursor across rests (matching the frontend's `fromMaybe v.cursor`). The
-- | BEAM threads its own `prevCursor` and re-feeds Odonus when this changes.
odoCursorAt :: Perf -> Int -> Int -> Int
odoCursorAt perf pulse prevCursor =
  case firstOdoIx perf of
    Nothing -> prevCursor
    Just ix -> case perf.voices !! ix of
      Nothing -> prevCursor
      Just v -> fromMaybe prevCursor (cursorAt (length perf.chords) v pulse)

-- | The pitch-class set of the progression chord at `cursor` (empty if out of range)
-- | — what a → odo voice feeds Odonus's chord overlay.
odoPcsAt :: Perf -> Int -> Array Int
odoPcsAt perf cursor = maybe [] _.pcs (wrapAt perf.chords cursor)

-- | One MIDI note a → midi voice sounds at a pulse: absolute note, velocity, and the
-- | gate length in PULSES (the runtime multiplies by the current step-ms, so this
-- | stays tempo-agnostic). All renderers are gated (a note-with-duration), which is
-- | exactly what both `Midi.scheduleNote` (browser) and `scheduleNoteAt` (rig) speak —
-- | so the same decision emits byte-identically on both.
type VMidiNote =
  { note :: Int
  , velocity :: Int
  , durPulses :: Number
  }

-- | Render one pulse of one → midi voice to concrete notes — the SHARED decision the
-- | browser (Vetula.App stepVoice) and the rig (reef_vetula_voice) both use. Block
-- | attacks the whole chord on the segment onset for the segment's length; Arp plays
-- | one chord note per pulse, cycling. (Strummed is V2b — its sustain/tie needs a
-- | computed gate; here it stays silent so block/arp land first.) A muted voice, a
-- | → odo voice, or a resting pulse produces nothing.
renderVoiceMidiAt :: Array VChord -> VVoice -> Int -> Array VMidiNote
renderVoiceMidiAt chords v pulse =
  renderClockMidiAt chords v (clockOfDurs (length chords) v.durs) pulse

-- | The clock-source-agnostic core of `renderVoiceMidiAt`: render one pulse of a
-- | → midi voice against ANY clock (durs-derived today, Tidal-pattern-derived once
-- | the frontend/BEAM feed one). Block/arp/strum are unchanged — they only ever
-- | read `clock.segs`, so the same pulse produces the same notes regardless of how
-- | the segments were computed.
renderClockMidiAt :: Array VChord -> VVoice -> PerfClock -> Int -> Array VMidiNote
renderClockMidiAt chords = renderAlphaBlockMidiAt (map (sort <<< _.notes) chords)

-- | The articulator-aware core of `renderClockMidiAt`: block / arp / strum sound a
-- | precomputed **alphabet per chord** (`Reef.Vetula.Articulate.articulate`) rather than
-- | always the chord's own notes, so the articulator reaches the renderer path too — a
-- | plain block or strum voice can sound a voice-led line, not just a `♪`-patterned one.
-- | `renderClockMidiAt` is exactly this with the block alphabet (`map (sort <<< _.notes)
-- | chords`), so that path is byte-identical.
-- |
-- | Strum falls out **principled** here: over a voice-led alphabet, common tones keep the
-- | SAME MIDI number chord-to-chord, so the `entering` test (in this chord, not the
-- | previous) already tags them as held — they tie via `strumSustain` while genuinely new
-- | notes re-attack. The voice-leading IS the held/entering distinction; no lifetime flag.
renderAlphaBlockMidiAt :: Array (Array Int) -> VVoice -> PerfClock -> Int -> Array VMidiNote
renderAlphaBlockMidiAt alphabets v clock pulse
  | v.muted = []
  | v.dest /= VToMidi = []
  | otherwise =
      let segs = clock.segs
          loopLen = clock.loopLen
      in
        if loopLen <= 0 then []
        else
          let pos = mod (pulse + v.phase) loopLen
          in case findIndex (\s -> pos >= s.start && pos < s.start + s.len) segs of
            Nothing -> []
            Just segIx ->
              let seg = fromMaybe emptySeg (segs !! segIx)
                  notes = fromMaybe [] (wrapAt alphabets seg.ix)
              in case v.renderer of
                VBlock ->
                  if pos == seg.start
                    then map (\nn -> { note: nn, velocity: 82, durPulses: toNumber seg.len * 0.98 }) notes
                    else []
                VArp ->
                  if null notes then []
                  else case notes !! mod (pos - seg.start) (length notes) of
                    Just nn -> [ { note: nn, velocity: 80, durPulses: 0.9 } ]
                    Nothing -> []
                VStrummed ->
                  -- Only at a chord onset. The notes ENTERING here (in this chord but
                  -- not the previous segment's) attack; each is gated for its
                  -- sustain — the run of consecutive segments from here that still
                  -- contain it — so common tones tie into one long note instead of
                  -- re-triggering. Loop-relative (the first segment has no previous, so
                  -- everything re-attacks each loop): stateless, a pure fn of the pulse.
                  if pos /= seg.start then []
                  else
                    let prevNotes =
                          if segIx <= 0 then []
                          else fromMaybe []
                                 (wrapAt alphabets (fromMaybe emptySeg (segs !! (segIx - 1))).ix)
                        entering = filter (\nn -> not (elem nn prevNotes)) notes
                    in map (\nn -> { note: nn, velocity: 84, durPulses: strumSustain alphabets segs segIx nn }) entering
  where
  emptySeg = { ix: 0, start: 0, len: 0 }

-- | Axis-B rendering: a NOTE-index pattern sequences the notes of whichever chord the
-- | read-head is on. `chordClock` says which chord (its notes are the alphabet, low→
-- | high); `noteClock` is a second pattern whose segment values are note indices INTO
-- | that alphabet (wrapping, so a 4-note chord cycles 0..3). At a note-segment onset we
-- | sound one note, gated for the segment's length — so `0 1 2 3` arps, `3` holds the
-- | top voice, `[0 1 2 3]*4` is a fast arp. Monophonic for now (one note per pulse);
-- | polyphonic stacks + non-trivial alphabets are the articulator layer. Muted / → odo
-- | / resting produces nothing, same as the renderer path.
renderNoteClockMidiAt :: Array VChord -> VVoice -> PerfClock -> PerfClock -> Int -> Array VMidiNote
renderNoteClockMidiAt chords = renderAlphaClockMidiAt (map (sort <<< _.notes) chords)

-- | The articulator-aware core of `renderNoteClockMidiAt`: the note-pattern indexes a
-- | precomputed **alphabet per chord** (`Reef.Vetula.Articulate.articulate`) rather than
-- | always the chord's own notes. `renderNoteClockMidiAt` is exactly this with the block
-- | alphabet (`map (sort <<< _.notes) chords`), so that path is byte-identical; other
-- | articulators (voice-led lines, …) just hand a different alphabet in. The whole
-- | timing/onset/wrap logic below is shared, so every articulator sounds through the one
-- | seam. Muted / → odo / resting produces nothing, same as before.
renderAlphaClockMidiAt :: Array (Array Int) -> VVoice -> PerfClock -> PerfClock -> Int -> Array VMidiNote
renderAlphaClockMidiAt alphabets v chordClock noteClock pulse
  | v.muted = []
  | v.dest /= VToMidi = []
  | otherwise =
      case cursorAtClock chordClock v.phase pulse of
        Nothing -> []
        Just cix ->
          let notes = fromMaybe [] (alphabets !! cix)
          in if null notes then []
             else case segAtClock noteClock v.phase pulse of
               Nothing -> []
               Just seg ->
                 let pos = mod (pulse + v.phase) noteClock.loopLen
                 in if pos /= seg.start then []  -- fire once, on the note's onset
                    else
                      -- Euclidean-normalised index: positive wraps (0..n-1), NEGATIVE
                      -- counts from the top (-1 = highest note, size-independent), so a
                      -- melody line stays on top across chords of different sizes.
                      let len = length notes
                          j = mod (mod seg.ix len + len) len
                      in case notes !! j of
                        Just nn -> [ { note: nn, velocity: 80, durPulses: toNumber seg.len * 0.9 } ]
                        Nothing -> []

-- | How long a strummed note sustains from segment `segIx`: the total pulses of the
-- | consecutive run of segments (no loop wrap) whose alphabet still contains it, trimmed
-- | a hair so a re-attack at the loop boundary can't collide with the note-off. Reads the
-- | same per-chord alphabets the renderer sounds, so voice-led common tones (identical
-- | MIDI across chords) sustain correctly.
strumSustain :: Array (Array Int) -> Array Seg -> Int -> Int -> Number
strumSustain alphabets segs segIx nn = toNumber (go segIx 0) * 0.98
  where
  go k acc = case segs !! k of
    Nothing -> acc
    Just s ->
      if elem nn (fromMaybe [] (wrapAt alphabets s.ix))
        then go (k + 1) (acc + s.len)
        else acc

-- | One MIDI note across the WHOLE performance, tagged with the ORDINAL of the → midi
-- | voice that sounds it (0-based, counting every → midi voice in `voices` order,
-- | muted or not, so a mute never shifts channel assignments). The rig maps that
-- | ordinal to its own channel list; the browser voice keeps its own `channel`.
type VMidiOut =
  { voiceOrd :: Int
  , note :: Int
  , velocity :: Int
  , durPulses :: Number
  }

-- | Every → midi note the performance sounds at this pulse, flattened across voices
-- | and tagged with each voice's ordinal — the single call the rig's reef_vetula_voice
-- | makes per pulse to drive its MIDI emit. Pure; a → odo voice contributes nothing
-- | but does NOT consume an ordinal (only → midi voices are numbered).
renderMidiAt :: Perf -> Int -> Array VMidiOut
renderMidiAt perf pulse = (foldl step { ord: 0, out: [] } perf.voices).out
  where
  step acc v =
    if v.dest /= VToMidi then acc
    else
      let tagged = map (\e -> { voiceOrd: acc.ord, note: e.note, velocity: e.velocity, durPulses: e.durPulses })
                     (renderVoiceMidiAt perf.chords v pulse)
      in { ord: acc.ord + 1, out: acc.out <> tagged }

-- | The pushed MIDI channel of each → midi voice, in the SAME order `renderMidiAt`
-- | assigns `voiceOrd` (VToMidi voices in `voices` order, muted or not). The rig indexes
-- | this by `voiceOrd` to place each note on the channel the browser chose — so both
-- | runtimes honour one routing map instead of a rig-local channel list. Pure, and NOT
-- | in the conformance digest (channel is routing, not note-generation).
midiChannels :: Perf -> Array Int
midiChannels perf = map _.channel (filter (\v -> v.dest == VToMidi) perf.voices)

-- | **A sequence index past the progression's end wraps** (AC, 2026-10-07):
-- | a progression is an endless stream of itself, so a voice's own sequence
-- | `"0 1 2 3"` over three chords plays 0 1 2 0, never a rest. Negative
-- | indices count from the end, as the note alphabet's do.
wrapAt :: forall a. Array a -> Int -> Maybe a
wrapAt xs i =
  let n = length xs
  in if n == 0 then Nothing else xs !! (mod (mod i n + n) n)
