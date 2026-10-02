-- | `Reef.Clips` — the shared MIDI clip library types (recording axis #27,
-- | see docs/DESIGN-midi-clip-library.md). A leaf module (Prelude + Data only) so
-- | every capturing machine (Odonus, Vetula, …) and the shared store can depend on
-- | ONE definition of a captured clip without an import cycle.
-- |
-- | Principle: STORE EVERYTHING, FLATTEN LATE. A `MidiClip` keeps the full captured
-- | events — pitch, source channel (`headIdx`), velocity, gate, absolute onset
-- | micros — and bakes in NO tempo, key, or quantization. Every lossy projection
-- | (flatten-to-chord, drop-velocity, re-tempo) happens at PLAYBACK, so future
-- | fidelity (honour velocity, per-phrase rate) needs no re-capture.
-- |
-- | Moved from Triggerfish (`Triggerfish.Clips`) on 2026-10-02 so the rig can read Vetula's
-- | cards (docs/kb/plans/gpl-boundary-review.md, step 3); Triggerfish re-exports it.
module Reef.Clips
  ( NoteEvent
  , MidiClip
  , headCount
  , Group
  , onsetGroups
  ) where

import Prelude

import Data.Array (filter, length, nub, sort)
import Data.Foldable (maximum)
import Data.Maybe (Maybe, fromMaybe)

-- | One emitted note in a scrolling monitor / logbook / clip. `fireUnixMicros` is
-- | the wall-clock instant it sounded (rebased to [0, lenMicros) inside a clip);
-- | `headIdx` is the source voice/channel it came from (0-based), preserved so a
-- | multi-channel recording keeps its channel structure. `vel`/`gateMs` carry the
-- | dynamics + note length so a faithful re-emit is possible. Moved here from
-- | `Odonus.Grid.Types` (which now re-exports it) so it's the one shared definition.
type NoteEvent = { pitch :: Int, headIdx :: Int, fireUnixMicros :: Number, vel :: Int, gateMs :: Number }

-- | A captured MIDI clip in the shared library. CORE fields are always present; the
-- | rest is optional/defaulted metadata for the future clip browser — populated
-- | cheaply at capture, all forward-compatible so the schema doesn't churn. Naming
-- | clips is a mug's game (you won't remember what "clip 7" was), so the browser is
-- | meant to work by tags + computed features (density, pitch range, voice count —
-- | all DERIVABLE from `events`, hence not stored), not by name.
type MidiClip =
  { -- core --
    id             :: String        -- stable id (capture instant + machine); survives rename
  , events         :: Array NoteEvent  -- full fidelity, rebased to [0, lenMicros)
  , lenMicros      :: Number         -- loop length; onsets repeat every lenMicros
  , heads          :: Int            -- distinct source channels present
  , capturedMicros :: Number         -- wall-clock capture instant (default sort key)
  , source         :: String         -- capturing machine: "odonus" | "vetula" | …
    -- human metadata (optional; "" / [] / Nothing defaults) --
  , name           :: String         -- may be ""; editable; never relied upon
  , tags           :: Array String   -- populated at capture (machine, key, scale); editable
  , notes          :: String         -- free annotation
  , bpm            :: Maybe Number    -- capture tempo (informational — playback RE-tempos)
  , key            :: Maybe String    -- capture key/scale if known
  , context        :: Maybe String    -- capturing patch (Lepidoptera text), for harmonic recall
  }

-- | How many distinct source channels a clip's events span — the `heads` field, and
-- | the count the phrase-voice mute mask ranges over.
headCount :: Array NoteEvent -> Int
headCount = length <<< nub <<< map _.headIdx

-- | **Notes sharing an instant, as one strike.**
-- |
-- | The same grouping Quadrat does on overheard MIDI, except that these times
-- | were DECLARED by whatever played them — so where a detector guesses with a
-- | 50 ms window about a human hand, this is exact about a machine. Equality on
-- | the onset, not a window: a machine that fired four notes together wrote one
-- | number four times.
-- |
-- | A CAPTURED clip groups into mostly-singletons, and that is not a failure of
-- | the grouping — a phrase really is one note after another. The shape is the
-- | same either way, which is why one function serves a progression and a
-- | phrase alike.
type Group = { at :: Number, notes :: Array Int, gate :: Number }

onsetGroups :: Array NoteEvent -> Array Group
onsetGroups evs = map grp (sort (nub (map _.fireUnixMicros evs)))
  where
  grp at =
    let here = filter (\e -> e.fireUnixMicros == at) evs
    in { at
       , notes: sort (map _.pitch here)
       -- The LONGEST gate in the group: a strike is over when its last voice
       -- is, and a shorter inner voice is a detail of the voicing rather than
       -- of the strike.
       , gate: fromMaybe 0.0 (maximum (map _.gateMs here))
       }
