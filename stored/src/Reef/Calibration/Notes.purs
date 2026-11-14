-- | What is written down beside a calibration table: which physical module
-- | it was, where it sat, what was said about it, and the two recordings.
-- |
-- | Beside the table and not inside it. A table is content in Amphora, named
-- | by the hash of its text, so a comment inside it would rename the table
-- | every time a typo was fixed. The notes are content of their own (kind
-- | `calibration-notes`) that the ref `quadrat/calibrations/<label>` points
-- | at; editing them moves the ref, and the store keeps where it pointed
-- | before. So a signal path's calibrations are a history.
module Reef.Calibration.Notes
  ( Notes
  , kind
  , refFor
  , notes
  , read
  , write
  ) where

import Prelude

import Data.Maybe (Maybe)
import Hawser.Message (field, recordOf)
import Hawser.Value (Value, fromText, string, toText)

kind :: String
kind = "calibration-notes"

-- | The ref that points at a signal path's current notes.
refFor :: String -> String
refFor label = "quadrat/calibrations/" <> label

type Notes =
  { label :: String
  -- | The hash of the table these notes are about.
  , table :: String
  -- | Which physical module.
  , module :: String
  -- | Which case, which row.
  , place :: String
  , comment :: String
  -- | The takes: the sweep, and the same pitches played through the table.
  -- | Names of recordings on disk, empty when there is none. A take can be
  -- | deleted without harm to the table; only the replaying is lost.
  , before :: String
  , after :: String
  }

notes :: Value Notes
notes = recordOf "calibration notes" $
  (\label table mod place comment before after -> { label, table, module: mod, place, comment, before, after })
    <$> field "label" _.label string "the signal path"
    <*> field "table" _.table string "the hash of the table these are about"
    <*> field "module" _.module string "which physical module"
    <*> field "where" _.place string "which case, which row"
    <*> field "comment" _.comment string "anything worth knowing later"
    <*> field "before" _.before string "the take of the sweep"
    <*> field "after" _.after string "the take of the same pitches through the table"

read :: String -> Maybe Notes
read = fromText notes

write :: Notes -> String
write = toText notes
