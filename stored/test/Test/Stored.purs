-- | A stored calibration table reads, writes with its keys in order, and
-- | refuses what it does not describe.
module Test.Stored (main) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Reef.Calibration (realise)
import Reef.Calibration.Notes as Notes
import Reef.Calibration.Stored as Stored

-- | As DeepStar wrote one on 2026-07-11, shortened to three points.
fixture :: String
fixture = """{"base_note":"C2","bus":8,"channel":0,"coarse_setting":"dialed to C2","device":"ES-9","kind":"calibration-table","label":"instruo-cruinn","measured_at":"2026-07-11T18:49:53+0200","module":"Instruo Cruinn","points":[{"err_cents":0,"hz":65.363,"reads":3,"spread_cents":0.2,"volts":0},{"err_cents":7.93,"hz":78.087,"reads":3,"spread_cents":0.42,"volts":0.25},{"err_cents":15.71,"hz":93.28,"reads":3,"spread_cents":0.1,"volts":0.5}],"probe":"deepstar-probe (mcleod)","ref_hz_at_0v":65.363,"stimulus":"es9-cv"}"""

main :: Effect Unit
main = do
  let
    failures = case Stored.read fixture of
      Nothing -> [ "the fixture does not read" ]
      Just kept ->
        expect "its label" (kept.label == "instruo-cruinn")
          <> expect "its points" (Array.length kept.points == 3)
          <> expect "a field that was left out" (isNothing kept.fullScaleVolts)
          <> expect "the coarse setting" (kept.coarseSetting == Just "dialed to C2")
          <> expect "written back, it is the same text: the keys are in order" (Stored.write kept == fixture)
          <> expect "the realiser takes it" (realise (Stored.table kept) 65.363 == 0.0)
          <> expect "volts per octave" (let tracking = Stored.voltsPerOctave kept in tracking > 0.97 && tracking < 0.98)
          <> expect "a table with an undescribed field is refused"
            (isNothing (Stored.read """{"bus":8,"channel":0,"coarse_setting":null,"kind":"calibration-table","label":"x","points":[],"colour":"red"}"""))
          <> expect "a null coarse setting is nobody having recorded it"
            ((Stored.read """{"bus":8,"channel":0,"coarse_setting":null,"kind":"calibration-table","label":"x","points":[]}""" <#> _.coarseSetting) == Just Nothing)
          <> expect "notes written and read are the same, quotes and all"
            ( let
                kept = { label: "plaits-es9-jack1", table: "abc", module: "Plaits \"the black one\"", moduleKey: Just "mutable-instruments-plaits-2", place: "top row\nleft", comment: "", before: "calibrate-before-1114-120000", after: "" }
              in
                Notes.read (Notes.write kept) == Just kept
            )
          <> expect "notes as the page first wrote them, by hand, still read"
            ( (Notes.read """{"label":"x","table":"h","module":"m","where":"w","comment":"c","before":"b","after":"a"}""" <#> _.place) == Just "w" )
          <> expect "a point without a pitch is refused"
            (isNothing (Stored.read """{"bus":8,"channel":0,"coarse_setting":null,"kind":"calibration-table","label":"x","points":[{"volts":1}]}"""))
  for_ failures \failure -> log ("FAIL  " <> failure)
  log ("stored calibration tables: " <> show (Array.length failures) <> " failures")
  unless (Array.null failures) (throw "the stored table's description does not hold")
  where
  expect label holds = if holds then [] else [ label ]
