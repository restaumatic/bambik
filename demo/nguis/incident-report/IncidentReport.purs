module IncidentReport (Incident, IncidentRow, Position, Report, Time, clockPeriod, fileReport, noDescription, referenceLine, reportLine) where

import Prelude (const, discard, pure, show, (/), (<), (<>), (==))

import Data.Int (round)
import Data.Number.Format (fixed, toStringWith)
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)

type Incident = { | IncidentRow () }

type Report = { | IncidentRow ( position :: Position, time :: Time ) }

type IncidentRow r =
  ( "Description" :: String
  , "Photo" :: [ none :: {}, picked :: { mime :: String, name :: String, size :: Number } ]
  | r
  )

type Position = [ denied :: {}, located :: { lat :: Number, lng :: Number }, unknown :: {} ]

type Time = { hours :: Int, minutes :: Int, seconds :: Int }

noDescription :: {} -> String
noDescription _ = ""

clockPeriod :: { ms :: Number }
clockPeriod = { ms: 1000.0 }

reportLine :: Report -> String
reportLine { "Description": description, "Photo": photo, position, time } =
  "Reporting " <> subject <> " " <> whereabouts <> " at " <> clockText ":" time <> attachment <> "."
  where
  subject = if description == "" then "an incident" else description
  whereabouts = whereaboutsText position
  attachment = match
    { picked: \{ name, size } -> " with " <> name <> " (" <> show (round (size / 1024.0)) <> " kB)"
    , none: const ", no photo attached"
    } photo

fileReport :: Report -> Aff [ "Report filed" :: { position :: Position, reference :: String } ]
fileReport { position, time } = do
  delay (Milliseconds filingDelay.ms)
  pure (."Report filed" { position, reference: "IR-" <> clockText "" time })

referenceLine :: { position :: Position, reference :: String } -> String
referenceLine { position, reference } =
  "Your report, filed " <> whereaboutsText position <> ", has reference " <> reference <> ". Quote it when you follow up."

filingDelay :: { ms :: Number }
filingDelay = { ms: 800.0 }

whereaboutsText :: Position -> String
whereaboutsText = match
  { located: \{ lat, lng } -> "from " <> coordinate lat <> ", " <> coordinate lng
  , unknown: const "from a location not yet known"
  , denied: const "from an undisclosed location"
  }

coordinate :: Number -> String
coordinate = toStringWith (fixed 2)

clockText :: String -> Time -> String
clockText separator { hours, minutes, seconds } = padded hours <> separator <> padded minutes <> separator <> padded seconds

padded :: Int -> String
padded n = if n < 10 then "0" <> show n else show n
