module IncidentReport (Filing, Incident, clockPeriod, fileReport, referenceLine, reportLine) where

import Prelude (const, discard, pure, show, (/), (<), (<>))

import Data.Int (round)
import Data.Number.Format (fixed, toStringWith)
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)

type Incident =
  { "Photo" :: [ none :: {}, picked :: { mime :: String, name :: String, size :: Number } ]
  , position :: [ denied :: {}, located :: { lat :: Number, lng :: Number }, unknown :: {} ]
  , time :: { hours :: Int, minutes :: Int, seconds :: Int }
  }

clockPeriod :: { ms :: Number }
clockPeriod = { ms: 1000.0 }

reportLine :: Incident -> String
reportLine { "Photo": photo, position, time } =
  "Reporting " <> whereabouts <> " at " <> padded time.hours <> ":" <> padded time.minutes <> ":" <> padded time.seconds <> attachment <> "."
  where
  whereabouts = match
    { located: \{ lat, lng } -> "from " <> coordinate lat <> ", " <> coordinate lng
    , unknown: const "from a location still being found"
    , denied: const "from an undisclosed location"
    } position
  attachment = match
    { picked: \{ name, size } -> " with " <> name <> " (" <> show (round (size / 1024.0)) <> " kB)"
    , none: const ", no photo attached"
    } photo

type Filing = { event :: { reference :: String }, model :: Incident }

fileReport :: Incident -> Aff [ "Report filed" :: { reference :: String } ]
fileReport { time } = do
  delay (Milliseconds filingDelay.ms)
  pure (."Report filed" { reference: "IR-" <> padded time.hours <> padded time.minutes <> padded time.seconds })

referenceLine :: Filing -> String
referenceLine { event: { reference } } = "Your report's reference is " <> reference <> ". Quote it when you follow up."

filingDelay :: { ms :: Number }
filingDelay = { ms: 800.0 }

coordinate :: Number -> String
coordinate = toStringWith (fixed 2)

padded :: Int -> String
padded n = if n < 10 then "0" <> show n else show n
