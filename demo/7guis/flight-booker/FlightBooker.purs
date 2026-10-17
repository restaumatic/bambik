module FlightBooker (Trip, bookingLine, bookingState, itinerarySettleTime, plannedTrip, submit) where

import Prelude (bind, const, discard, otherwise, pure, show, (#), (&&), (*), (+), (<), (<=), (<>), (>=), (>>>))

import Data.Either (Either(..), either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)

type Trip =
  { "Flight type" :: [ "one-way" :: {}, return :: {} ]
  , "Return date" :: String
  , "Start date" :: String
  }

type Date = { y :: Int, m :: Int, d :: Int }

type Problem =
  [ returnBeforeStart :: {}
  , unreadableReturn :: { input :: String }
  , unreadableStart :: { input :: String }
  ]

type Itinerary = [ oneWayOn :: Date, returnBetween :: { out :: Date, back :: Date } ]

plannedTrip :: Trip
plannedTrip = { "Flight type": ."one-way" {}, "Start date": "27.03.2026", "Return date": "27.03.2026" }

itinerarySettleTime :: { ms :: Number }
itinerarySettleTime = { ms: 300.0 }

bookingLine :: Trip -> String
bookingLine = parse >>> either (\problem -> "⚠ " <> problemText problem) itineraryText

bookingState :: Trip -> [ bookable :: {}, problem :: {} ]
bookingState = parse >>> either (const (.problem {})) (const (.bookable {}))

submit :: Trip -> Aff [ "Flight booked" :: Trip ]
submit trip = do
  delay (Milliseconds 600.0)
  pure (."Flight booked" trip)

parse :: Trip -> Either Problem Itinerary
parse { "Flight type": flightType, "Start date": startInput, "Return date": returnInput } =
  case parseDate startInput of
    Nothing -> Left (.unreadableStart { input: startInput })
    Just out -> flightType # match
      { "one-way": \_ -> Right (.oneWayOn out)
      , return: \_ -> case parseDate returnInput of
          Nothing -> Left (.unreadableReturn { input: returnInput })
          Just back
            | dateKey back < dateKey out -> Left (.returnBeforeStart {})
            | otherwise -> Right (.returnBetween { out, back })
      }

problemText :: Problem -> String
problemText = match
  { unreadableStart: \{ input } -> "start date " <> show input <> " is not a valid DD.MM.YYYY date"
  , unreadableReturn: \{ input } -> "return date " <> show input <> " is not a valid DD.MM.YYYY date"
  , returnBeforeStart: \_ -> "the return date is before the start date"
  }

itineraryText :: Itinerary -> String
itineraryText = match
  { oneWayOn: \out -> "A one-way flight on " <> formatDate out
  , returnBetween: \{ out, back } -> "A return flight: out " <> formatDate out <> ", back " <> formatDate back
  }

parseDate :: String -> Maybe Date
parseDate s = case split (Pattern ".") s of
  [ dd, mm, yyyy ] -> do
    d <- fromString dd
    m <- fromString mm
    y <- fromString yyyy
    if d >= 1 && d <= 31 && m >= 1 && m <= 12 && y >= 1000
      then Just { y, m, d }
      else Nothing
  _ -> Nothing

formatDate :: forall r1. { y :: Int, m :: Int, d :: Int | r1 } -> String
formatDate { y, m, d } = pad d <> "." <> pad m <> "." <> show y
  where
  pad n = (if n < 10 then "0" else "") <> show n

dateKey :: forall r1. { y :: Int, m :: Int, d :: Int | r1 } -> Int
dateKey { y, m, d } = y * 10000 + m * 100 + d
