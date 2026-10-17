module FlightBooker (Trip, bookingLine, bookingState, plannedTrip, returnDateInvalid, startDateInvalid) where

import Prelude (bind, otherwise, (#), (&&), (*), (+), (/=), (<=), (<>), (==), (>=), (||))

import Data.Int (fromString, rem)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), split)
import Data.Variant (match)

type Trip =
  { "Flight type" :: [ "one-way flight" :: {}, "return flight" :: {} ]
  , "Return date" :: String
  , "Start date" :: String
  }

type Date = { y :: Int, m :: Int, d :: Int }

plannedTrip :: Trip
plannedTrip = { "Flight type": ."one-way flight" {}, "Start date": "27.03.2026", "Return date": "27.03.2026" }

startDateInvalid :: Trip -> String
startDateInvalid { "Start date": input } = invalidity input

returnDateInvalid :: Trip -> String
returnDateInvalid { "Return date": input } = invalidity input

bookingState :: Trip -> [ bookable :: {}, unbookable :: {} ]
bookingState trip = if bookable trip then .bookable {} else .unbookable {}

bookingLine :: Trip -> String
bookingLine { "Flight type": flightType, "Start date": out, "Return date": back } = flightType # match
  { "one-way flight": \_ -> "You are going to book a one-way flight on " <> out <> "."
  , "return flight": \_ -> "You are going to book a return flight on " <> out <> ", returning on " <> back <> "."
  }

invalidity :: String -> String
invalidity input = maybe "true" (\_ -> "false") (parseDate input)

bookable :: Trip -> Boolean
bookable { "Flight type": flightType, "Start date": startInput, "Return date": returnInput } =
  case parseDate startInput of
    Nothing -> false
    Just out -> flightType # match
      { "one-way flight": \_ -> true
      , "return flight": \_ -> maybe false (\back -> dateKey out <= dateKey back) (parseDate returnInput)
      }

parseDate :: String -> Maybe Date
parseDate s = case split (Pattern ".") s of
  [ dd, mm, yyyy ] -> do
    d <- fromString dd
    m <- fromString mm
    y <- fromString yyyy
    if m >= 1 && m <= 12 && d >= 1 && d <= daysIn m y && y >= 1000
      then Just { y, m, d }
      else Nothing
  _ -> Nothing

daysIn :: Int -> Int -> Int
daysIn m y
  | m == 2 = if rem y 4 == 0 && (rem y 100 /= 0 || rem y 400 == 0) then 29 else 28
  | m == 4 || m == 6 || m == 9 || m == 11 = 30
  | otherwise = 31

dateKey :: forall r1. { y :: Int, m :: Int, d :: Int | r1 } -> Int
dateKey { y, m, d } = y * 10000 + m * 100 + d
