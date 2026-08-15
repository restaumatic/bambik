module FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit) where

import Prelude ((&&), (*), (+), (/=), (<), (<$>), (<=), (<>), (==), (>=), (>>>), bind, pure, show)

import Data.Either (Either(..), either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Variant (expand, match)
import Effect.Aff (Aff)

plannedTrip :: { "Flight type" :: [ "one-way flight" :: {}, "return flight" :: {} ], "Start date (DD.MM.YYYY)" :: String, "Return date (DD.MM.YYYY)" :: String }
plannedTrip = { "Flight type": ."one-way flight" {}, "Start date (DD.MM.YYYY)": "27.03.2026", "Return date (DD.MM.YYYY)": "27.03.2026" }

itinerarySettleTime :: { ms :: Number }
itinerarySettleTime = { ms: 300.0 }

bookingLine :: [ booked :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ], rejected :: String ] -> String
bookingLine = match
  { booked: \itinerary -> "You have booked: " <> summary itinerary
  , rejected: \problem -> "Cannot book: " <> problem
  }

returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } -> Maybe [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
returnBetween { out, back } =
  if dateKey back >= dateKey out then Just (.returnBetween { out, back })
  else Nothing

parse :: { "Flight type" :: [ "one-way flight" :: {}, "return flight" :: {} ], "Start date (DD.MM.YYYY)" :: String, "Return date (DD.MM.YYYY)" :: String } -> Either String [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
parse { "Flight type": flightType, "Start date (DD.MM.YYYY)": startInput, "Return date (DD.MM.YYYY)": returnInput } = case parseDate startInput of
  Nothing -> Left ("start date " <> show startInput <> " is not a valid DD.MM.YYYY date")
  Just start ->
    if flightType /= ."return flight" {} then Right (.oneWayOn start)
    else case parseDate returnInput of
        Nothing -> Left ("return date " <> show returnInput <> " is not a valid DD.MM.YYYY date")
        Just back -> case returnBetween { out: start, back } of
          Nothing -> Left "the return date is before the start date"
          Just itinerary -> Right itinerary

bookingState :: { "Flight type" :: [ "one-way flight" :: {}, "return flight" :: {} ], "Start date (DD.MM.YYYY)" :: String, "Return date (DD.MM.YYYY)" :: String } -> [ problem :: { problem :: String }, "one-way flight" :: { date :: String }, "return flight" :: { out :: String, back :: String } ]
bookingState = parse >>> either (\problem -> .problem { problem })
  (match
    { oneWayOn: \out -> ."one-way flight" { date: formatDate out }
    , returnBetween: \r -> ."return flight" { out: formatDate r.out, back: formatDate r.back }
    })

summary :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] -> String
summary = match
  { oneWayOn: \out -> "A one-way flight on " <> formatDate out
  , returnBetween: \r -> "A return flight: out " <> formatDate r.out <> ", back " <> formatDate r.back
  }

submit :: { "Flight type" :: [ "one-way flight" :: {}, "return flight" :: {} ], "Start date (DD.MM.YYYY)" :: String, "Return date (DD.MM.YYYY)" :: String } -> Aff [ booked :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ], rejected :: String ]
submit { "Flight type": flightType, "Start date (DD.MM.YYYY)": start, "Return date (DD.MM.YYYY)": back } = case parse { "Flight type": flightType, "Start date (DD.MM.YYYY)": start, "Return date (DD.MM.YYYY)": back } of
  Left problem -> pure (.rejected problem)
  Right itinerary -> expand <$> bookFlight itinerary

bookFlight :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] -> Aff [ booked :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] ]
bookFlight itinerary = pure (.booked itinerary)

parseDate :: String -> Maybe { y :: Int, m :: Int, d :: Int }
parseDate s = case split (Pattern ".") s of
  [ dd, mm, yyyy ] -> do
    d <- fromString dd
    m <- fromString mm
    y <- fromString yyyy
    if d >= 1 && d <= 31 && m >= 1 && m <= 12 && y >= 1000
      then Just { y, m, d }
      else Nothing
  _ -> Nothing

formatDate :: { y :: Int, m :: Int, d :: Int } -> String
formatDate { y, m, d } = pad d <> "." <> pad m <> "." <> show y
  where
  pad n = (if n < 10 then "0" else "") <> show n

dateKey :: { y :: Int, m :: Int, d :: Int } -> Int
dateKey { y, m, d } = y * 10000 + m * 100 + d

returnLeg :: { "Flight type" :: [ "one-way flight" :: {}, "return flight" :: {} ], "Return date (DD.MM.YYYY)" :: String } -> Maybe { "Return date (DD.MM.YYYY)" :: String }
returnLeg { "Flight type": flightType, "Return date (DD.MM.YYYY)": back } = if flightType == ."return flight" {} then Just { "Return date (DD.MM.YYYY)": back } else Nothing

setReturn :: { "Return date (DD.MM.YYYY)" :: String } -> { "Return date (DD.MM.YYYY)" :: String }
setReturn { "Return date (DD.MM.YYYY)": back } = { "Return date (DD.MM.YYYY)": back }
