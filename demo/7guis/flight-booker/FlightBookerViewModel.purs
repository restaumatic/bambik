module FlightBookerViewModel (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit) where

import Prelude ((&&), (*), (+), (/=), (<), (<$>), (<=), (<>), (>=), (>>>), bind, pure, show)

import Data.Either (Either(..), either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Variant (expand, match)
import Effect.Aff (Aff)

plannedTrip
  :: { "Flight type" :: [ "one-way" :: {}, return :: {} ]
     , "Return date (DD.MM.YYYY)" :: String
     , "Start date (DD.MM.YYYY)" :: String
     }
plannedTrip = { "Flight type": ."one-way" {}, "Start date (DD.MM.YYYY)": "27.03.2026", "Return date (DD.MM.YYYY)": "27.03.2026" }

itinerarySettleTime :: { ms :: Number }
itinerarySettleTime = { ms: 300.0 }

bookedLine
  :: [ oneWayOn :: { d :: Int, m :: Int, y :: Int }
     , returnBetween :: { back :: { d :: Int, m :: Int, y :: Int }
                        , out :: { d :: Int, m :: Int, y :: Int }
                        }
     ]
  -> String
bookedLine itinerary = "You have booked: " <> summary itinerary

rejectedLine :: String -> String
rejectedLine problem = "Cannot book: " <> problem

returnBetween :: forall r1. { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } | r1 } -> Maybe [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
returnBetween { out, back } =
  if dateKey back >= dateKey out then Just (.returnBetween { out, back })
  else Nothing

parse :: forall r1. { "Flight type" :: [ "one-way" :: {}, "return" :: {} ], "Start date (DD.MM.YYYY)" :: String, "Return date (DD.MM.YYYY)" :: String | r1 } -> Either String [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
parse { "Flight type": flightType, "Start date (DD.MM.YYYY)": startInput, "Return date (DD.MM.YYYY)": returnInput } = case parseDate startInput of
  Nothing -> Left ("start date " <> show startInput <> " is not a valid DD.MM.YYYY date")
  Just start ->
    if flightType /= ."return" {} then Right (.oneWayOn start)
    else case parseDate returnInput of
      Nothing -> Left ("return date " <> show returnInput <> " is not a valid DD.MM.YYYY date")
      Just back -> case returnBetween { out: start, back } of
        Nothing -> Left "the return date is before the start date"
        Just itinerary -> Right itinerary

bookingState
  :: { "Flight type" :: [ "one-way" :: {}, return :: {} ]
     , "Return date (DD.MM.YYYY)" :: String
     , "Start date (DD.MM.YYYY)" :: String
     }
  -> [ "one-way" :: { out :: { d :: Int, m :: Int, y :: Int } }
     , problem :: { problem :: String }
     , return :: { back :: { d :: Int, m :: Int, y :: Int }
                 , out :: { d :: Int, m :: Int, y :: Int }
                 }
     ]
bookingState = parse >>> either (\problem -> .problem { problem })
  (match
    { oneWayOn: \out -> ."one-way" { out }
    , returnBetween: ."return"
    })

problemLine :: { problem :: String } -> String
problemLine { problem } = "⚠ " <> problem

oneWayLine :: { out :: { d :: Int, m :: Int, y :: Int } } -> String
oneWayLine { out } = summary (.oneWayOn out)

returnLine
  :: { back :: { d :: Int, m :: Int, y :: Int }, out :: { d :: Int, m :: Int, y :: Int } }
  -> String
returnLine r = summary (.returnBetween { out: r.out, back: r.back })

summary :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] -> String
summary = match
  { oneWayOn: \out -> "A one-way flight on " <> formatDate out
  , returnBetween: \r -> "A return flight: out " <> formatDate r.out <> ", back " <> formatDate r.back
  }

submit
  :: { "Flight type" :: [ "one-way" :: {}, return :: {} ]
     , "Return date (DD.MM.YYYY)" :: String
     , "Start date (DD.MM.YYYY)" :: String
     }
  -> Aff [ "Booking rejected" :: String
         , "Flight booked" :: [ oneWayOn :: { d :: Int, m :: Int, y :: Int }
                              , returnBetween :: { back :: { d :: Int, m :: Int, y :: Int }
                                                 , out :: { d :: Int, m :: Int, y :: Int }
                                                 }
                              ]
         ]
submit trip = case parse trip of
  Left problem -> pure (."Booking rejected" problem)
  Right itinerary -> expand <$> bookFlight itinerary

bookFlight :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] -> Aff [ "Flight booked" :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] ]
bookFlight itinerary = pure (."Flight booked" itinerary)

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

formatDate :: forall r1. { y :: Int, m :: Int, d :: Int | r1 } -> String
formatDate { y, m, d } = pad d <> "." <> pad m <> "." <> show y
  where
  pad n = (if n < 10 then "0" else "") <> show n

dateKey :: forall r1. { y :: Int, m :: Int, d :: Int | r1 } -> Int
dateKey { y, m, d } = y * 10000 + m * 100 + d
