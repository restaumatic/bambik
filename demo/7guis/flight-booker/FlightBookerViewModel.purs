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
     , "Return date" :: String
     , "Start date" :: String
     }
plannedTrip = { "Flight type": ."one-way" {}, "Start date": "27.03.2026", "Return date": "27.03.2026" }

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

rejectedLine
  :: [ returnBeforeStart :: {}
     , unreadableReturn :: { input :: String }
     , unreadableStart :: { input :: String }
     ]
  -> String
rejectedLine problem = "Cannot book: " <> problemText problem

returnBetween :: forall r1. { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } | r1 } -> Maybe [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
returnBetween { out, back } =
  if dateKey back >= dateKey out then Just (.returnBetween { out, back })
  else Nothing

parse :: forall r1. { "Flight type" :: [ "one-way" :: {}, "return" :: {} ], "Start date" :: String, "Return date" :: String | r1 } -> Either [ returnBeforeStart :: {}, unreadableReturn :: { input :: String }, unreadableStart :: { input :: String } ] [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
parse { "Flight type": flightType, "Start date": startInput, "Return date": returnInput } = case parseDate startInput of
  Nothing -> Left (.unreadableStart { input: startInput })
  Just start ->
    if flightType /= ."return" {} then Right (.oneWayOn start)
    else case parseDate returnInput of
      Nothing -> Left (.unreadableReturn { input: returnInput })
      Just back -> case returnBetween { out: start, back } of
        Nothing -> Left (.returnBeforeStart {})
        Just itinerary -> Right itinerary

bookingState
  :: { "Flight type" :: [ "one-way" :: {}, return :: {} ]
     , "Return date" :: String
     , "Start date" :: String
     }
  -> [ "one-way" :: { out :: { d :: Int, m :: Int, y :: Int } }
     , problem :: { problem :: [ returnBeforeStart :: {}
                               , unreadableReturn :: { input :: String }
                               , unreadableStart :: { input :: String }
                               ]
                  }
     , return :: { back :: { d :: Int, m :: Int, y :: Int }
                 , out :: { d :: Int, m :: Int, y :: Int }
                 }
     ]
bookingState = parse >>> either (\problem -> .problem { problem })
  (match
    { oneWayOn: \out -> ."one-way" { out }
    , returnBetween: ."return"
    })

problemLine
  :: { problem :: [ returnBeforeStart :: {}
                  , unreadableReturn :: { input :: String }
                  , unreadableStart :: { input :: String }
                  ]
     }
  -> String
problemLine { problem } = "⚠ " <> problemText problem

problemText :: [ returnBeforeStart :: {}, unreadableReturn :: { input :: String }, unreadableStart :: { input :: String } ] -> String
problemText = match
  { unreadableStart: \{ input } -> "start date " <> show input <> " is not a valid DD.MM.YYYY date"
  , unreadableReturn: \{ input } -> "return date " <> show input <> " is not a valid DD.MM.YYYY date"
  , returnBeforeStart: \_ -> "the return date is before the start date"
  }

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
     , "Return date" :: String
     , "Start date" :: String
     }
  -> Aff [ "Booking rejected" :: [ returnBeforeStart :: {}
                                 , unreadableReturn :: { input :: String }
                                 , unreadableStart :: { input :: String }
                                 ]
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
