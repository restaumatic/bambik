module FlightBookerPolishViewModel (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit) where

import Prelude ((<), (<>), show)

import Data.Variant (match)
import Effect.Aff (Aff)
import FlightBookerViewModel (bookingState, itinerarySettleTime, plannedTrip, submit) as Booking

plannedTrip
  :: { "Flight type" :: [ "one-way" :: {}, return :: {} ]
     , "Return date" :: String
     , "Start date" :: String
     }
plannedTrip = Booking.plannedTrip

itinerarySettleTime :: { ms :: Number }
itinerarySettleTime = Booking.itinerarySettleTime

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
bookingState = Booking.bookingState

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
submit = Booking.submit

problemLine
  :: { problem :: [ returnBeforeStart :: {}
                  , unreadableReturn :: { input :: String }
                  , unreadableStart :: { input :: String }
                  ]
     }
  -> String
problemLine { problem } = "⚠ " <> problemText problem

oneWayLine :: { out :: { d :: Int, m :: Int, y :: Int } } -> String
oneWayLine { out } = summary (.oneWayOn out)

returnLine
  :: { back :: { d :: Int, m :: Int, y :: Int }, out :: { d :: Int, m :: Int, y :: Int } }
  -> String
returnLine { out, back } = summary (.returnBetween { out, back })

bookedLine
  :: [ oneWayOn :: { d :: Int, m :: Int, y :: Int }
     , returnBetween :: { back :: { d :: Int, m :: Int, y :: Int }
                        , out :: { d :: Int, m :: Int, y :: Int }
                        }
     ]
  -> String
bookedLine itinerary = "Zarezerwowano: " <> summary itinerary

rejectedLine
  :: [ returnBeforeStart :: {}
     , unreadableReturn :: { input :: String }
     , unreadableStart :: { input :: String }
     ]
  -> String
rejectedLine problem = "Nie można zarezerwować: " <> problemText problem

problemText :: [ returnBeforeStart :: {}, unreadableReturn :: { input :: String }, unreadableStart :: { input :: String } ] -> String
problemText = match
  { unreadableStart: \{ input } -> "data wylotu " <> show input <> " nie jest datą w formacie DD.MM.RRRR"
  , unreadableReturn: \{ input } -> "data powrotu " <> show input <> " nie jest datą w formacie DD.MM.RRRR"
  , returnBeforeStart: \_ -> "data powrotu jest wcześniejsza niż data wylotu"
  }

summary :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] -> String
summary = match
  { oneWayOn: \out -> "Lot w jedną stronę, " <> formatDate out
  , returnBetween: \r -> "Lot w obie strony: wylot " <> formatDate r.out <> ", powrót " <> formatDate r.back
  }

formatDate :: forall r1. { y :: Int, m :: Int, d :: Int | r1 } -> String
formatDate { y, m, d } = pad d <> "." <> pad m <> "." <> show y
  where
  pad n = (if n < 10 then "0" else "") <> show n
