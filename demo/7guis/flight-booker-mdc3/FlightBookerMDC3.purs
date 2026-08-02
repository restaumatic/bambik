module FlightBookerMDC3 (flightBookerMDC3) where

import Prelude (identity, (#), ($), (&&), (*), (+), (/=), (<), (<$>), (<=), (<>), (==), (>=), (>>>), Unit, bind, pure, show)

import Data.Either (Either(..), either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.String (Pattern(..), split)
import Data.Variant (expand, match)
import Effect (Effect)
import Effect.Aff (Aff)
import PUI (PUI, action, asCase, asField, completed, debounced, displayed, forCases, forField, mvu, required, updated)
import PUI.HTML (body, provided, staticText, text)
import PUI.Web (Web)
import PUI.MDC3 (bodyLarge, button, card, elevation5, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerMDC3 :: Effect Unit
flightBookerMDC3 =
  body $
    elevation5 $
      card { caption: "Book Flight" } Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              select { floatingLabel: "Flight type" }
                [ { value: .oneWay {}, label: "one-way flight" }
                , { value: .return {}, label: "return flight" }
                ] # required # asField @"flightType"
              filledTextField { floatingLabel: "Start date (DD.MM.YYYY)" } # asField @"start") # completed
          filledTextField { floatingLabel: "Return date (DD.MM.YYYY)" } # asField @"return" # provided returnLeg # updated setReturn
      ) # mvu plannedTrip
      ( Semigroupoid.do
          bodyLarge ( RecordToRecord.do
              staticText "⚠ "
              text # forField @"problem" identity ) # provided bookingProblem # displayed
          bodyLarge ( RecordToRecord.do
              staticText "A one-way flight on "
              text # forField @"date" identity ) # provided oneWayItinerary # displayed
          bodyLarge ( RecordToRecord.do
              staticText "A return flight: out "
              text # forField @"out" identity
              staticText ", back "
              text # forField @"back" identity ) # provided returnItinerary # displayed
      ) # debounced itinerarySettleTime
      button { label: "Book", icon: "flight_takeoff" } # asCase @"book"
      indeterminateLinearProgress # action (match { book: submit })
      bookingToast

bookingToast :: PUI Web [ booked :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ], rejected :: String ] {}
bookingToast = snackbar # forCases bookingLine

bookingLine :: [ booked :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ], rejected :: String ] -> String
bookingLine = match
  { booked: \itinerary -> "You have booked: " <> summary itinerary
  , rejected: \problem -> "Cannot book: " <> problem
  }

returnBetween :: { y :: Int, m :: Int, d :: Int } -> { y :: Int, m :: Int, d :: Int } -> Maybe [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
returnBetween out back =
  if dateKey back >= dateKey out then Just (.returnBetween { out, back })
  else Nothing

parse :: { flightType :: [ oneWay :: {}, return :: {} ], start :: String, return :: String } -> Either String [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
parse { flightType, start: startInput, return: returnInput } = case parseDate startInput of
  Nothing -> Left ("start date " <> show startInput <> " is not a valid DD.MM.YYYY date")
  Just start ->
    if flightType /= .return {} then Right (.oneWayOn start)
    else case parseDate returnInput of
        Nothing -> Left ("return date " <> show returnInput <> " is not a valid DD.MM.YYYY date")
        Just back -> case returnBetween start back of
          Nothing -> Left "the return date is before the start date"
          Just itinerary -> Right itinerary

bookingProblem :: { flightType :: [ oneWay :: {}, return :: {} ], start :: String, return :: String } -> Maybe { problem :: String }
bookingProblem = parse >>> either (\problem -> Just { problem }) (\_ -> Nothing)

oneWayItinerary :: { flightType :: [ oneWay :: {}, return :: {} ], start :: String, return :: String } -> Maybe { date :: String }
oneWayItinerary = parse >>> either (\_ -> Nothing)
  (match { oneWayOn: \out -> Just { date: formatDate out }, returnBetween: \_ -> Nothing })

returnItinerary :: { flightType :: [ oneWay :: {}, return :: {} ], start :: String, return :: String } -> Maybe { out :: String, back :: String }
returnItinerary = parse >>> either (\_ -> Nothing)
  (match { oneWayOn: \_ -> Nothing, returnBetween: \r -> Just { out: formatDate r.out, back: formatDate r.back } })

summary :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] -> String
summary = match
  { oneWayOn: \out -> "A one-way flight on " <> formatDate out
  , returnBetween: \r -> "A return flight: out " <> formatDate r.out <> ", back " <> formatDate r.back
  }

submit :: { flightType :: [ oneWay :: {}, return :: {} ], start :: String, return :: String } -> Aff [ booked :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ], rejected :: String ]
submit { flightType, start, return } = case parse { flightType, start, return } of
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

returnLeg :: { flightType :: [ oneWay :: {}, return :: {} ], return :: String } -> Maybe { return :: String }
returnLeg { flightType, return } = if flightType == .return {} then Just { return } else Nothing

setReturn :: { return :: String } -> { return :: String } -> { return :: String }
setReturn { return } b = b { return = return }

plannedTrip :: { flightType :: [ oneWay :: {}, return :: {} ], start :: String, return :: String }
plannedTrip = { flightType: .oneWay {}, start: "27.03.2026", return: "27.03.2026" }

itinerarySettleTime :: { ms :: Number }
itinerarySettleTime = { ms: 300.0 }
