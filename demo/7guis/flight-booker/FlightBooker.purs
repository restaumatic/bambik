module FlightBooker (flightBooker) where

import Prelude ((#), ($), (&&), (*), (+), (/=), (<), (<$>), (<=), (<>), (==), (>=), (>>>), Unit, bind, pure, show, unit)

import Data.Either (Either(..), either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.String (Pattern(..), split)
import Data.Variant (expand)
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Aff)
import PUI (action, asCase, asField, completed, debounced, forCase, mvu, projection, required, updatesOn, widenRecordInput)
import PUI.HTML (body, provided, text)
import PUI.MDC (body1, button, card, elevation20, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $
    elevation20 $
      card { caption: "Book Flight" } Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              select { floatingLabel: "Flight type" }
                [ { value: .oneWay unit, label: "one-way flight" }
                , { value: .return unit, label: "return flight" }
                ] # required # asField @"flightType"
              filledTextField { floatingLabel: "Start date (DD.MM.YYYY)" } # asField @"start") # completed
          filledTextField { floatingLabel: "Return date (DD.MM.YYYY)" } # asField @"return" # provided # lcmap returnLeg # widenRecordInput # updatesOn setReturn
      ) # mvu plannedTrip
      body1 text # projection validationText # debounced # completed
      button { label: "Book", icon: "flight_takeoff" } # asCase @"book"
      indeterminateLinearProgress # action (match { book: submit })
      VariantToRecord.do
        snackbar # forCase @"booked"
        snackbar # forCase @"rejected"

returnBetween :: { y :: Int, m :: Int, d :: Int } -> { y :: Int, m :: Int, d :: Int } -> Maybe [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
returnBetween out back =
  if dateKey back >= dateKey out then Just (.returnBetween { out, back })
  else Nothing

parse :: { flightType :: [ oneWay :: Unit, return :: Unit ], start :: String, return :: String } -> Either String [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
parse b = case parseDate b.start of
  Nothing -> Left ("start date " <> show b.start <> " is not a valid DD.MM.YYYY date")
  Just start ->
    if b.flightType /= .return unit then Right (.oneWayOn start)
    else case parseDate b.return of
        Nothing -> Left ("return date " <> show b.return <> " is not a valid DD.MM.YYYY date")
        Just back -> case returnBetween start back of
          Nothing -> Left "the return date is before the start date"
          Just itinerary -> Right itinerary

validationText :: { flightType :: [ oneWay :: Unit, return :: Unit ], start :: String, return :: String } -> String
validationText = parse >>> either (\err -> "⚠ " <> err) summary

summary :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] -> String
summary = match
  { oneWayOn: \out -> "A one-way flight on " <> formatDate out
  , returnBetween: \r -> "A return flight: out " <> formatDate r.out <> ", back " <> formatDate r.back
  }

submit :: { flightType :: [ oneWay :: Unit, return :: Unit ], start :: String, return :: String } -> Aff [ booked :: String, rejected :: String ]
submit b = case parse b of
  Left err -> pure (.rejected ("Cannot book: " <> err))
  Right itinerary -> expand <$> bookFlight itinerary

bookFlight :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ] -> Aff [ booked :: String ]
bookFlight itinerary = pure (.booked ("You have booked: " <> summary itinerary))

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
formatDate dt = pad dt.d <> "." <> pad dt.m <> "." <> show dt.y
  where
  pad n = (if n < 10 then "0" else "") <> show n

dateKey :: { y :: Int, m :: Int, d :: Int } -> Int
dateKey dt = dt.y * 10000 + dt.m * 100 + dt.d

returnLeg :: { flightType :: [ oneWay :: Unit, return :: Unit ], return :: String } -> Maybe { return :: String }
returnLeg b = if b.flightType == .return unit then Just { return: b.return } else Nothing

setReturn :: { return :: String } -> { return :: String } -> { return :: String }
setReturn { return } b = b { return = return }

plannedTrip :: { flightType :: [ oneWay :: Unit, return :: Unit ], start :: String, return :: String }
plannedTrip = { flightType: .oneWay unit, start: "27.03.2026", return: "27.03.2026" }
