module FlightBooker (flightBooker) where

import Prelude ((#), ($), (&&), (*), (+), (/=), (<), (<$>), (<=), (<>), (==), (>=), (>>>), class Eq, Unit, bind, pure, show)

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
import PUI (action, asCase, asField, completed, debounced, forCase, mvu, projection, required, updates)
import PUI.HTML (body, provided, text)
import PUI.MDC (body1, button, card, elevation20, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

data FlightType = OneWay | Return

derive instance Eq FlightType

type Booking =
  { flightType :: FlightType
  , start :: String
  , return :: String
  }

data Itinerary
  = OneWayOn Date
  | ReturnBetween { out :: Date, back :: Date }

type Date = { y :: Int, m :: Int, d :: Int }

flightBooker :: Effect Unit
flightBooker =
  body $
    elevation20 $
      card { caption: "Book Flight" } Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              select { floatingLabel: "Flight type" }
                [ { value: OneWay, label: "one-way flight" }
                , { value: Return, label: "return flight" }
                ]
                # required # asField @"flightType"
              filledTextField { floatingLabel: "Start date (DD.MM.YYYY)" } # asField @"start"
          ) # completed
          filledTextField { floatingLabel: "Return date (DD.MM.YYYY)" } # asField @"return"
            # provided # lcmap returnLeg # updates setReturn
      ) # mvu plannedTrip
      body1 (text # projection validationText) # debounced # completed
      button { label: "Book", icon: "flight_takeoff" } # asCase @"book"
      indeterminateLinearProgress # action (match { book: submit })
      VariantToRecord.do
        snackbar # forCase @"booked"
        snackbar # forCase @"rejected"

returnBetween :: Date -> Date -> Maybe Itinerary
returnBetween out back =
  if dateKey back >= dateKey out then Just (ReturnBetween { out, back })
  else Nothing

parse :: Booking -> Either String Itinerary
parse b = case parseDate b.start of
  Nothing -> Left ("start date " <> show b.start <> " is not a valid DD.MM.YYYY date")
  Just start ->
    if b.flightType /= Return then Right (OneWayOn start)
    else case parseDate b.return of
        Nothing -> Left ("return date " <> show b.return <> " is not a valid DD.MM.YYYY date")
        Just back -> case returnBetween start back of
          Nothing -> Left "the return date is before the start date"
          Just itinerary -> Right itinerary

validationText :: Booking -> String
validationText = parse >>> either (\err -> "⚠ " <> err) summary

summary :: Itinerary -> String
summary (OneWayOn out) = "A one-way flight on " <> formatDate out
summary (ReturnBetween r) = "A return flight: out " <> formatDate r.out <> ", back " <> formatDate r.back

submit :: Booking -> Aff [ booked :: String, rejected :: String ]
submit b = case parse b of
  Left err -> pure (.rejected ("Cannot book: " <> err))
  Right itinerary -> expand <$> bookFlight itinerary

bookFlight :: Itinerary -> Aff [ booked :: String ]
bookFlight itinerary = pure (.booked ("You have booked: " <> summary itinerary))

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

formatDate :: Date -> String
formatDate dt = pad dt.d <> "." <> pad dt.m <> "." <> show dt.y
  where
  pad n = (if n < 10 then "0" else "") <> show n

dateKey :: Date -> Int
dateKey dt = dt.y * 10000 + dt.m * 100 + dt.d

returnLeg :: Booking -> Maybe { return :: String }
returnLeg b = if b.flightType == Return then Just { return: b.return } else Nothing

setReturn :: { return :: String } -> Booking -> Booking
setReturn { return } b = b { return = return }

plannedTrip :: Booking
plannedTrip = { flightType: OneWay, start: "27.03.2026", return: "27.03.2026" }
