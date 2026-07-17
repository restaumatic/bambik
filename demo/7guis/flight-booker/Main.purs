module Main (main) where

import Prelude

import Data.Either (Either(..))
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap, lcmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.String (Pattern(..), split)
import Data.Variant (match) as Variant
import Effect (Effect)
import Effect.Aff (Aff)
import PUI (action, asCase, asField, completed, debounced, field, forCase, forValue, mvu, projection)
import PUI.HTML (body, shownWhen, text) as HTML
import PUI.MDC (body1, button, card, elevation20, filledTextField, indeterminateLinearProgress, select, snackbar) as MDC
import QualifiedDo.Semigroupoid as Semigroupoid

data FlightType = OneWay | Return

derive instance Eq FlightType

type Booking =
  { flightType :: FlightType
  , start :: String
  , return :: String
  }

main :: Effect Unit
main =
  HTML.body $ MDC.elevation20 $ MDC.card { caption: Just "Book Flight" } Semigroupoid.do
  ( RecordToRecord.do
      MDC.select { floatingLabel: "Flight type" }
        [ { value: OneWay, label: "one-way flight" }
        , { value: Return, label: "return flight" }
        ]
        # dimap (\v -> { value: Just v }) _.value # field @"flightType"
      MDC.filledTextField { floatingLabel: "Start date (DD.MM.YYYY)" } # asField @"start"
      HTML.shownWhen isReturn
        ( MDC.filledTextField { floatingLabel: "Return date (DD.MM.YYYY)" } # asField @"return"
            # lcmap returnDate
        )
  ) # mvu { flightType: OneWay, start: "27.03.2026", return: "27.03.2026" }
  MDC.body1 (HTML.text # projection validationText # forValue) # debounced # completed
  RecordToVariant.do
    MDC.button { label: Just "Book", icon: Just "flight_takeoff" } # asCase @"book"
  MDC.indeterminateLinearProgress # action (Variant.match { book: bookFlight })
  VariantToRecord.do
    MDC.snackbar # forCase @"booked"
    MDC.snackbar # forCase @"rejected"

validationText :: Booking -> String
validationText b = case validate b of
  Left err -> "⚠ " <> err
  Right summary -> summary

validate :: Booking -> Either String String
validate b = case parseDate b.start of
  Nothing -> Left ("start date " <> show b.start <> " is not a valid DD.MM.YYYY date")
  Just start
    | b.flightType /= Return -> Right ("A one-way flight on " <> b.start)
    | otherwise -> case parseDate b.return of
        Nothing -> Left ("return date " <> show b.return <> " is not a valid DD.MM.YYYY date")
        Just return
          | return < start -> Left "the return date is before the start date"
          | otherwise -> Right ("A return flight: out " <> b.start <> ", back " <> b.return)

parseDate :: String -> Maybe { y :: Int, m :: Int, d :: Int }
parseDate s = case split (Pattern ".") s of
  [ dd, mm, yyyy ] -> do
    d <- Int.fromString dd
    m <- Int.fromString mm
    y <- Int.fromString yyyy
    if d >= 1 && d <= 31 && m >= 1 && m <= 12 && y >= 1000
      then Just { y, m, d }
      else Nothing
  _ -> Nothing

bookFlight :: Booking -> Aff [ booked :: String, rejected :: String ]
bookFlight b = pure case validate b of
  Left err -> .rejected ("Cannot book: " <> err)
  Right summary -> .booked ("You have booked: " <> summary)

isReturn :: Booking -> Boolean
isReturn b = b.flightType == Return

returnDate :: Booking -> { return :: String }
returnDate b = { return: b.return }
