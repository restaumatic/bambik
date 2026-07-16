module Main (main) where

import Prelude

import Data.Either (Either(..))
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap, lcmap)
import Data.Profunctor.Row.RecordToRecord (completed, field)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.String (Pattern(..), split)
import Data.Variant (case_, on) as Variant
import Effect (Effect)
import Effect.Aff (Aff)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import Type.Proxy (Proxy(..))
import UI (action, debounced, looped)
import Web (bodyWith, shownWhen, text)

type Booking =
  { flightType :: String
  , start :: String
  , return :: String
  }

main :: Effect Unit
main = bodyWith { flightType: "one-way", start: "27.03.2026", return: "27.03.2026" } $ MDC.elevation20 $ MDC.card { caption: Just "Book Flight" } Semigroupoid.do
  looped RecordToRecord.do
    field @"flightType" $ dimap (\v -> { selected: Just v }) _.selected $
      MDC.select @"selected" { floatingLabel: "Flight type" }
        [ { value: "one-way", label: "one-way flight" }
        , { value: "return", label: "return flight" }
        ]
    MDC.filledTextField @"start" { floatingLabel: "Start date (DD.MM.YYYY)" }
    shownWhen isReturn $ lcmap returnDate $
      MDC.filledTextField @"return" { floatingLabel: "Return date (DD.MM.YYYY)" }
  completed $ debounced $ MDC.body1 $ lcmap validationText text
  RecordToVariant.do
    MDC.button @"book" { label: Just "Book", icon: Just "flight_takeoff" }
  action (Variant.case_ # Variant.on (Proxy @"book") bookFlight) MDC.indeterminateLinearProgress
  VariantToRecord.do
    MDC.snackbar @"booked"
    MDC.snackbar @"rejected"

validationText :: Booking -> String
validationText b = case validate b of
  Left err -> "⚠ " <> err
  Right summary -> summary

validate :: Booking -> Either String String
validate b = case parseDate b.start of
  Nothing -> Left ("start date " <> show b.start <> " is not a valid DD.MM.YYYY date")
  Just start
    | b.flightType /= "return" -> Right ("A one-way flight on " <> b.start)
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
isReturn b = b.flightType == "return"

returnDate :: Booking -> { return :: String }
returnDate b = { return: b.return }
