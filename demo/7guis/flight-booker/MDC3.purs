module FlightBooker.MDC3 (flightBooker) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBooker (bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit)
import PUI (action, debounced, joined, kept, looped, with)
import PUI.Web ((<+>), choice, inCase, inCaseAt, text)
import PUI.Web.MDC3 (body, bodyLarge, button', filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type"
        (choice @"one-way" <+> choice @"return")
      filledTextField @"Start date (DD.MM.YYYY)"
      filledTextField @"Return date (DD.MM.YYYY)" # inCaseAt @"return" @"Flight type"
    ) # looped
      @( "Flight type" :: [ "one-way" :: {}, "return" :: {} ]
       , "Start date (DD.MM.YYYY)" :: String
       , "Return date (DD.MM.YYYY)" :: String
       ) # with plannedTrip
    ( Semigroupoid.do
      ( bodyLarge $ text problemLine ) # inCase @"problem"
        @( "one-way" :: {}, problem :: {}, return :: {} ) bookingState
      ( bodyLarge $ text oneWayLine ) # inCase @"one-way" bookingState
      ( bodyLarge $ text returnLine ) # inCase @"return" bookingState
    ) # debounced itinerarySettleTime
    ( Semigroupoid.do
      button' @"Book" { icon: "flight_takeoff" }
      indeterminateLinearProgress # action @"Book"
        @( "Flight booked" :: {}, "Booking rejected" :: {} ) submit ) # joined @"Flight booked" # joined @"Booking rejected"
    VariantToRecord.do
      snackbar @"Flight booked" kept
      snackbar @"Booking rejected" kept
