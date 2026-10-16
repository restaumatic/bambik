module FlightBooker.MDC3 (flightBooker) where

import Prelude (Unit, (#), identity, ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBooker (bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit)
import PUI (action, debounced, looped, with)
import PUI.Web ((<+>), choice, inCase, inCaseAt, text)
import PUI.Web.MDC3 (body, bodyLarge, button', filledTextField', indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type"
        (choice @"one-way" <+> choice @"return")
      filledTextField' @"Start date" { hint: "DD.MM.YYYY" }
      filledTextField' @"Return date" { hint: "DD.MM.YYYY" } # inCaseAt @"return" @"Flight type"
    ) # looped # with plannedTrip
    ( Semigroupoid.do
      ( bodyLarge $ text problemLine ) # inCase @"problem" bookingState
      ( bodyLarge $ text oneWayLine ) # inCase @"one-way" bookingState
      ( bodyLarge $ text returnLine ) # inCase @"return" bookingState
    ) # debounced itinerarySettleTime
    button' @"Book" { icon: "flight_takeoff" }
    indeterminateLinearProgress # action @"Book" submit
    VariantToRecord.do
      snackbar @"Flight booked" identity
      snackbar @"Booking rejected" identity
