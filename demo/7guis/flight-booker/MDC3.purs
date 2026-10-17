module FlightBooker.MDC3 (flightBooker) where

import Prelude (Unit, (#), ($), identity)

import Effect (Effect)
import FlightBooker (bookingLine, bookingState, itinerarySettleTime, plannedTrip, submit)
import PUI (action, debounced, looped, with)
import PUI.Web ((<+>), choice, inCaseAt, provided, text)
import PUI.Web.MDC3 (body, bodyLarge, button', filledTextField', indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type" (choice @"one-way" <+> choice @"return")
      filledTextField' @"Start date" { hint: "DD.MM.YYYY" }
      filledTextField' @"Return date" { hint: "DD.MM.YYYY" } # inCaseAt @"return" @"Flight type"
    ) # looped # with plannedTrip
    ( bodyLarge $ text bookingLine ) # debounced itinerarySettleTime
    button' @"Book" { icon: "flight_takeoff" } # provided @"bookable" bookingState
    indeterminateLinearProgress # action @"Book" submit
    snackbar @"Flight booked" identity
