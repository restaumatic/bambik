module FlightBooker.Shoelace (flightBooker) where

import Prelude (Unit, (#), ($), identity)

import Effect (Effect)
import FlightBooker (bookingLine, bookingState, itinerarySettleTime, plannedTrip, submit)
import PUI (action, debounced, looped, with)
import PUI.Web ((<+>), choice, inCaseAt, provided, text)
import PUI.Web.HTML (p)
import PUI.Web.Shoelace (body, button, indeterminateLinearProgress, select, textField', toast)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type" (choice @"one-way" <+> choice @"return")
      textField' @"Start date" { hint: "DD.MM.YYYY" }
      textField' @"Return date" { hint: "DD.MM.YYYY" } # inCaseAt @"return" @"Flight type"
    ) # looped # with plannedTrip
    ( p $ text bookingLine ) # debounced itinerarySettleTime
    button @"Book" # provided @"bookable" bookingState
    indeterminateLinearProgress # action @"Book" submit
    toast @"Flight booked" identity
