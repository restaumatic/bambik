module FlightBooker.HTML (flightBooker) where

import Prelude (Unit, (#), ($), identity)

import Effect (Effect)
import FlightBooker (bookingLine, bookingState, itinerarySettleTime, plannedTrip, submit)
import PUI (action, debounced, looped, with)
import PUI.Web ((<+>), choice, inCaseAt, provided, staticText, text)
import PUI.Web.HTML (body, button, div, indeterminateLinearProgress, input, label, output, p, select)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ div $ Semigroupoid.do
    ( Semigroupoid.do
      p $ label $ Semigroupoid.do
        staticText "Flight type "
        select @"Flight type" (choice @"one-way" <+> choice @"return")
      p $ label $ Semigroupoid.do
        staticText "Start date (DD.MM.YYYY) "
        input @"Start date" "text"
      ( p $ label $ Semigroupoid.do
        staticText "Return date (DD.MM.YYYY) "
        input @"Return date" "text" ) # inCaseAt @"return" @"Flight type"
    ) # looped # with plannedTrip
    ( p $ text bookingLine ) # debounced itinerarySettleTime
    button @"Book" # provided @"bookable" bookingState
    indeterminateLinearProgress # action @"Book" submit
    output @"Flight booked" identity
