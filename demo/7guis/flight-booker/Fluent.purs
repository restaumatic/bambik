module FlightBooker.Fluent (flightBooker) where

import Prelude (Unit, (#), identity, ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBooker (bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit)
import PUI (action, debounced, looped, with)
import PUI.Web ((<+>), choice, inCase, inCaseAt, text)
import PUI.Web.Fluent (body, body1, button, dropdown, indeterminateLinearProgress, messageBar, textField')
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      dropdown @"Flight type"
        (choice @"one-way" <+> choice @"return")
      textField' @"Start date" { hint: "DD.MM.YYYY" }
      textField' @"Return date" { hint: "DD.MM.YYYY" } # inCaseAt @"return" @"Flight type"
    ) # looped # with plannedTrip
    ( Semigroupoid.do
      ( body1 $ text problemLine ) # inCase @"problem" bookingState
      ( body1 $ text oneWayLine ) # inCase @"one-way" bookingState
      ( body1 $ text returnLine ) # inCase @"return" bookingState
    ) # debounced itinerarySettleTime
    button @"Book"
    indeterminateLinearProgress # action @"Book" submit
    VariantToRecord.do
      messageBar @"Flight booked" identity
      messageBar @"Booking rejected" identity
