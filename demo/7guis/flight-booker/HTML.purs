module FlightBooker.HTML (flightBooker) where

import Prelude (Unit, (#), identity, ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBooker (bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit)
import PUI (action, debounced, looped, with)
import PUI.Web ((<+>), choice, inCase, inCaseAt, staticText, text)
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
    ( Semigroupoid.do
      ( p $ text problemLine ) # inCase @"problem" bookingState
      ( p $ text oneWayLine ) # inCase @"one-way" bookingState
      ( p $ text returnLine ) # inCase @"return" bookingState
    ) # debounced itinerarySettleTime
    button @"Book"
    indeterminateLinearProgress # action @"Book" submit
    VariantToRecord.do
      output @"Flight booked" identity
      output @"Booking rejected" identity
