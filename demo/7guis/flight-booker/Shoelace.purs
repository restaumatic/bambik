module FlightBooker.Shoelace (flightBooker) where

import Prelude (Unit, (#), identity, ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBooker (bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit)
import PUI (action, debounced, looped, with)
import PUI.Web ((<+>), choice, inCase, inCaseAt, text)
import PUI.Web.HTML (p)
import PUI.Web.Shoelace (body, button, indeterminateLinearProgress, select, textField, toast)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type"
        (choice @"one-way" <+> choice @"return")
      textField @"Start date (DD.MM.YYYY)"
      textField @"Return date (DD.MM.YYYY)" # inCaseAt @"return" @"Flight type"
    ) # looped # with plannedTrip
    ( Semigroupoid.do
      ( p $ text problemLine ) # inCase @"problem" bookingState
      ( p $ text oneWayLine ) # inCase @"one-way" bookingState
      ( p $ text returnLine ) # inCase @"return" bookingState
    ) # debounced itinerarySettleTime
    button @"Book"
    indeterminateLinearProgress # action @"Book" submit
    VariantToRecord.do
      toast @"Flight booked" identity
      toast @"Booking rejected" identity
