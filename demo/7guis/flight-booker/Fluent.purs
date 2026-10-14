module FlightBooker.Fluent (flightBooker) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBooker (bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit)
import PUI (action, atCase, debounced, joined, kept, looped, with)
import PUI.Web ((<+>), choice, inCase, inCaseAt, text)
import PUI.Web.Fluent (body, body1, button, dropdown, indeterminateLinearProgress, messageBar, textField)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      dropdown @"Flight type"
        (choice @"one-way" <+> choice @"return")
      textField @"Start date (DD.MM.YYYY)"
      textField @"Return date (DD.MM.YYYY)" # inCaseAt @"return" @"Flight type"
    ) # looped
      @( "Flight type" :: [ "one-way" :: {}, "return" :: {} ]
       , "Start date (DD.MM.YYYY)" :: String
       , "Return date (DD.MM.YYYY)" :: String
       ) # with plannedTrip
    ( Semigroupoid.do
      ( body1 $ text problemLine ) # inCase @"problem"
        @( "one-way" :: {}, problem :: {}, return :: {} ) bookingState
      ( body1 $ text oneWayLine ) # inCase @"one-way" bookingState
      ( body1 $ text returnLine ) # inCase @"return" bookingState
    ) # debounced itinerarySettleTime
    ( Semigroupoid.do
      button @"Book"
      indeterminateLinearProgress # action
        @( "Flight booked" :: {}, "Booking rejected" :: {} ) submit # atCase @"Book" ) # joined
    VariantToRecord.do
      messageBar @"Flight booked" kept
      messageBar @"Booking rejected" kept
