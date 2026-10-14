module FlightBooker.Bootstrap (flightBooker) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBooker (bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit)
import PUI (action, atCase, debounced, joined, kept, looped, with)
import PUI.Web ((<+>), choice, inCase, inCaseAt, text)
import PUI.Web.Bootstrap (body, button, indeterminateLinearProgress, select, textField, toast)
import PUI.Web.HTML (p)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type"
        (choice @"one-way" <+> choice @"return")
      textField @"Start date (DD.MM.YYYY)"
      textField @"Return date (DD.MM.YYYY)" # inCaseAt @"return" @"Flight type"
    ) # looped
      @( "Flight type" :: [ "one-way" :: {}, "return" :: {} ]
       , "Start date (DD.MM.YYYY)" :: String
       , "Return date (DD.MM.YYYY)" :: String
       ) # with plannedTrip
    ( Semigroupoid.do
      ( p $ text problemLine ) # inCase @"problem"
        @( "one-way" :: {}, problem :: {}, return :: {} ) bookingState
      ( p $ text oneWayLine ) # inCase @"one-way" bookingState
      ( p $ text returnLine ) # inCase @"return" bookingState
    ) # debounced itinerarySettleTime
    ( Semigroupoid.do
      button @"Book"
      indeterminateLinearProgress # action
        @( "Flight booked" :: {}, "Booking rejected" :: {} ) submit # atCase @"Book" ) # joined
    VariantToRecord.do
      toast @"Flight booked" kept
      toast @"Booking rejected" kept
