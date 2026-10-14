module FlightBooker.HTML (flightBooker) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBooker (bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit)
import PUI (action, atCase, debounced, joined, kept, looped, with)
import PUI.Web ((<+>), choice, inCase, inCaseAt, staticText, text)
import PUI.Web.HTML (body, button, div, indeterminateLinearProgress, input, label, output, p, select)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ div $ Semigroupoid.do
    ( Semigroupoid.do
      p ( label $ Semigroupoid.do
        staticText "Flight type "
        select @"Flight type" (choice @"one-way" <+> choice @"return") )
      p ( label $ Semigroupoid.do
        staticText "Start date (DD.MM.YYYY) "
        input @"Start date (DD.MM.YYYY)" "text" )
      p ( label $ Semigroupoid.do
        staticText "Return date (DD.MM.YYYY) "
        input @"Return date (DD.MM.YYYY)" "text" ) # inCaseAt @"return" @"Flight type"
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
      output @"Flight booked" kept
      output @"Booking rejected" kept
