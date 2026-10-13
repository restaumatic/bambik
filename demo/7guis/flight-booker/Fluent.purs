module FlightBooker.Fluent (flightBooker) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBooker (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit)
import PUI (action, atCase, debounced, looped, with)
import PUI.Web ((<+>), choice, inCase, inCaseAt, text)
import PUI.Web.Fluent (body, body1, button, dropdown, messageBar, textField)
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
      body1 (text problemLine) # inCase @"problem"
        @( "one-way" :: {}, problem :: {}, return :: {} ) bookingState
      body1 (text oneWayLine) # inCase @"one-way" bookingState
      body1 (text returnLine) # inCase @"return" bookingState
    ) # debounced itinerarySettleTime
    button @"Book"
    ( VariantToRecord.do
      messageBar @"Flight booked" bookedLine
      messageBar @"Booking rejected" rejectedLine ) # action
        @( "Flight booked" :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
         , "Booking rejected" :: [ returnBeforeStart :: {}, unreadableReturn :: { input :: String }, unreadableStart :: { input :: String } ]
         ) submit # atCase @"Book"
