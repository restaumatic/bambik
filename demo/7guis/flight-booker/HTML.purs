module FlightBooker.HTML (flightBooker) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBooker (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit)
import PUI (action, atCase, debounced, looped, with)
import PUI.Web ((<+>), choice, inCase, inCaseAt, staticText, text)
import PUI.Web.HTML (body, button, div, input, label, output, p, select)
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
      p (text problemLine) # inCase @"problem"
        @( "one-way" :: {}, problem :: {}, return :: {} ) bookingState
      p (text oneWayLine) # inCase @"one-way" bookingState
      p (text returnLine) # inCase @"return" bookingState
    ) # debounced itinerarySettleTime
    button @"Book"
    ( VariantToRecord.do
      output @"Flight booked" bookedLine
      output @"Booking rejected" rejectedLine ) # action
        @( "Flight booked" :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
         , "Booking rejected" :: [ returnBeforeStart :: {}, unreadableReturn :: { input :: String }, unreadableStart :: { input :: String } ]
         ) submit # atCase @"Book"
