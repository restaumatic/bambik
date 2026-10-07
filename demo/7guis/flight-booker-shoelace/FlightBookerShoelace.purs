module FlightBookerShoelace (flightBookerShoelace) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBookerViewModel (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit)
import PUI (action, atCase, debounced, looped, with)
import PUI.Web ((<+>), choice, inCase, shownWhen, text)
import PUI.Web.HTML (p)
import PUI.Web.Shoelace (body, button, select, textField, toast)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerShoelace :: Effect Unit
flightBookerShoelace =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type" {}
        (choice @"one-way" <+> choice @"return")
      textField @"Start date (DD.MM.YYYY)" {}
      textField @"Return date (DD.MM.YYYY)" {} # inCase @"return" _."Flight type"
    ) # looped
      @( "Flight type" :: [ "one-way" :: {}, "return" :: {} ]
       , "Start date (DD.MM.YYYY)" :: String
       , "Return date (DD.MM.YYYY)" :: String
       ) # with plannedTrip
    ( Semigroupoid.do
      p (text problemLine) # shownWhen @"problem"
        @( problem :: { problem :: String }
         , "one-way" :: { out :: { y :: Int, m :: Int, d :: Int } }
         , "return" :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } }
         ) bookingState
      p (text oneWayLine) # shownWhen @"one-way" bookingState
      p (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
    button @"Book" {}
    ( VariantToRecord.do
      toast @"Flight booked" bookedLine
      toast @"Booking rejected" rejectedLine ) # action
        @( "Flight booked" :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
         , "Booking rejected" :: String
         ) submit # atCase @"Book"
