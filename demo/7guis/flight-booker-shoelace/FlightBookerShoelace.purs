module FlightBookerShoelace (flightBookerShoelace) where

import Prelude (Unit, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBookerViewModel (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit)
import PUI (action, atCase, debounced, looped, prefixed, with)
import PUI.Web ((<+>), choice, inCaseAt, pane, text)
import PUI.Web.HTML (p)
import PUI.Web.Shoelace (body, button, select, textField, toast)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerShoelace :: Effect Unit
flightBookerShoelace =
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
    prefixed ( RecordToRecord.do
      p (text problemLine) # pane @"problem"
        @( problem :: { problem :: [ returnBeforeStart :: {}, unreadableReturn :: { input :: String }, unreadableStart :: { input :: String } ] }
         , "one-way" :: { out :: { y :: Int, m :: Int, d :: Int } }
         , "return" :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } }
         ) bookingState
      p (text oneWayLine) # pane @"one-way" bookingState
      p (text returnLine) # pane @"return" bookingState ) identity # debounced itinerarySettleTime
    button @"Book"
    ( VariantToRecord.do
      toast @"Flight booked" bookedLine
      toast @"Booking rejected" rejectedLine ) # action
        @( "Flight booked" :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
         , "Booking rejected" :: [ returnBeforeStart :: {}, unreadableReturn :: { input :: String }, unreadableStart :: { input :: String } ]
         ) submit # atCase @"Book"
