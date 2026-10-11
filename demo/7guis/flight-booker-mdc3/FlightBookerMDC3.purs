module FlightBookerMDC3 (flightBookerMDC3) where

import Prelude (Unit, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBookerViewModel (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit)
import PUI (action, atCase, debounced, looped, prefixed, with)
import PUI.Web ((<+>), choice, inCaseAt, pane, text)
import PUI.Web.MDC3 (body, bodyLarge, button, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerMDC3 :: Effect Unit
flightBookerMDC3 =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type" {}
        (choice @"one-way" <+> choice @"return")
      filledTextField @"Start date (DD.MM.YYYY)" {}
      filledTextField @"Return date (DD.MM.YYYY)" {} # inCaseAt @"return" @"Flight type"
    ) # looped
      @( "Flight type" :: [ "one-way" :: {}, "return" :: {} ]
       , "Start date (DD.MM.YYYY)" :: String
       , "Return date (DD.MM.YYYY)" :: String
       ) # with plannedTrip
    prefixed ( RecordToRecord.do
      bodyLarge (text problemLine) # pane @"problem"
        @( problem :: { problem :: [ returnBeforeStart :: {}, unreadableReturn :: { input :: String }, unreadableStart :: { input :: String } ] }
         , "one-way" :: { out :: { y :: Int, m :: Int, d :: Int } }
         , "return" :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } }
         ) bookingState
      bodyLarge (text oneWayLine) # pane @"one-way" bookingState
      bodyLarge (text returnLine) # pane @"return" bookingState ) identity # debounced itinerarySettleTime
    button @"Book" { icon: "flight_takeoff" }
    ( VariantToRecord.do
      indeterminateLinearProgress
      snackbar @"Flight booked" bookedLine
      snackbar @"Booking rejected" rejectedLine ) # action
        @( "Flight booked" :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ]
         , "Booking rejected" :: [ returnBeforeStart :: {}, unreadableReturn :: { input :: String }, unreadableStart :: { input :: String } ]
         ) submit # atCase @"Book"
