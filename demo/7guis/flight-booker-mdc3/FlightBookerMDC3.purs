module FlightBookerMDC3 (flightBookerMDC3) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBookerViewModel (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit)
import PUI (action, atCase, debounced, looped, with)
import PUI.Web ((<+>), choice, inCase, shownWhen, text)
import PUI.Web.MDC3 (body, bodyLarge, button, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerMDC3 :: Effect Unit
flightBookerMDC3 =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type" {}
        (choice @"one-way" <+> choice @"return")
      filledTextField @"Start date (DD.MM.YYYY)" {}
      filledTextField @"Return date (DD.MM.YYYY)" {} # inCase @"return" _."Flight type"
    ) # looped
      @( "Flight type" :: [ "one-way" :: {}, "return" :: {} ]
       , "Start date (DD.MM.YYYY)" :: String
       , "Return date (DD.MM.YYYY)" :: String
       ) # with plannedTrip
    ( Semigroupoid.do
      bodyLarge (text problemLine) # shownWhen @"problem" @( problem :: { problem :: String }, "one-way" :: { out :: { y :: Int, m :: Int, d :: Int } }, "return" :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ) bookingState
      bodyLarge (text oneWayLine) # shownWhen @"one-way" bookingState
      bodyLarge (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
    button @"Book" { icon: "flight_takeoff" }
    indeterminateLinearProgress @"Booking flight" # action @[ booked :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ], rejected :: String ] submit # atCase @"Book"
    VariantToRecord.do
      snackbar @"booked" bookedLine
      snackbar @"rejected" rejectedLine
