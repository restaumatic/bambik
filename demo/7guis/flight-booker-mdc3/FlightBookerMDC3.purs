module FlightBookerMDC3 (flightBookerMDC3) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBookerViewModel (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit)
import PUI (action, atCase, debounced, mvu, state)
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
    ) # mvu plannedTrip
    ( Semigroupoid.do
      ( Semigroupoid.do
        state @"problem" @String
        bodyLarge (text problemLine) ) # shownWhen @"problem" bookingState
      ( Semigroupoid.do
        state @"out" @{ y :: Int, m :: Int, d :: Int }
        bodyLarge (text oneWayLine) ) # shownWhen @"one-way" bookingState
      ( Semigroupoid.do
        state @"out" @{ y :: Int, m :: Int, d :: Int }
        state @"back" @{ y :: Int, m :: Int, d :: Int }
        bodyLarge (text returnLine) ) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
    button @"Book" { icon: "flight_takeoff" }
    indeterminateLinearProgress @"Booking flight" # action @[ booked :: [ oneWayOn :: { y :: Int, m :: Int, d :: Int }, returnBetween :: { out :: { y :: Int, m :: Int, d :: Int }, back :: { y :: Int, m :: Int, d :: Int } } ], rejected :: String ] submit # atCase @"Book"
    VariantToRecord.do
      snackbar @"booked" bookedLine
      snackbar @"rejected" rejectedLine
