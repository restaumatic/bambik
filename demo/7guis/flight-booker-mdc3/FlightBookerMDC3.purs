module FlightBookerMDC3 (flightBookerMDC3) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBookerLogic (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit, tripType)
import PUI (action, atCase, debounced, mvu)
import PUI.Web (choice, inCase, shownWhen, text)
import PUI.Web.MDC3 (body, bodyLarge, button, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerMDC3 :: Effect Unit
flightBookerMDC3 =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type" {}
        [ choice @"one-way", choice @"return" ]
      filledTextField @"Start date (DD.MM.YYYY)" {}
      filledTextField @"Return date (DD.MM.YYYY)" {} # inCase @"return" tripType
    ) # mvu plannedTrip
    ( Semigroupoid.do
      bodyLarge (text problemLine) # shownWhen @"problem" bookingState
      bodyLarge (text oneWayLine) # shownWhen @"one-way" bookingState
      bodyLarge (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
    button @"Book" { icon: "flight_takeoff" }
    indeterminateLinearProgress @"busy" # action submit # atCase @"Book"
    VariantToRecord.do
      snackbar @"booked" bookedLine
      snackbar @"rejected" rejectedLine
