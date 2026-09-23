module FlightBookerMDC2 (flightBookerMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit, tripType)
import PUI (action, atCase, debounced, forCases, mvu, required)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shownWhen, text)
import PUI.Web.MDC2 (body, body1, button, card, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerMDC2 :: Effect Unit
flightBookerMDC2 =
  body $
    card $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type" {}
        [ choice @"one-way", choice @"return" ] # required
      filledTextField @"Start date (DD.MM.YYYY)" {}
      filledTextField @"Return date (DD.MM.YYYY)" {} # inCase @"return" tripType
    ) # mvu plannedTrip
    ( Semigroupoid.do
      body1 (text problemLine) # shownWhen @"problem" bookingState
      body1 (text oneWayLine) # shownWhen @"one-way" bookingState
      body1 (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
    button @"Book" { icon: "flight_takeoff" }
    indeterminateLinearProgress @"busy" # action submit # atCase @"Book"
    snackbar # forCases bookingLine
