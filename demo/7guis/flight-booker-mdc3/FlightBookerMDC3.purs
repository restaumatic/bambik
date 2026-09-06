module FlightBookerMDC3 (flightBookerMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit, tripType)
import PUI (action, atCase, debounced, forCases, mvu, required)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shownWhen, text)
import PUI.Web.MDC3 (body, bodyLarge, button, card, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Category as Category

flightBookerMDC3 :: Effect Unit
flightBookerMDC3 =
  body $
    card $ Category.do
    ( Category.do
        select @"Flight type" {}
          [ choice @"one-way", choice @"return" ] # required
        filledTextField @"Start date (DD.MM.YYYY)" {}
        filledTextField @"Return date (DD.MM.YYYY)" {} # inCase @"return" tripType
    ) # mvu plannedTrip
    ( Category.do
        bodyLarge (text problemLine) # shownWhen @"problem" bookingState
        bodyLarge (text oneWayLine) # shownWhen @"one-way" bookingState
        bodyLarge (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
    button @"Book" { icon: "flight_takeoff" }
    indeterminateLinearProgress @"busy" # action submit # atCase @"Book"
    snackbar # forCases bookingLine
