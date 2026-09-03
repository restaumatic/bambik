module FlightBookerMDC2 (flightBookerMDC2) where

import Prelude (Unit, (#), ($))

import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit, tripType)
import PUI (action, debounced, forCases, mvu, required)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shownWhen, body, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Category as Category

flightBookerMDC2 :: Effect Unit
flightBookerMDC2 =
  body $
    elevation20 $
      card $ Category.do
      ( Category.do
          select @"Flight type" {}
            [ choice @"one-way", choice @"return" ] # required
          filledTextField @"Start date (DD.MM.YYYY)" {}
          filledTextField @"Return date (DD.MM.YYYY)" {} # inCase @"return" tripType
      ) # mvu plannedTrip
      ( Category.do
          body1 (text problemLine) # shownWhen @"problem" bookingState
          body1 (text oneWayLine) # shownWhen @"one-way" bookingState
          body1 (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
      button @"Book" { icon: "flight_takeoff" }
      indeterminateLinearProgress @"busy" # action (match { "Book": submit })
      snackbar # forCases bookingLine
