module FlightBookerMDC3 (flightBookerMDC3) where

import Prelude (Unit, (#), ($))

import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, submit, tripType)
import PUI (action, debounced, forCases, mvu, required)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shownWhen, body, text)
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Category as Category

flightBookerMDC3 :: Effect Unit
flightBookerMDC3 =
  body $
    elevation5 $
      card $ Category.do
      ( Category.do
          select @"Flight type" {}
            [ choice @"one-way", choice @"return" ] # required
          filledTextField @"Start date (DD.MM.YYYY)" {}
          filledTextField @"Return date (DD.MM.YYYY)" {} # inCase @"return" tripType
      ) # mvu plannedTrip
      ( Category.do
          bodyLarge (text @"problemLine") # shownWhen @"problem" bookingState
          bodyLarge (text @"oneWayLine") # shownWhen @"one-way" bookingState
          bodyLarge (text @"returnLine") # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
      button @"Book" { icon: "flight_takeoff" }
      indeterminateLinearProgress @"busy" # action (match { "Book": submit })
      snackbar # forCases bookingLine
