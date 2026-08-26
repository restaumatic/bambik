module FlightBookerMDC2 (flightBookerMDC2) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, submit, tripType)
import PUI (action, debounced, forCases, mvu, required)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shownCase, body, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Pipeline

flightBookerMDC2 :: Effect Unit
flightBookerMDC2 =
  body $
    elevation20 $
      card $ Pipeline.do
      ( Pipeline.do
          select @"Flight type" {}
            [ choice @"one-way", choice @"return" ] # required
          filledTextField @"Start date (DD.MM.YYYY)" {}
          filledTextField @"Return date (DD.MM.YYYY)" {} # inCase @"return" tripType
      ) # mvu plannedTrip
      ( Pipeline.do
          ( body1 $ RecordToRecord.do
              staticText "⚠ "
              text @"problem" ) # shownCase @"problem" bookingState
          ( body1 $ RecordToRecord.do
              staticText "A one-way flight on "
              text @"date" ) # shownCase @"one-way" bookingState
          ( body1 $ RecordToRecord.do
              staticText "A return flight: out "
              text @"out"
              staticText ", back "
              text @"back" ) # shownCase @"return" bookingState ) # debounced itinerarySettleTime
      button @"Book" { icon: "flight_takeoff" }
      indeterminateLinearProgress @"busy" # action (match { "Book": submit })
      snackbar # forCases bookingLine
