module FlightBookerMDC3 (flightBookerMDC3) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit)
import PUI (action, debounced, forCases, informed, mvu, required, updated)
import PUI.Web (choice)
import PUI.Web.HTML (shownCase, body, provided, staticText, text)
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerMDC3 :: Effect Unit
flightBookerMDC3 =
  body $
    elevation5 $
      card $ Semigroupoid.do
      ( Semigroupoid.do
          select @"Flight type" {}
            [ choice @"one-way", choice @"return" ] # required
          filledTextField @"Start date (DD.MM.YYYY)" {}
          filledTextField @"Return date (DD.MM.YYYY)" {} # provided returnLeg # updated (informed setReturn)
      ) # mvu plannedTrip
      ( Semigroupoid.do
          ( bodyLarge $ RecordToRecord.do
              staticText "⚠ "
              text @"problem" ) # shownCase @"problem" bookingState
          ( bodyLarge $ RecordToRecord.do
              staticText "A one-way flight on "
              text @"date" ) # shownCase @"one-way" bookingState
          ( bodyLarge $ RecordToRecord.do
              staticText "A return flight: out "
              text @"out"
              staticText ", back "
              text @"back" ) # shownCase @"return" bookingState ) # debounced itinerarySettleTime
      button @"Book" { icon: "flight_takeoff" }
      indeterminateLinearProgress @"busy" # action (match { "Book": submit })
      snackbar # forCases bookingLine
