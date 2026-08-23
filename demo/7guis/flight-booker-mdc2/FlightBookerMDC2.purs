module FlightBookerMDC2 (flightBookerMDC2) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit)
import PUI (action, completed, debounced, forCases, informed, mvu, required, updated)
import PUI.Web (choice)
import PUI.Web.HTML (shownCase, body, provided, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerMDC2 :: Effect Unit
flightBookerMDC2 =
  body $
    elevation20 $
      card $ Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              select @"Flight type" {}
                [ choice @"one-way", choice @"return" ] # required
              filledTextField @"Start date (DD.MM.YYYY)" {}) # completed
          filledTextField @"Return date (DD.MM.YYYY)" {} # provided returnLeg # updated (informed setReturn)
      ) # mvu plannedTrip
      ( Semigroupoid.do
          shownCase @"problem" bookingState ( body1 $ RecordToRecord.do
              staticText "⚠ "
              text @"problem" )
          shownCase @"one-way" bookingState ( body1 $ RecordToRecord.do
              staticText "A one-way flight on "
              text @"date" )
          shownCase @"return" bookingState ( body1 $ RecordToRecord.do
              staticText "A return flight: out "
              text @"out"
              staticText ", back "
              text @"back" ) ) # debounced itinerarySettleTime
      button @"Book" { icon: "flight_takeoff" }
      indeterminateLinearProgress @"busy" # action (match { "Book": submit })
      snackbar # forCases bookingLine
