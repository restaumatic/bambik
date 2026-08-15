module FlightBookerMDC3 (flightBookerMDC3) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit)
import PUI (action, completed, debounced, displayed, forCases, informed, mvu, required, updated)
import PUI.Web (choice)
import Data.Tuple.Nested ((/\))
import PUI.Web.HTML (providedCase, body, provided, staticText, text)
import PUI.Web.MDC3 (bodyLarge, button, card, elevation5, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerMDC3 :: Effect Unit
flightBookerMDC3 =
  body $
    elevation5 $
      card { caption: "Book Flight" } $ Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              select @"Flight type" {}
                [ choice @"one-way flight", choice @"return flight" ] # required
              filledTextField @"Start date (DD.MM.YYYY)" {}) # completed
          filledTextField @"Return date (DD.MM.YYYY)" {} # provided returnLeg # updated (informed setReturn)
      ) # mvu plannedTrip
      ( Semigroupoid.do
          bodyLarge ( RecordToRecord.do
              staticText "⚠ "
              text @"problem" ) # providedCase @"problem" bookingState # displayed
          bodyLarge ( RecordToRecord.do
              staticText "A one-way flight on "
              text @"date" ) # providedCase @"one-way flight" bookingState # displayed
          bodyLarge ( RecordToRecord.do
              staticText "A return flight: out "
              text @"out"
              staticText ", back "
              text @"back" ) # providedCase @"return flight" bookingState # displayed ) # debounced itinerarySettleTime
      button @"Book" { icon: "flight_takeoff" }
      indeterminateLinearProgress @"busy" # action (match { "Book": submit })
      snackbar # forCases bookingLine
