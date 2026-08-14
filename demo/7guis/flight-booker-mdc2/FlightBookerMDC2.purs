module FlightBookerMDC2 (flightBookerMDC2) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit)
import PUI (action, asCase, completed, debounced, displayed, forCases, informed, mvu, required, updated)
import PUI.Web.HTML (providedCase, body, provided, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerMDC2 :: Effect Unit
flightBookerMDC2 =
  body $
    elevation20 $
      card { caption: "Book Flight" } $ Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              select @"flightType" {}
                [ { value: .oneWay {}, label: "one-way flight" }
                , { value: .return {}, label: "return flight" }
                ] # required
              filledTextField @"start" { floatingLabel: "Start date (DD.MM.YYYY)" }) # completed
          filledTextField @"return" { floatingLabel: "Return date (DD.MM.YYYY)" } # provided returnLeg # updated (informed setReturn)
      ) # mvu plannedTrip
      ( Semigroupoid.do
          body1 ( RecordToRecord.do
              staticText "⚠ "
              text @"problem" ) # providedCase @"problem" bookingState # displayed
          body1 ( RecordToRecord.do
              staticText "A one-way flight on "
              text @"date" ) # providedCase @"oneWay" bookingState # displayed
          body1 ( RecordToRecord.do
              staticText "A return flight: out "
              text @"out"
              staticText ", back "
              text @"back" ) # providedCase @"return" bookingState # displayed ) # debounced itinerarySettleTime
      button { label: "Book", icon: "flight_takeoff" } # asCase @"clicked" @"book"
      indeterminateLinearProgress @"busy" # action (match { book: submit })
      snackbar # forCases @"event" bookingLine
