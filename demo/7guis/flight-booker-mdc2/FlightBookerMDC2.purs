module FlightBookerMDC2 (flightBookerMDC2) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit)
import PUI (action, asCase, asField, completed, debounced, displayed, forCases, forField, informed, mvu, required, updated)
import PUI.Web.HTML (atCase, body, provided, staticText, text)
import PUI.Web.MDC2 (body1, button, card, elevation20, filledTextField, indeterminateLinearProgress, select, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerMDC2 :: Effect Unit
flightBookerMDC2 =
  body $
    elevation20 $
      card { caption: "Book Flight" } $ Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              select { floatingLabel: "Flight type" }
                [ { value: .oneWay {}, label: "one-way flight" }
                , { value: .return {}, label: "return flight" }
                ] # required # asField @"flightType"
              filledTextField { floatingLabel: "Start date (DD.MM.YYYY)" } # asField @"start") # completed
          filledTextField { floatingLabel: "Return date (DD.MM.YYYY)" } # asField @"return" # provided returnLeg # updated (informed setReturn)
      ) # mvu plannedTrip
      ( Semigroupoid.do
          body1 ( RecordToRecord.do
              staticText "⚠ "
              text # forField @"problem" identity ) # atCase @"problem" bookingState # displayed
          body1 ( RecordToRecord.do
              staticText "A one-way flight on "
              text # forField @"date" identity ) # atCase @"oneWay" bookingState # displayed
          body1 ( RecordToRecord.do
              staticText "A return flight: out "
              text # forField @"out" identity
              staticText ", back "
              text # forField @"back" identity ) # atCase @"return" bookingState # displayed ) # debounced itinerarySettleTime
      button { label: "Book", icon: "flight_takeoff" } # asCase @"book"
      indeterminateLinearProgress # action (match { book: submit })
      snackbar # forCases bookingLine
