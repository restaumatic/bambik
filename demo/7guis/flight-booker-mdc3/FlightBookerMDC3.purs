module FlightBookerMDC3 (flightBookerMDC3) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit)
import PUI (action, asCase, asField, completed, debounced, displayed, forCases, forField, informed, mvu, required, updated)
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
              select { floatingLabel: "Flight type" }
                [ { value: .oneWay {}, label: "one-way flight" }
                , { value: .return {}, label: "return flight" }
                ] # required @"value" # asField @"value" @"flightType"
              filledTextField { floatingLabel: "Start date (DD.MM.YYYY)" } # asField @"value" @"start") # completed
          filledTextField { floatingLabel: "Return date (DD.MM.YYYY)" } # asField @"value" @"return" # provided returnLeg # updated (informed setReturn)
      ) # mvu plannedTrip
      ( Semigroupoid.do
          bodyLarge ( RecordToRecord.do
              staticText "⚠ "
              text # forField @"value" @"problem" identity ) # providedCase @"problem" bookingState # displayed
          bodyLarge ( RecordToRecord.do
              staticText "A one-way flight on "
              text # forField @"value" @"date" identity ) # providedCase @"oneWay" bookingState # displayed
          bodyLarge ( RecordToRecord.do
              staticText "A return flight: out "
              text # forField @"value" @"out" identity
              staticText ", back "
              text # forField @"value" @"back" identity ) # providedCase @"return" bookingState # displayed ) # debounced itinerarySettleTime
      button { label: "Book", icon: "flight_takeoff" } # asCase @"clicked" @"book"
      indeterminateLinearProgress # action (match { book: submit })
      snackbar # forCases @"event" bookingLine
