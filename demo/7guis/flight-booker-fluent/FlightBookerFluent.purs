module FlightBookerFluent (flightBookerFluent) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit)
import PUI (action, asCase, asField, completed, debounced, displayed, forCases, forField, informed, mvu, pempty, required, updated)
import PUI.Web.Fluent (body1, button, card, dropdown, messageBar, textField)
import PUI.Web.HTML (providedCase, body, provided, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerFluent :: Effect Unit
flightBookerFluent =
  body $
    card { caption: "Book Flight" } $ Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              dropdown { label: "Flight type" }
                [ { value: .oneWay {}, label: "one-way flight" }
                , { value: .return {}, label: "return flight" }
                ] # required @"value" # asField @"value" @"flightType"
              textField { label: "Start date (DD.MM.YYYY)" } # asField @"value" @"start") # completed
          textField { label: "Return date (DD.MM.YYYY)" } # asField @"value" @"return" # provided returnLeg # updated (informed setReturn)
      ) # mvu plannedTrip
      ( Semigroupoid.do
          body1 ( RecordToRecord.do
              staticText "⚠ "
              text # forField @"problem" identity ) # providedCase @"problem" bookingState # displayed
          body1 ( RecordToRecord.do
              staticText "A one-way flight on "
              text # forField @"date" identity ) # providedCase @"oneWay" bookingState # displayed
          body1 ( RecordToRecord.do
              staticText "A return flight: out "
              text # forField @"out" identity
              staticText ", back "
              text # forField @"back" identity ) # providedCase @"return" bookingState # displayed ) # debounced itinerarySettleTime
      button { label: "Book" } # asCase @"clicked" @"book"
      pempty # action (match { book: submit })
      messageBar # forCases @"event" bookingLine
