module FlightBookerFluent (flightBookerFluent) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit)
import PUI (action, completed, debounced, displayed, forCases, informed, mvu, pempty, required, updated)
import PUI.Web.Fluent (body1, button, card, dropdown, messageBar, textField)
import PUI.Web.HTML (providedCase, body, provided, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerFluent :: Effect Unit
flightBookerFluent =
  body $
    card { caption: "Book Flight" } $ Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              dropdown @"Flight type" {}
                [ { value: .oneWay {}, label: "one-way flight" }
                , { value: .return {}, label: "return flight" }
                ] # required
              textField @"start" { label: "Start date (DD.MM.YYYY)" }) # completed
          textField @"return" { label: "Return date (DD.MM.YYYY)" } # provided returnLeg # updated (informed setReturn)
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
      button @"Book" {}
      pempty # action (match { "Book": submit })
      messageBar # forCases bookingLine
