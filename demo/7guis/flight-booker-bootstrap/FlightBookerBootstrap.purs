module FlightBookerBootstrap (flightBookerBootstrap) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit)
import PUI (action, asCase, asField, completed, debounced, displayed, forCases, forField, informed, mvu, required, silence, updated)
import PUI.Web.Bootstrap (button, card, select, textField, toast)
import PUI.Web.HTML (atCase, body, p, provided, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerBootstrap :: Effect Unit
flightBookerBootstrap =
  body $
    card { caption: "Book Flight" } $ Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              select { label: "Flight type" }
                [ { value: .oneWay {}, label: "one-way flight" }
                , { value: .return {}, label: "return flight" }
                ] # required # asField @"flightType"
              textField { label: "Start date (DD.MM.YYYY)" } # asField @"start") # completed
          textField { label: "Return date (DD.MM.YYYY)" } # asField @"return" # provided returnLeg # updated (informed setReturn)
      ) # mvu plannedTrip
      ( Semigroupoid.do
          p ( RecordToRecord.do
              staticText "⚠ "
              text # forField @"problem" identity ) # atCase @"problem" bookingState # displayed
          p ( RecordToRecord.do
              staticText "A one-way flight on "
              text # forField @"date" identity ) # atCase @"oneWay" bookingState # displayed
          p ( RecordToRecord.do
              staticText "A return flight: out "
              text # forField @"out" identity
              staticText ", back "
              text # forField @"back" identity ) # atCase @"return" bookingState # displayed ) # debounced itinerarySettleTime
      button { label: "Book" } # asCase @"book"
      silence # action (match { book: submit })
      toast # forCases bookingLine
