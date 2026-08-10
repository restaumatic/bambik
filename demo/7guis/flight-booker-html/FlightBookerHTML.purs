module FlightBookerHTML (flightBookerHTML) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit)
import PUI (action, asField, completed, debounced, displayed, field, forCases, forField, informed, mvu, required, silence, toCase, updated)
import PUI.Web.HTML (providedCase, body, button, div, input, label, output, p, provided, select, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerHTML :: Effect Unit
flightBookerHTML =
  body $ div $ Semigroupoid.do
    ( Semigroupoid.do
        ( RecordToRecord.do
            p ( label $ RecordToRecord.do
                staticText "Flight type "
                select
                  [ { value: .oneWay {}, label: "one-way flight" }
                  , { value: .return {}, label: "return flight" }
                  ] ) # required @"value" # asField @"value" @"flightType"
            p ( label $ RecordToRecord.do
                staticText "Start date (DD.MM.YYYY) "
                input "text" # field @"value" ) # asField @"value" @"start") # completed
        p ( label $ RecordToRecord.do
            staticText "Return date (DD.MM.YYYY) "
            input "text" # field @"value" ) # asField @"value" @"return" # provided returnLeg # updated (informed setReturn)
    ) # mvu plannedTrip
    ( Semigroupoid.do
        p ( RecordToRecord.do
            staticText "⚠ "
            text # forField @"value" @"problem" identity ) # providedCase @"problem" bookingState # displayed
        p ( RecordToRecord.do
            staticText "A one-way flight on "
            text # forField @"value" @"date" identity ) # providedCase @"oneWay" bookingState # displayed
        p ( RecordToRecord.do
            staticText "A return flight: out "
            text # forField @"value" @"out" identity
            staticText ", back "
            text # forField @"value" @"back" identity ) # providedCase @"return" bookingState # displayed ) # debounced itinerarySettleTime
    button (staticText "Book") # toCase @"book" identity
    silence # action (match { book: submit })
    output # forCases @"event" bookingLine
