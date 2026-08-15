module FlightBookerHTML (flightBookerHTML) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit)
import PUI (action, completed, debounced, displayed, field, forCases, informed, mvu, pempty, required, toCase, updated)
import PUI.Web.HTML (providedCase, body, button, div, input, label, output, p, provided, select, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerHTML :: Effect Unit
flightBookerHTML =
  body $ div $ Semigroupoid.do
    ( Semigroupoid.do
        ( RecordToRecord.do
            p ( label $ RecordToRecord.do
                staticText "Flight type "
                select @"Flight type"
                  [ { value: .oneWay {}, label: "one-way flight" }
                  , { value: .roundTrip {}, label: "return flight" }
                  ] ) # required
            p ( label $ RecordToRecord.do
                staticText "Start date (DD.MM.YYYY) "
                input "text" # field @"Start date (DD.MM.YYYY)" )) # completed
        p ( label $ RecordToRecord.do
            staticText "Return date (DD.MM.YYYY) "
            input "text" # field @"Return date (DD.MM.YYYY)" ) # provided returnLeg # updated (informed setReturn)
    ) # mvu plannedTrip
    ( Semigroupoid.do
        p ( RecordToRecord.do
            staticText "⚠ "
            text @"problem" ) # providedCase @"problem" bookingState # displayed
        p ( RecordToRecord.do
            staticText "A one-way flight on "
            text @"date" ) # providedCase @"oneWay" bookingState # displayed
        p ( RecordToRecord.do
            staticText "A return flight: out "
            text @"out"
            staticText ", back "
            text @"back" ) # providedCase @"roundTrip" bookingState # displayed ) # debounced itinerarySettleTime
    button (staticText "Book") # toCase @"book" identity
    pempty # action (match { book: submit })
    output # forCases bookingLine
