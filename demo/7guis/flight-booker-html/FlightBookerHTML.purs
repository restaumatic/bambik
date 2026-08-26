module FlightBookerHTML (flightBookerHTML) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, submit, tripType)
import PUI (action, debounced, field, forCases, mvu, pempty, required, toCase)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shownAlways, shownCase, body, button, div, input, label, output, p, select, staticText, text)
import QualifiedDo.Semigroupoid as Pipeline

flightBookerHTML :: Effect Unit
flightBookerHTML =
  body $ div $ Pipeline.do
    ( Pipeline.do
        p ( label $ RecordToRecord.do
            staticText "Flight type "
            select @"Flight type"
              [ choice @"one-way", choice @"return" ] ) # required
        p ( label $ Pipeline.do
            (staticText "Start date (DD.MM.YYYY) ") # shownAlways
            input "text" # field @"Start date (DD.MM.YYYY)" )
        p ( label $ Pipeline.do
            (staticText "Return date (DD.MM.YYYY) ") # shownAlways
            input "text" # field @"Return date (DD.MM.YYYY)" ) # inCase @"return" tripType
    ) # mvu plannedTrip
    ( Pipeline.do
        ( p $ RecordToRecord.do
            staticText "⚠ "
            text @"problem" ) # shownCase @"problem" bookingState
        ( p $ RecordToRecord.do
            staticText "A one-way flight on "
            text @"date" ) # shownCase @"one-way" bookingState
        ( p $ RecordToRecord.do
            staticText "A return flight: out "
            text @"out"
            staticText ", back "
            text @"back" ) # shownCase @"return" bookingState ) # debounced itinerarySettleTime
    button (staticText "Book") # toCase @"book" identity
    pempty # action (match { book: submit })
    output # forCases bookingLine
