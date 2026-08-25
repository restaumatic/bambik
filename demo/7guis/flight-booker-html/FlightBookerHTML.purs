module FlightBookerHTML (flightBookerHTML) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit)
import PUI (action, debounced, field, forCases, informed, mvu, pempty, required, toCase, updated)
import PUI.Web (choice)
import PUI.Web.HTML (shownAs, shownCase, body, button, div, input, label, output, p, provided, select, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerHTML :: Effect Unit
flightBookerHTML =
  body $ div $ Semigroupoid.do
    ( Semigroupoid.do
        p ( label $ RecordToRecord.do
            staticText "Flight type "
            select @"Flight type"
              [ choice @"one-way", choice @"return" ] ) # required
        p ( label $ Semigroupoid.do
            (staticText "Start date (DD.MM.YYYY) ") # shownAs identity
            input "text" # field @"Start date (DD.MM.YYYY)" )
        p ( label $ Semigroupoid.do
            (staticText "Return date (DD.MM.YYYY) ") # shownAs identity
            input "text" # field @"Return date (DD.MM.YYYY)" ) # provided returnLeg # updated (informed setReturn)
    ) # mvu plannedTrip
    ( Semigroupoid.do
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
