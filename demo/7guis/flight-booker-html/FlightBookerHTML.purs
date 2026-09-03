module FlightBookerHTML (flightBookerHTML) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit, tripType)
import PUI (action, debounced, field, forCases, mvu, required, toCase, blank)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shown, shownWhen, body, button, div, input, label, output, p, select, staticText, text)
import QualifiedDo.Category as Category

flightBookerHTML :: Effect Unit
flightBookerHTML =
  body $ div $ Category.do
    ( Category.do
        p ( label $ RecordToRecord.do
            staticText "Flight type "
            select @"Flight type"
              [ choice @"one-way", choice @"return" ] ) # required
        p ( label $ Category.do
            (staticText "Start date (DD.MM.YYYY) ") # shown
            input "text" # field @"Start date (DD.MM.YYYY)" )
        p ( label $ Category.do
            (staticText "Return date (DD.MM.YYYY) ") # shown
            input "text" # field @"Return date (DD.MM.YYYY)" ) # inCase @"return" tripType
    ) # mvu plannedTrip
    ( Category.do
        p (text problemLine) # shownWhen @"problem" bookingState
        p (text oneWayLine) # shownWhen @"one-way" bookingState
        p (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
    button (staticText "Book") # toCase @"book" identity
    blank # action (match { book: submit })
    output # forCases bookingLine
