module FlightBookerHTML (flightBookerHTML) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit, tripType)
import PUI (action, atCase, debounced, forCases, mvu, required, blank)
import PUI.Web (choice, inCase, shown, shownWhen, staticText, text)
import PUI.Web.HTML (body, button, div, input, label, output, p, select)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerHTML :: Effect Unit
flightBookerHTML =
  body $ div $ Semigroupoid.do
    ( Semigroupoid.do
      p ( label $ Semigroupoid.do
        (staticText "Flight type ") # shown
        select @"Flight type" required [ choice @"one-way", choice @"return" ] )
      p ( label $ Semigroupoid.do
        (staticText "Start date (DD.MM.YYYY) ") # shown
        input @"Start date (DD.MM.YYYY)" "text" )
      p ( label $ Semigroupoid.do
        (staticText "Return date (DD.MM.YYYY) ") # shown
        input @"Return date (DD.MM.YYYY)" "text" ) # inCase @"return" tripType
    ) # mvu plannedTrip
    ( Semigroupoid.do
      p (text problemLine) # shownWhen @"problem" bookingState
      p (text oneWayLine) # shownWhen @"one-way" bookingState
      p (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
    button @"Book" (staticText "Book")
    blank # action submit # atCase @"Book"
    output # forCases bookingLine
