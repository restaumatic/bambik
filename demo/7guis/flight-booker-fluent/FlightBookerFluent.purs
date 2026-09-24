module FlightBookerFluent (flightBookerFluent) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit, tripType)
import PUI (action, atCase, debounced, forCases, mvu, required, blank)
import PUI.Web (choice, inCase, shownWhen, text)
import PUI.Web.Fluent (body, body1, button, card, dropdown, messageBar, textField)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerFluent :: Effect Unit
flightBookerFluent =
  body $
    card $ Semigroupoid.do
      ( Semigroupoid.do
        dropdown @"Flight type" {}
          [ choice @"one-way", choice @"return" ] # required
        textField @"Start date (DD.MM.YYYY)" {}
        textField @"Return date (DD.MM.YYYY)" {} # inCase @"return" tripType
      ) # mvu plannedTrip
      ( Semigroupoid.do
        body1 (text problemLine) # shownWhen @"problem" bookingState
        body1 (text oneWayLine) # shownWhen @"one-way" bookingState
        body1 (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
      button @"Book" {}
      blank # action submit # atCase @"Book"
      messageBar # forCases bookingLine
