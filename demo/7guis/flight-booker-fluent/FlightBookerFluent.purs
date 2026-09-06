module FlightBookerFluent (flightBookerFluent) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit, tripType)
import PUI (action, atCase, debounced, forCases, mvu, required, blank)
import PUI.Web (choice)
import PUI.Web.Fluent (body1, button, card, dropdown, messageBar, textField)
import PUI.Web.HTML (inCase, shownWhen, body, text)
import QualifiedDo.Category as Category

flightBookerFluent :: Effect Unit
flightBookerFluent =
  body $
    card $ Category.do
      ( Category.do
          dropdown @"Flight type" {}
            [ choice @"one-way", choice @"return" ] # required
          textField @"Start date (DD.MM.YYYY)" {}
          textField @"Return date (DD.MM.YYYY)" {} # inCase @"return" tripType
      ) # mvu plannedTrip
      ( Category.do
          body1 (text problemLine) # shownWhen @"problem" bookingState
          body1 (text oneWayLine) # shownWhen @"one-way" bookingState
          body1 (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
      button @"Book" {}
      blank # action submit # atCase @"Book"
      messageBar # forCases bookingLine
