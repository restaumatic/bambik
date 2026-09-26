module FlightBookerBootstrap (flightBookerBootstrap) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit, tripType)
import PUI (action, atCase, debounced, forCases, mvu, required, blank)
import PUI.Web (choice, inCase, shownWhen, text)
import PUI.Web.Bootstrap (body, button, card, select, textField, toast)
import PUI.Web.HTML (p)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerBootstrap :: Effect Unit
flightBookerBootstrap =
  body $
    card $ Semigroupoid.do
      ( Semigroupoid.do
        select @"Flight type" {} required
          [ choice @"one-way", choice @"return" ]
        textField @"Start date (DD.MM.YYYY)" {}
        textField @"Return date (DD.MM.YYYY)" {} # inCase @"return" tripType
      ) # mvu plannedTrip
      ( Semigroupoid.do
        p (text problemLine) # shownWhen @"problem" bookingState
        p (text oneWayLine) # shownWhen @"one-way" bookingState
        p (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
      button @"Book" {}
      blank # action submit # atCase @"Book"
      toast # forCases bookingLine
