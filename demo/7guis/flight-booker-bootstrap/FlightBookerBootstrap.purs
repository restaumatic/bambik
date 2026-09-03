module FlightBookerBootstrap (flightBookerBootstrap) where

import Prelude (Unit, (#), ($))

import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, returnLine, submit, tripType)
import PUI (action, debounced, forCases, mvu, required, blank)
import PUI.Web.Bootstrap (button, card, select, textField, toast)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shownWhen, body, p, text)
import QualifiedDo.Category as Category

flightBookerBootstrap :: Effect Unit
flightBookerBootstrap =
  body $
    card $ Category.do
      ( Category.do
          select @"Flight type" {}
            [ choice @"one-way", choice @"return" ] # required
          textField @"Start date (DD.MM.YYYY)" {}
          textField @"Return date (DD.MM.YYYY)" {} # inCase @"return" tripType
      ) # mvu plannedTrip
      ( Category.do
          p (text problemLine) # shownWhen @"problem" bookingState
          p (text oneWayLine) # shownWhen @"one-way" bookingState
          p (text returnLine) # shownWhen @"return" bookingState ) # debounced itinerarySettleTime
      button @"Book" {}
      blank # action (match { "Book": submit })
      toast # forCases bookingLine
