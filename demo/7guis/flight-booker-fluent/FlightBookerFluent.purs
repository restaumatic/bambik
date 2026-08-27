module FlightBookerFluent (flightBookerFluent) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, submit, tripType)
import PUI (action, debounced, forCases, mvu, required, blank)
import PUI.Web.Fluent (body1, button, card, dropdown, messageBar, textField)
import PUI.Web (choice)
import PUI.Web.HTML (inCase, shownCase, body, staticText, text)
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
          ( body1 $ RecordToRecord.do
              staticText "⚠ "
              text @"problem" ) # shownCase @"problem" bookingState
          ( body1 $ RecordToRecord.do
              staticText "A one-way flight on "
              text @"date" ) # shownCase @"one-way" bookingState
          ( body1 $ RecordToRecord.do
              staticText "A return flight: out "
              text @"out"
              staticText ", back "
              text @"back" ) # shownCase @"return" bookingState ) # debounced itinerarySettleTime
      button @"Book" {}
      blank # action (match { "Book": submit })
      messageBar # forCases bookingLine
