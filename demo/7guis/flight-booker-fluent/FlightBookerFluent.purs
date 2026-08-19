module FlightBookerFluent (flightBookerFluent) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit)
import PUI (action, completed, debounced, tapped, forCases, informed, mvu, pempty, required, updated)
import PUI.Web.Fluent (body1, button, card, dropdown, messageBar, textField)
import PUI.Web (choice)
import PUI.Web.HTML (providedCase, body, provided, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerFluent :: Effect Unit
flightBookerFluent =
  body $
    card $ Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              dropdown @"Flight type" {}
                [ choice @"one-way", choice @"return" ] # required
              textField @"Start date (DD.MM.YYYY)" {}) # completed
          textField @"Return date (DD.MM.YYYY)" {} # provided returnLeg # updated (informed setReturn)
      ) # mvu plannedTrip
      ( Semigroupoid.do
          body1 ( RecordToRecord.do
              staticText "⚠ "
              text @"problem" ) # providedCase @"problem" bookingState # tapped
          body1 ( RecordToRecord.do
              staticText "A one-way flight on "
              text @"date" ) # providedCase @"one-way" bookingState # tapped
          body1 ( RecordToRecord.do
              staticText "A return flight: out "
              text @"out"
              staticText ", back "
              text @"back" ) # providedCase @"return" bookingState # tapped ) # debounced itinerarySettleTime
      button @"Book" {}
      pempty # action (match { "Book": submit })
      messageBar # forCases bookingLine
