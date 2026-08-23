module FlightBookerShoelace (flightBookerShoelace) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit)
import PUI (action, completed, debounced, forCases, informed, mvu, pempty, required, updated)
import PUI.Web (choice)
import PUI.Web.HTML (shownCase, body, p, provided, staticText, text)
import PUI.Web.Shoelace (button, card, select, textField, toast)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerShoelace :: Effect Unit
flightBookerShoelace =
  body $
    card $ Semigroupoid.do
      ( Semigroupoid.do
          ( RecordToRecord.do
              select @"Flight type" {}
                [ choice @"one-way", choice @"return" ] # required
              textField @"Start date (DD.MM.YYYY)" {}) # completed
          textField @"Return date (DD.MM.YYYY)" {} # provided returnLeg # updated (informed setReturn)
      ) # mvu plannedTrip
      ( Semigroupoid.do
          shownCase @"problem" bookingState ( p $ RecordToRecord.do
              staticText "⚠ "
              text @"problem" )
          shownCase @"one-way" bookingState ( p $ RecordToRecord.do
              staticText "A one-way flight on "
              text @"date" )
          shownCase @"return" bookingState ( p $ RecordToRecord.do
              staticText "A return flight: out "
              text @"out"
              staticText ", back "
              text @"back" ) ) # debounced itinerarySettleTime
      button @"Book" {}
      pempty # action (match { "Book": submit })
      toast # forCases bookingLine
