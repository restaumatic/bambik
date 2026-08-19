module FlightBookerBootstrap (flightBookerBootstrap) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import FlightBookerLogic (bookingLine, bookingState, itinerarySettleTime, plannedTrip, returnLeg, setReturn, submit)
import PUI (action, completed, debounced, tapped, forCases, informed, mvu, pempty, required, updated)
import PUI.Web.Bootstrap (button, card, select, textField, toast)
import PUI.Web (choice)
import PUI.Web.HTML (providedCase, body, p, provided, staticText, text)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerBootstrap :: Effect Unit
flightBookerBootstrap =
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
          p ( RecordToRecord.do
              staticText "⚠ "
              text @"problem" ) # providedCase @"problem" bookingState # tapped
          p ( RecordToRecord.do
              staticText "A one-way flight on "
              text @"date" ) # providedCase @"one-way" bookingState # tapped
          p ( RecordToRecord.do
              staticText "A return flight: out "
              text @"out"
              staticText ", back "
              text @"back" ) # providedCase @"return" bookingState # tapped ) # debounced itinerarySettleTime
      button @"Book" {}
      pempty # action (match { "Book": submit })
      toast # forCases bookingLine
