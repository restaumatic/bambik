module FlightBookerShoelace (flightBookerShoelace) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import FlightBookerLogic (bookedLine, bookingState, itinerarySettleTime, oneWayLine, plannedTrip, problemLine, rejectedLine, returnLine, submit, tripType)
import PUI (action, atCase, debounced, mvu, blank)
import PUI.Web (choice, inCase, shownWhen, text)
import PUI.Web.HTML (p)
import PUI.Web.Shoelace (body, button, card, select, textField, toast)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBookerShoelace :: Effect Unit
flightBookerShoelace =
  body $
    card $ Semigroupoid.do
      ( Semigroupoid.do
        select @"Flight type" {}
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
      VariantToRecord.do
        toast @"booked" bookedLine
        toast @"rejected" rejectedLine
