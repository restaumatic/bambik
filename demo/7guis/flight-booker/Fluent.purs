module FlightBooker.Fluent (flightBooker) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import FlightBooker (bookedLine, bookingState, plannedTrip, returnDateInvalid, startDateInvalid)
import PUI (looped, with)
import PUI.Web ((<+>), attrWith, choice, inCaseAt, provided, text)
import PUI.Web.Fluent (acknowledged, body, body1, button, dropdown, textField')
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      dropdown @"Flight type" (choice @"one-way flight" <+> choice @"return flight")
      textField' @"Start date" { hint: "DD.MM.YYYY" } # attrWith "aria-invalid" startDateInvalid
      textField' @"Return date" { hint: "DD.MM.YYYY" } # attrWith "aria-invalid" returnDateInvalid # inCaseAt @"return flight" @"Flight type"
    ) # looped # with plannedTrip
    button @"Book" # provided @"bookable" bookingState
    acknowledged @"Book" "OK" $ body1 $ text bookedLine
