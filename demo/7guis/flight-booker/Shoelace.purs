module FlightBooker.Shoelace (flightBooker) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import FlightBooker (bookedLine, bookingState, plannedTrip, returnDateInvalid, startDateInvalid)
import PUI (looped, with)
import PUI.Web ((<+>), attrWith, choice, inCaseAt, provided, text)
import PUI.Web.HTML (p)
import PUI.Web.Shoelace (acknowledged, body, button, select, textField')
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type" (choice @"one-way flight" <+> choice @"return flight")
      textField' @"Start date" { hint: "DD.MM.YYYY" } # attrWith "aria-invalid" startDateInvalid
      textField' @"Return date" { hint: "DD.MM.YYYY" } # attrWith "aria-invalid" returnDateInvalid # inCaseAt @"return flight" @"Flight type"
    ) # looped # with plannedTrip
    button @"Book" # provided @"bookable" bookingState
    acknowledged @"Book" "OK" $ p $ text bookedLine
