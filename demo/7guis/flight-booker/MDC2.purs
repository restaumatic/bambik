module FlightBooker.MDC2 (flightBooker) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import FlightBooker (bookedLine, bookingState, plannedTrip, returnDateInvalid, startDateInvalid)
import PUI (looped, with)
import PUI.Web ((<+>), attrWith, choice, inCaseAt, provided, text)
import PUI.Web.MDC2 (acknowledged, body, body1, button', filledTextField', select)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type" (choice @"one-way flight" <+> choice @"return flight")
      filledTextField' @"Start date" { hint: "DD.MM.YYYY" } # attrWith "aria-invalid" startDateInvalid
      filledTextField' @"Return date" { hint: "DD.MM.YYYY" } # attrWith "aria-invalid" returnDateInvalid # inCaseAt @"return flight" @"Flight type"
    ) # looped # with plannedTrip
    button' @"Book" { icon: "flight_takeoff" } # provided @"bookable" bookingState
    acknowledged @"Book" "OK" $ body1 $ text bookedLine
