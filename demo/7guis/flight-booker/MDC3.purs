module FlightBooker.MDC3 (flightBooker) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import FlightBooker (bookingLine, bookingState, plannedTrip, returnDateInvalid, startDateInvalid)
import PUI (looped, with)
import PUI.Web ((<+>), attrWith, choice, inCaseAt, provided, text)
import PUI.Web.MDC3 (body, bodyMedium, button', confirmed, filledTextField', select)
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
    confirmed @"Book" $ bodyMedium $ text bookingLine
