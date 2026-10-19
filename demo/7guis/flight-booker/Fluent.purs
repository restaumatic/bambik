module FlightBooker.Fluent (flightBooker) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import FlightBooker (bookingLine, bookingState, plannedTrip, returnDateInvalid, startDateInvalid)
import PUI (looped, with)
import PUI.Web (attrWith, inCaseAt, provided, text)
import PUI.Web.Fluent (body, body1, button, confirmed, dropdown, option, textField')
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      dropdown @"Flight type" $ VariantToVariant.do
        option @"one-way flight"
        option @"return flight"
      textField' @"Start date" { hint: "DD.MM.YYYY" } # attrWith "aria-invalid" startDateInvalid
      textField' @"Return date" { hint: "DD.MM.YYYY" } # attrWith "aria-invalid" returnDateInvalid # inCaseAt @"return flight" @"Flight type"
    ) # looped # with plannedTrip
    button @"Book" # provided @"bookable" bookingState
    confirmed @"Book" $ body1 $ text bookingLine
