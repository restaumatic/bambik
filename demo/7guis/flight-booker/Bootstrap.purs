module FlightBooker.Bootstrap (flightBooker) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import FlightBooker (bookingLine, bookingState, plannedTrip, returnDateInvalid, startDateInvalid)
import PUI (looped, with)
import PUI.Web (attrWith, inCaseAt, provided, text)
import PUI.Web.Bootstrap (body, button, option, select, textField')
import PUI.Web.HTML (confirmed, p)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      select @"Flight type" $ VariantToVariant.do
        option @"one-way flight"
        option @"return flight"
      textField' @"Start date" { hint: "DD.MM.YYYY" } # attrWith "aria-invalid" startDateInvalid
      textField' @"Return date" { hint: "DD.MM.YYYY" } # attrWith "aria-invalid" returnDateInvalid # inCaseAt @"return flight" @"Flight type"
    ) # looped # with plannedTrip
    button @"Book" # provided @"bookable" bookingState
    confirmed @"Book" $ p $ text bookingLine
