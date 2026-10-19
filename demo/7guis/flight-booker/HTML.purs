module FlightBooker.HTML (flightBooker) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import FlightBooker (bookingLine, bookingState, plannedTrip, returnDateInvalid, startDateInvalid)
import PUI (looped, with)
import PUI.Web (attrWith, inCaseAt, provided, staticText, text)
import PUI.Web.HTML (body, button, confirmed, div, input, label, option, p, select)
import QualifiedDo.Semigroupoid as Semigroupoid

flightBooker :: Effect Unit
flightBooker =
  body $ div $ Semigroupoid.do
    ( Semigroupoid.do
      p $ label $ Semigroupoid.do
        staticText "Flight type "
        select @"Flight type" $ VariantToVariant.do
          option @"one-way flight"
          option @"return flight"
      p $ label $ Semigroupoid.do
        staticText "Start date (DD.MM.YYYY) "
        input @"Start date" "text" # attrWith "aria-invalid" startDateInvalid
      ( p $ label $ Semigroupoid.do
        staticText "Return date (DD.MM.YYYY) "
        input @"Return date" "text" # attrWith "aria-invalid" returnDateInvalid ) # inCaseAt @"return flight" @"Flight type"
    ) # looped # with plannedTrip
    button @"Book" # provided @"bookable" bookingState
    confirmed @"Book" $ p $ text bookingLine
