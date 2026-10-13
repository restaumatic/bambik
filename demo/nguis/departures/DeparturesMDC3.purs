module DeparturesMDC3 (departuresMDC3) where

import Prelude (Unit, (#), ($))

import DeparturesViewModel (arrival, flightLine, tickPeriod, updateLine)
import Effect (Effect)
import PUI (dispatched)
import PUI.Web (clock, text)
import PUI.Web.MDC3 (body, bodyMedium, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC3 :: Effect Unit
departuresMDC3 =
  body $ Semigroupoid.do
    clock @"time" tickPeriod
    list $ ( listItem $ text flightLine ) # dispatched @String @{ code :: String, status :: String } arrival
    bodyMedium (text updateLine)
