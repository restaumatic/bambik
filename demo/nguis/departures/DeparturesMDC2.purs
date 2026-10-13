module DeparturesMDC2 (departuresMDC2) where

import Prelude (Unit, (#), ($))

import DeparturesViewModel (arrival, flightLine, tickPeriod, updateLine)
import Effect (Effect)
import PUI (dispatched)
import PUI.Web (clock, text)
import PUI.Web.MDC2 (body, body2, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC2 :: Effect Unit
departuresMDC2 =
  body $ Semigroupoid.do
    clock @"time" tickPeriod
    list $ ( listItem $ text flightLine ) # dispatched @String @{ code :: String, status :: String } arrival
    body2 (text updateLine)
