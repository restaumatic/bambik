module Departures.MDC2 (departures) where

import Prelude (Unit, (#), ($))

import Departures (arrival, clockPeriod, flightLine, updateLine)
import Effect (Effect)
import PUI (dispatched)
import PUI.Web (clock, text)
import PUI.Web.MDC2 (body, body2, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departures :: Effect Unit
departures =
  body $ Semigroupoid.do
    clock @"time" clockPeriod
    list $ ( listItem $ text flightLine ) # dispatched arrival
    body2 $ text updateLine
