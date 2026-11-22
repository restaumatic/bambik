module Departures.MDC3 (departures) where

import Prelude (Unit, (#), ($))

import Departures (arrival, clockPeriod, flightLine, updateLine)
import Effect (Effect)
import PUI (dispatchedOf, ticks)
import PUI.Web (text, timestamping)
import PUI.Web.MDC3 (body, bodyMedium, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departures :: Effect Unit
departures =
  body $ Semigroupoid.do
    ticks @"Clock ticked" clockPeriod # timestamping @"time"
    list $ ( listItem $ text flightLine ) # dispatchedOf @"code" arrival
    bodyMedium $ text updateLine
