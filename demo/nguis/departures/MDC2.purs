module Departures.MDC2 (departures) where

import Prelude (Unit, (#), ($))

import Departures (arrival, clockPeriod, flightLine, updateLine)
import Effect (Effect)
import PUI (dispatchedOf, ticks)
import PUI.Web (text, timestamping)
import PUI.Web.MDC2 (body, body2, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departures :: Effect Unit
departures =
  body $ Semigroupoid.do
    ticks @"Clock ticked" clockPeriod # timestamping @"time"
    list $ ( listItem $ text flightLine ) # dispatchedOf @"code" arrival
    body2 $ text updateLine
