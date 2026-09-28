module DeparturesMDC3 (departuresMDC3) where

import Prelude (Unit, (#), ($))

import DeparturesLogic (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (dispatched, every, mvu)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyMedium, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC3 :: Effect Unit
departuresMDC3 =
  body $
    ( Semigroupoid.do
      every tickPeriod tick
      ( Semigroupoid.do
        list $ ( listItem $ text flightLine ) # shown # dispatched arrival
        bodyMedium (text updateLine) ) # shown
    ) # mvu boardOpening
