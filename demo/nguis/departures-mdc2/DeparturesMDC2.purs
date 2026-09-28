module DeparturesMDC2 (departuresMDC2) where

import Prelude (Unit, (#), ($))

import DeparturesLogic (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (dispatched, every, mvu)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body2, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC2 :: Effect Unit
departuresMDC2 =
  body $
    ( Semigroupoid.do
      every tickPeriod tick
      ( Semigroupoid.do
        list $ ( listItem $ text flightLine ) # shown # dispatched arrival
        body2 (text updateLine) ) # shown
    ) # mvu boardOpening
