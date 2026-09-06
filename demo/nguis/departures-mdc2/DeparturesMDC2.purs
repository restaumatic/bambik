module DeparturesMDC2 (departuresMDC2) where

import Prelude (Unit, (#), ($))

import DeparturesLogic (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (dispatched, every, mvu)
import PUI.Web.HTML (shown, text)
import PUI.Web.MDC2 (body, body2, card, list, listItem)
import QualifiedDo.Category as Category

departuresMDC2 :: Effect Unit
departuresMDC2 =
  body $
    card $ ( Category.do
      every tickPeriod tick
      ( Category.do
        list $ ( listItem $ text flightLine ) # shown # dispatched arrival
        body2 (text updateLine) ) # shown
    ) # mvu boardOpening
