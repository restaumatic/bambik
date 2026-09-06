module DeparturesMDC3 (departuresMDC3) where

import Prelude (Unit, (#), ($))

import DeparturesLogic (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (dispatched, every, mvu)
import PUI.Web.HTML (shown, text)
import PUI.Web.MDC3 (body, bodyMedium, card, list, listItem)
import QualifiedDo.Category as Category

departuresMDC3 :: Effect Unit
departuresMDC3 =
  body $
    card $ ( Category.do
        every tickPeriod tick
        ( Category.do
            list $ ( listItem $ text flightLine ) # shown # dispatched arrival
            bodyMedium (text updateLine) ) # shown
    ) # mvu boardOpening
