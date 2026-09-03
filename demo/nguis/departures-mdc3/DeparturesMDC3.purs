module DeparturesMDC3 (departuresMDC3) where

import Prelude (Unit, (#), ($))

import DeparturesLogic (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (dispatched, every, mvu, muted)
import PUI.Web.HTML (shown, body, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, list, listItem)
import QualifiedDo.Category as Category

departuresMDC3 :: Effect Unit
departuresMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          every tickPeriod tick
          ( Category.do
              list ( ( listItem $ text flightLine ) # shown ) # dispatched arrival
              bodyMedium (text updateLine) # shown # muted ) # shown
      ) # mvu boardOpening
