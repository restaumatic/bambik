module DeparturesMDC2 (departuresMDC2) where

import Prelude (Unit, (#), ($))

import DeparturesLogic (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (dispatched, every, mvu, muted)
import PUI.Web.HTML (shown, body, text)
import PUI.Web.MDC2 (body2, card, elevation20, list, listItem)
import QualifiedDo.Category as Category

departuresMDC2 :: Effect Unit
departuresMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          every tickPeriod tick
          ( Category.do
              list ( ( listItem $ text flightLine ) # shown ) # dispatched arrival
              body2 (text updateLine) # shown # muted ) # shown
      ) # mvu boardOpening
