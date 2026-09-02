module DeparturesMDC2 (departuresMDC2) where

import Prelude (Unit, (#), ($))

import DeparturesLogic (arrival, boardOpening, tick, tickPeriod)
import Effect (Effect)
import PUI (atField, dispatched, every, forProperty, mvu)
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
              list ( ( listItem $ text @"flightLine" ) # shown ) # dispatched arrival
              body2 (text @"updateLine" # forProperty) # atField @"value" # forProperty ) # shown
      ) # mvu boardOpening
