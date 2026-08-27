module DeparturesMDC2 (departuresMDC2) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import DeparturesLogic (arrival, boardOpening, tick, tickPeriod)
import Effect (Effect)
import PUI (atField, dispatched, every, forProperty, mvu)
import PUI.Web.HTML (shown, body, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, list, listItem)
import QualifiedDo.Category as Category

departuresMDC2 :: Effect Unit
departuresMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          every tickPeriod tick
          ( Category.do
              list ( ( listItem $ RecordToRecord.do
                  text @"code"
                  staticText " — "
                  text @"status" ) # shown ) # dispatched arrival
              body2 ( RecordToRecord.do
                  staticText "Last update: "
                  text @"code"
                  staticText " → "
                  text @"status" ) # atField @"value" # forProperty ) # shown
      ) # mvu boardOpening
