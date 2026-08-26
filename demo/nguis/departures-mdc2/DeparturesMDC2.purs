module DeparturesMDC2 (departuresMDC2) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import DeparturesLogic (arrival, boardOpening, tick, tickPeriod, updatedFlight, updatedStatus)
import Effect (Effect)
import PUI (dispatched, every, mvu, projected)
import PUI.Web.HTML (shownAlways, body, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, list, listItem)
import QualifiedDo.Semigroupoid as Pipeline

departuresMDC2 :: Effect Unit
departuresMDC2 =
  body $
    elevation20 $
      card $ ( Pipeline.do
          every tickPeriod tick
          ( Pipeline.do
              list ( ( listItem $ RecordToRecord.do
                  text @"code"
                  staticText " — "
                  text @"status" ) # shownAlways ) # dispatched arrival
              body2 ( RecordToRecord.do
                  staticText "Last update: "
                  text @"updatedFlight" # projected updatedFlight
                  staticText " → "
                  text @"updatedStatus" # projected updatedStatus ) ) # shownAlways
      ) # mvu boardOpening
