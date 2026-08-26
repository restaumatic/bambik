module DeparturesMDC3 (departuresMDC3) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import DeparturesLogic (arrival, boardOpening, tick, tickPeriod, updatedFlight, updatedStatus)
import Effect (Effect)
import PUI (dispatched, every, mvu, projected)
import PUI.Web.HTML (shown, body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, list, listItem)
import QualifiedDo.Semigroupoid as Pipeline

departuresMDC3 :: Effect Unit
departuresMDC3 =
  body $
    elevation5 $
      card $ ( Pipeline.do
          every tickPeriod tick
          ( Pipeline.do
              list ( ( listItem $ RecordToRecord.do
                  text @"code"
                  staticText " — "
                  text @"status" ) # shown ) # dispatched arrival
              bodyMedium ( RecordToRecord.do
                  staticText "Last update: "
                  text @"updatedFlight" # projected updatedFlight
                  staticText " → "
                  text @"updatedStatus" # projected updatedStatus ) ) # shown
      ) # mvu boardOpening
