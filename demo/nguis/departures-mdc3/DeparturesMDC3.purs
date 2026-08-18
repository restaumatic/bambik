module DeparturesMDC3 (departuresMDC3) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import DeparturesLogic (arrival, boardOpening, tick, tickPeriod, updatedFlight, updatedStatus)
import Effect (Effect)
import PUI (dispatched, displayed, every, mvu, projected)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC3 (bodyMedium, card, elevation5, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC3 :: Effect Unit
departuresMDC3 =
  body $
    elevation5 $
      card { caption: "Departures" } $ ( Semigroupoid.do
          every tickPeriod tick
          ( Semigroupoid.do
              list ( ( listItem $ RecordToRecord.do
                  text @"code"
                  staticText " — "
                  text @"status" ) # displayed ) # dispatched arrival
              bodyMedium ( RecordToRecord.do
                  staticText "Last update: "
                  text @"updatedFlight" # projected updatedFlight
                  staticText " → "
                  text @"updatedStatus" # projected updatedStatus ) ) # displayed
      ) # mvu boardOpening
