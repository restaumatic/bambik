module DeparturesMDC3 (departuresMDC3) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import DeparturesLogic (arrival, boardOpening, tick, tickPeriod, updatedFlight, updatedStatus)
import Effect (Effect)
import PUI (dispatched, displayed, every, forField, mvu, projected)
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
                  text # forField @"code" identity
                  staticText " — "
                  text # forField @"status" identity ) # displayed ) # dispatched arrival
              bodyMedium ( RecordToRecord.do
                  staticText "Last update: "
                  text # projected @"value" updatedFlight
                  staticText " → "
                  text # projected @"value" updatedStatus ) ) # displayed
      ) # mvu boardOpening
