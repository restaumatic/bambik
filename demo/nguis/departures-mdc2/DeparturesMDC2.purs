module DeparturesMDC2 (departuresMDC2) where

import Prelude (identity, (#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import DeparturesLogic (arrival, boardOpening, tick, tickPeriod, updatedFlight, updatedStatus)
import Effect (Effect)
import PUI (dispatched, displayed, every, forField, mvu, projected)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC2 (body2, card, elevation20, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC2 :: Effect Unit
departuresMDC2 =
  body $
    elevation20 $
      card { caption: "Departures" } $ ( Semigroupoid.do
          every tickPeriod tick
          ( Semigroupoid.do
              list ( ( listItem $ RecordToRecord.do
                  text # forField @"code" identity
                  staticText " — "
                  text # forField @"status" identity ) # displayed ) # dispatched arrival
              body2 ( RecordToRecord.do
                  staticText "Last update: "
                  text # projected updatedFlight
                  staticText " → "
                  text # projected updatedStatus ) ) # displayed
      ) # mvu boardOpening
