module DeparturesMDC2 (departuresMDC2) where

import Prelude (Unit, (#), ($))

import DeparturesViewModel (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (dispatched, every, mvu, state)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body2, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC2 :: Effect Unit
departuresMDC2 =
  body $
    ( Semigroupoid.do
      state @"beat" @Int
      every tickPeriod tick
      ( Semigroupoid.do
        list $ ( Semigroupoid.do
          state @"code" @String
          state @"status" @String
          listItem $ text flightLine ) # shown # dispatched @String arrival
        body2 (text updateLine) ) # shown
    ) # mvu boardOpening
