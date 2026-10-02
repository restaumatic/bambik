module DeparturesMDC3 (departuresMDC3) where

import Prelude (Unit, (#), ($))

import DeparturesViewModel (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (dispatched, every, mvu, state)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyMedium, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC3 :: Effect Unit
departuresMDC3 =
  body $
    ( Semigroupoid.do
      state @"beat" @Int
      every tickPeriod tick
      ( Semigroupoid.do
        list $ ( Semigroupoid.do
          state @"code" @String
          state @"status" @String
          listItem $ text flightLine ) # shown # dispatched @String arrival
        bodyMedium (text updateLine) ) # shown
    ) # mvu boardOpening
