module DeparturesMDC2 (departuresMDC2) where

import Prelude ((#), ($), Unit, identity)

import DeparturesViewModel (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (dispatched, fold, mvu, replaying, ticks)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body2, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC2 :: Effect Unit
departuresMDC2 =
  body $
    ( Semigroupoid.do
      ( Semigroupoid.do
        list $ ( listItem $ text flightLine ) # shown # dispatched @String @{ code :: String, status :: String } arrival
        body2 (text updateLine) ) # shown
      ticks @"tick" tickPeriod # replaying @"tick" identity
      fold @"tick" tick
    ) # mvu @( beat :: Int ) boardOpening
