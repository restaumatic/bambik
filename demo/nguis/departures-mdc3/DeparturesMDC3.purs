module DeparturesMDC3 (departuresMDC3) where

import Prelude ((#), ($), Unit, identity)

import DeparturesViewModel (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (dispatched, fold, looped, replaying, ticks, with)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyMedium, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC3 :: Effect Unit
departuresMDC3 =
  body $
    ( Semigroupoid.do
      ( Semigroupoid.do
        list $ ( listItem $ text flightLine ) # shown # dispatched @String @{ code :: String, status :: String } arrival
        bodyMedium (text updateLine) ) # shown
      ticks @"tick" tickPeriod # replaying @"tick" identity
      fold @"tick" tick
    ) # looped @( beat :: Int ) # with boardOpening
