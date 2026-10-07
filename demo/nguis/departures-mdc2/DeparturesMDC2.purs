module DeparturesMDC2 (departuresMDC2) where

import Prelude ((#), ($), Unit)

import DeparturesViewModel (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (blankStatus, dispatched, fold, looped, ticks, with)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body2, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC2 :: Effect Unit
departuresMDC2 =
  body $
    Semigroupoid.do
      ( Semigroupoid.do
        list $ ( listItem $ text flightLine ) # shown # dispatched @String @{ code :: String, status :: String } arrival
        body2 (text updateLine) ) # shown
      blankStatus @"Board refreshed" # ticks tickPeriod
      blankStatus @"Board refreshed" # fold tick
    # looped @( beat :: Int ) # with boardOpening
