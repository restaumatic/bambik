module DeparturesMDC2 (departuresMDC2) where

import Prelude ((#), ($), Unit, identity)

import DeparturesViewModel (arrival, boardOpening, boardRefreshedLine, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (dispatched, fold, looped, replaying, ticks, with)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body2, list, listItem, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC2 :: Effect Unit
departuresMDC2 =
  body $
    ( Semigroupoid.do
      ( Semigroupoid.do
        list $ ( listItem $ text flightLine ) # shown # dispatched @String @{ code :: String, status :: String } arrival
        body2 (text updateLine) ) # shown
      ticks @"Board refreshed" tickPeriod # replaying @"Board refreshed" identity
      snackbar @"Board refreshed" boardRefreshedLine # fold tick
    ) # looped @( beat :: Int ) # with boardOpening
