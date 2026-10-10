module DeparturesMDC3 (departuresMDC3) where

import Prelude ((#), ($), Unit)

import DeparturesViewModel (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (blankStatus, dispatched, fold, looped, ticks, with)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyMedium, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC3 :: Effect Unit
departuresMDC3 =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      list $ ( listItem $ text flightLine ) # shown # dispatched @String @{ code :: String, status :: String } arrival
      bodyMedium (text updateLine) ) # shown
    blankStatus @"Board refreshed" # ticks tickPeriod
    blankStatus @"Board refreshed" # fold tick
  # looped @( beat :: Int ) # with boardOpening
