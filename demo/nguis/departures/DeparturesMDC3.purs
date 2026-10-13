module DeparturesMDC3 (departuresMDC3) where

import Prelude (Unit, identity, (#), ($))

import DeparturesViewModel (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (dispatched, fold, looped, prefixed, ticks, unshown, with)
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyMedium, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC3 :: Effect Unit
departuresMDC3 =
  body $ Semigroupoid.do
    prefixed ( Semigroupoid.do
      list $ prefixed (listItem $ text flightLine) identity # dispatched @String @{ code :: String, status :: String } arrival
      bodyMedium (text updateLine) ) $ unshown @"Board refreshed" # ticks tickPeriod
    unshown @"Board refreshed" # fold tick
  # looped @( beat :: Int ) # with boardOpening
