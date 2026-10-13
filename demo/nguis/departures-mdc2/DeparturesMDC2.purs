module DeparturesMDC2 (departuresMDC2) where

import Prelude (Unit, identity, (#), ($))

import DeparturesViewModel (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (dispatched, fold, looped, prefixed, ticks, unshown, with)
import PUI.Web (text)
import PUI.Web.MDC2 (body, body2, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid

departuresMDC2 :: Effect Unit
departuresMDC2 =
  body $ Semigroupoid.do
    prefixed ( Semigroupoid.do
      list $ prefixed (listItem $ text flightLine) identity # dispatched @String @{ code :: String, status :: String } arrival
      body2 (text updateLine) ) $ unshown @"Board refreshed" # ticks tickPeriod
    unshown @"Board refreshed" # fold tick
  # looped @( beat :: Int ) # with boardOpening
