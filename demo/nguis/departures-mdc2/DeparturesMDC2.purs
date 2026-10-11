module DeparturesMDC2 (departuresMDC2) where

import Prelude (Unit, identity, (#), ($))

import DeparturesViewModel (arrival, boardOpening, flightLine, tick, tickPeriod, updateLine)
import Effect (Effect)
import PUI (blankStatus, dispatched, fold, looped, prefixed, ticks, with)
import PUI.Pipeline as Pipeline
import PUI.Web (text)
import PUI.Web.MDC2 (body, body2, list, listItem)

departuresMDC2 :: Effect Unit
departuresMDC2 =
  body $ Pipeline.do
    ( Pipeline.do
      list $ prefixed (listItem $ text flightLine) identity # dispatched @String @{ code :: String, status :: String } arrival
      body2 (text updateLine) )
    blankStatus @"Board refreshed" # ticks tickPeriod
    blankStatus @"Board refreshed" # fold tick
  # looped @( beat :: Int ) # with boardOpening
