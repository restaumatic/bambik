module TemperatureConverterMDC2 (temperatureConverterMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.MDC2 (body, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterMDC2 :: Effect Unit
temperatureConverterMDC2 =
  body $
    ( Semigroupoid.do
      filledTextField @"°C" {} # settled fromCelsius
      filledTextField @"°F" {} # settled fromFahrenheit
    ) # mvu roomTemperature
