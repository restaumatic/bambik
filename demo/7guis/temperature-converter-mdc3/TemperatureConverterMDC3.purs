module TemperatureConverterMDC3 (temperatureConverterMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.MDC3 (body, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterMDC3 :: Effect Unit
temperatureConverterMDC3 =
  body $
    ( Semigroupoid.do
      filledTextField @"°C" {} # settled fromCelsius
      filledTextField @"°F" {} # settled fromFahrenheit
    ) # mvu roomTemperature
