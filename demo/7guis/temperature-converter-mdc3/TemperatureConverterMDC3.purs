module TemperatureConverterMDC3 (temperatureConverterMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web.MDC3 (body, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterMDC3 :: Effect Unit
temperatureConverterMDC3 =
  body $ Semigroupoid.do
    filledTextField @"celsius" {} # settled fromCelsius
    filledTextField @"fahrenheit" {} # settled fromFahrenheit
  # looped @( celsius :: String, fahrenheit :: String ) # with roomTemperature
