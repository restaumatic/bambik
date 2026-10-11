module TemperatureConverterMDC3 (temperatureConverterMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Pipeline as Pipeline
import PUI.Web.MDC3 (body, filledTextField)
import TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterMDC3 :: Effect Unit
temperatureConverterMDC3 =
  body $ Pipeline.do
    filledTextField @"celsius" {} # settled fromCelsius
    filledTextField @"fahrenheit" {} # settled fromFahrenheit
  # looped @( celsius :: String, fahrenheit :: String ) # with roomTemperature
