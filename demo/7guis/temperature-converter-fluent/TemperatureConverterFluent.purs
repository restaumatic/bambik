module TemperatureConverterFluent (temperatureConverterFluent) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Pipeline as Pipeline
import PUI.Web.Fluent (body, textField)
import TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterFluent :: Effect Unit
temperatureConverterFluent =
  body $ Pipeline.do
    textField @"celsius" {} # settled fromCelsius
    textField @"fahrenheit" {} # settled fromFahrenheit
  # looped @( celsius :: String, fahrenheit :: String ) # with roomTemperature
