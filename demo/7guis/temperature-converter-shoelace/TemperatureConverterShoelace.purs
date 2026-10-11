module TemperatureConverterShoelace (temperatureConverterShoelace) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Pipeline as Pipeline
import PUI.Web.Shoelace (body, textField)
import TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterShoelace :: Effect Unit
temperatureConverterShoelace =
  body $ Pipeline.do
    textField @"celsius" {} # settled fromCelsius
    textField @"fahrenheit" {} # settled fromFahrenheit
  # looped @( celsius :: String, fahrenheit :: String ) # with roomTemperature
