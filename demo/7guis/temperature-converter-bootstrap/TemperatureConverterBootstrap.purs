module TemperatureConverterBootstrap (temperatureConverterBootstrap) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web.Bootstrap (body, textField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterBootstrap :: Effect Unit
temperatureConverterBootstrap =
  body $ Semigroupoid.do
    textField @"celsius" {} # settled fromCelsius
    textField @"fahrenheit" {} # settled fromFahrenheit
  # looped @( celsius :: String, fahrenheit :: String ) # with roomTemperature
