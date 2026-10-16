module TemperatureConverter.Bootstrap (temperatureConverter) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web.Bootstrap (body, textField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverter (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverter :: Effect Unit
temperatureConverter =
  body $ Semigroupoid.do
    textField @"Celsius" # settled fromCelsius
    textField @"Fahrenheit" # settled fromFahrenheit
  # looped # with roomTemperature
