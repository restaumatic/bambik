module TemperatureConverter.Shoelace (temperatureConverter) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web.Shoelace (body, textField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverter (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverter :: Effect Unit
temperatureConverter =
  body $ Semigroupoid.do
    textField @"celsius" # settled fromCelsius
    textField @"fahrenheit" # settled fromFahrenheit
  # looped # with roomTemperature
