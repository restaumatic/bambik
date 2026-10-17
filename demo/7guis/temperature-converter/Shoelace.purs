module TemperatureConverter.Shoelace (temperatureConverter) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web.Shoelace (body, textField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverter (blankTemperatures, fromCelsius, fromFahrenheit)

temperatureConverter :: Effect Unit
temperatureConverter =
  body $ Semigroupoid.do
    textField @"Celsius" # settled fromCelsius
    textField @"Fahrenheit" # settled fromFahrenheit
  # looped # with blankTemperatures
