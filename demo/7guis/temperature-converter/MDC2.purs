module TemperatureConverter.MDC2 (temperatureConverter) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web.MDC2 (body, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverter (blankTemperatures, fromCelsius, fromFahrenheit)

temperatureConverter :: Effect Unit
temperatureConverter =
  body $ Semigroupoid.do
    filledTextField @"Celsius" # settled fromCelsius
    filledTextField @"Fahrenheit" # settled fromFahrenheit
  # looped # with blankTemperatures
