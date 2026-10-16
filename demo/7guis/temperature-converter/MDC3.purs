module TemperatureConverter.MDC3 (temperatureConverter) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web.MDC3 (body, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverter (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverter :: Effect Unit
temperatureConverter =
  body $ Semigroupoid.do
    filledTextField @"Celsius" # settled fromCelsius
    filledTextField @"Fahrenheit" # settled fromFahrenheit
  # looped # with roomTemperature
