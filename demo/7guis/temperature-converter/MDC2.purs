module TemperatureConverter.MDC2 (temperatureConverter) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web.MDC2 (body, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverter (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverter :: Effect Unit
temperatureConverter =
  body $ Semigroupoid.do
    filledTextField @"celsius" # settled fromCelsius
    filledTextField @"fahrenheit" # settled fromFahrenheit
  # looped # with roomTemperature
