module TemperatureConverter.HTML (temperatureConverter) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web (staticText)
import PUI.Web.HTML (body, div, input, label, p)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverter (blankTemperatures, fromCelsius, fromFahrenheit)

temperatureConverter :: Effect Unit
temperatureConverter =
  body $ div $ Semigroupoid.do
    ( p $ label $ Semigroupoid.do
      staticText "°C "
      input @"Celsius" "text" ) # settled fromCelsius
    ( p $ label $ Semigroupoid.do
      staticText "°F "
      input @"Fahrenheit" "text" ) # settled fromFahrenheit
  # looped # with blankTemperatures
