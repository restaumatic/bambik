module TemperatureConverter.HTML (temperatureConverter) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web (staticText)
import PUI.Web.HTML (body, div, input, label, p)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverter (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverter :: Effect Unit
temperatureConverter =
  body $ div $ Semigroupoid.do
    p ( label $ Semigroupoid.do
      staticText "°C "
      input @"celsius" "text" ) # settled fromCelsius
    p ( label $ Semigroupoid.do
      staticText "°F "
      input @"fahrenheit" "text" ) # settled fromFahrenheit
  # looped @( celsius :: String, fahrenheit :: String ) # with roomTemperature
