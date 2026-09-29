module TemperatureConverterHTML (temperatureConverterHTML) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web (shown, staticText)
import PUI.Web.HTML (body, div, input, label, p)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterHTML :: Effect Unit
temperatureConverterHTML =
  body $ div $ ( Semigroupoid.do
    p ( label $ Semigroupoid.do
      (staticText @"°C ") # shown
      input @"°C" "text" ) # settled fromCelsius
    p ( label $ Semigroupoid.do
      (staticText @"°F ") # shown
      input @"°F" "text" ) # settled fromFahrenheit
  ) # mvu roomTemperature
