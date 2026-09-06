module TemperatureConverterHTML (temperatureConverterHTML) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.HTML (shown, body, div, input, label, p, staticText)
import QualifiedDo.Category as Category
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterHTML :: Effect Unit
temperatureConverterHTML =
  body $ div $ ( Category.do
    p ( label $ Category.do
      (staticText "°C ") # shown
      input @"°C" "text" ) # settled fromCelsius
    p ( label $ Category.do
      (staticText "°F ") # shown
      input @"°F" "text" ) # settled fromFahrenheit
  ) # mvu roomTemperature
