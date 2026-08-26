module TemperatureConverterHTML (temperatureConverterHTML) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (field, mvu, settled)
import PUI.Web.HTML (shown, body, div, input, label, p, staticText)
import QualifiedDo.Category as Category
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterHTML :: Effect Unit
temperatureConverterHTML =
  body $ div $ ( Category.do
      p ( label $ Category.do
          (staticText "Celsius ") # shown
          input "text" # field @"°C" ) # settled fromCelsius
      p ( label $ Category.do
          (staticText "Fahrenheit ") # shown
          input "text" # field @"°F" ) # settled fromFahrenheit
  ) # mvu roomTemperature
