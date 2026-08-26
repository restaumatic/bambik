module TemperatureConverterHTML (temperatureConverterHTML) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (field, mvu, settled)
import PUI.Web.HTML (shownAlways, body, div, input, label, p, staticText)
import QualifiedDo.Semigroupoid as Pipeline
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterHTML :: Effect Unit
temperatureConverterHTML =
  body $ div $ ( Pipeline.do
      p ( label $ Pipeline.do
          (staticText "Celsius ") # shownAlways
          input "text" # field @"°C" ) # settled fromCelsius
      p ( label $ Pipeline.do
          (staticText "Fahrenheit ") # shownAlways
          input "text" # field @"°F" ) # settled fromFahrenheit
  ) # mvu roomTemperature
