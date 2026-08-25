module TemperatureConverterHTML (temperatureConverterHTML) where

import Prelude (Unit, identity, (#), ($))

import Effect (Effect)
import PUI (field, mvu, settled)
import PUI.Web.HTML (shownAs, body, div, input, label, p, staticText)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterHTML :: Effect Unit
temperatureConverterHTML =
  body $ div $ ( Semigroupoid.do
      p ( label $ Semigroupoid.do
          (staticText "Celsius ") # shownAs identity
          input "text" # field @"°C" ) # settled fromCelsius
      p ( label $ Semigroupoid.do
          (staticText "Fahrenheit ") # shownAs identity
          input "text" # field @"°F" ) # settled fromFahrenheit
  ) # mvu roomTemperature
