module TemperatureConverterHTML (temperatureConverterHTML) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web (shown, staticText)
import PUI.Web.HTML (body, div, input, label, p)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterHTML :: Effect Unit
temperatureConverterHTML =
  body $ div $ ( Semigroupoid.do
    p ( label $ Semigroupoid.do
      (staticText @"°C ") # shown
      input @"°C" "text" ) # settled fromCelsius
    p ( label $ Semigroupoid.do
      (staticText @"°F ") # shown
      input @"°F" "text" ) # settled fromFahrenheit
  ) # looped @( "°C" :: String, "°F" :: String ) # with roomTemperature
