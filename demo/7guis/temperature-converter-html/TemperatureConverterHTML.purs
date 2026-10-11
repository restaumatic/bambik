module TemperatureConverterHTML (temperatureConverterHTML) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, prefixed, settled, with)
import PUI.Pipeline as Pipeline
import PUI.Web (staticText)
import PUI.Web.HTML (body, div, input, label, p)
import TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterHTML :: Effect Unit
temperatureConverterHTML =
  body $ div $ Pipeline.do
    p ( label $ prefixed (staticText "°C ") $ input @"celsius" "text" ) # settled fromCelsius
    p ( label $ prefixed (staticText "°F ") $ input @"fahrenheit" "text" ) # settled fromFahrenheit
  # looped @( celsius :: String, fahrenheit :: String ) # with roomTemperature
