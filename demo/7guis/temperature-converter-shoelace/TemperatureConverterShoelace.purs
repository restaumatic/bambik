module TemperatureConverterShoelace (temperatureConverterShoelace) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web.Shoelace (body, textField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterShoelace :: Effect Unit
temperatureConverterShoelace =
  body $
    ( Semigroupoid.do
      textField @"°C" {} # settled fromCelsius
      textField @"°F" {} # settled fromFahrenheit
    ) # looped @( "°C" :: String, "°F" :: String ) # with roomTemperature
