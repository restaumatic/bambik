module TemperatureConverterBootstrap (temperatureConverterBootstrap) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web.Bootstrap (body, textField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterBootstrap :: Effect Unit
temperatureConverterBootstrap =
  body $
    ( Semigroupoid.do
      textField @"°C" {} # settled fromCelsius
      textField @"°F" {} # settled fromFahrenheit
    ) # looped @( "°C" :: String, "°F" :: String ) # with roomTemperature
