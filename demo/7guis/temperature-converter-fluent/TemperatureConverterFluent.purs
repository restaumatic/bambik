module TemperatureConverterFluent (temperatureConverterFluent) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web.Fluent (body, textField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterFluent :: Effect Unit
temperatureConverterFluent =
  body $
    ( Semigroupoid.do
      textField @"°C" {} # settled fromCelsius
      textField @"°F" {} # settled fromFahrenheit
    ) # looped @( "°C" :: String, "°F" :: String ) # with roomTemperature
