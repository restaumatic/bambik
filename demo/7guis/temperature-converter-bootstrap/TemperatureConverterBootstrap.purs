module TemperatureConverterBootstrap (temperatureConverterBootstrap) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.Bootstrap (body, textField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterBootstrap :: Effect Unit
temperatureConverterBootstrap =
  body $
    ( Semigroupoid.do
      textField @"°C" {} # settled fromCelsius
      textField @"°F" {} # settled fromFahrenheit
    ) # mvu roomTemperature
