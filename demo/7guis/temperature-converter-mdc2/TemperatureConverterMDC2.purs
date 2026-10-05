module TemperatureConverterMDC2 (temperatureConverterMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web.MDC2 (body, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterMDC2 :: Effect Unit
temperatureConverterMDC2 =
  body $
    ( Semigroupoid.do
      filledTextField @"°C" {} # settled fromCelsius
      filledTextField @"°F" {} # settled fromFahrenheit
    ) # looped @( "°C" :: String, "°F" :: String ) # with roomTemperature
