module TemperatureConverterMDC2 (temperatureConverterMDC2) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordUpdate as RecordUpdate
import Effect (Effect)
import PUI (looped, settled, with)
import PUI.Web.MDC2 (body, filledTextField)
import TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterMDC2 :: Effect Unit
temperatureConverterMDC2 =
  body $
    RecordUpdate.do
      filledTextField @"celsius" {} # settled fromCelsius
      filledTextField @"fahrenheit" {} # settled fromFahrenheit
    # looped @( celsius :: String, fahrenheit :: String ) # with roomTemperature
