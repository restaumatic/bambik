module TemperatureConverterMDC2 (temperatureConverterMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.HTML (body)
import PUI.Web.MDC2 (card, elevation20, filledTextField)
import QualifiedDo.Semigroupoid as Pipeline
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterMDC2 :: Effect Unit
temperatureConverterMDC2 =
  body $
    elevation20 $
      card $ ( Pipeline.do
          filledTextField @"°C" {} # settled fromCelsius
          filledTextField @"°F" {} # settled fromFahrenheit
      ) # mvu roomTemperature
