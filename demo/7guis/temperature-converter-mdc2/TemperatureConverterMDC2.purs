module TemperatureConverterMDC2 (temperatureConverterMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.MDC2 (body, card, filledTextField)
import QualifiedDo.Category as Category
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterMDC2 :: Effect Unit
temperatureConverterMDC2 =
  body $
    card $ ( Category.do
        filledTextField @"°C" {} # settled fromCelsius
        filledTextField @"°F" {} # settled fromFahrenheit
    ) # mvu roomTemperature
