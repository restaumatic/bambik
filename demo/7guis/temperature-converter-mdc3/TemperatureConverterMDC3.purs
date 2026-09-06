module TemperatureConverterMDC3 (temperatureConverterMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.MDC3 (body, card, filledTextField)
import QualifiedDo.Category as Category
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterMDC3 :: Effect Unit
temperatureConverterMDC3 =
  body $
    card $ ( Category.do
      filledTextField @"°C" {} # settled fromCelsius
      filledTextField @"°F" {} # settled fromFahrenheit
    ) # mvu roomTemperature
