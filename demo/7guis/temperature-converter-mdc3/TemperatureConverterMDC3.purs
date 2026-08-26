module TemperatureConverterMDC3 (temperatureConverterMDC3) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.HTML (body)
import PUI.Web.MDC3 (card, elevation5, filledTextField)
import QualifiedDo.Category as Category
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterMDC3 :: Effect Unit
temperatureConverterMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          filledTextField @"°C" {} # settled fromCelsius
          filledTextField @"°F" {} # settled fromFahrenheit
      ) # mvu roomTemperature
