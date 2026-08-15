module TemperatureConverterMDC3 (temperatureConverterMDC3) where

import Prelude ((#), ($), Unit, show)

import Effect (Effect)
import PUI (projected, informed, mvu, updated)
import PUI.Web.HTML (body)
import PUI.Web.MDC3 (card, elevation5, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterLogic (celsiusText, fahrenheitText, fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterMDC3 :: Effect Unit
temperatureConverterMDC3 =
  body $
    elevation5 $
      card { caption: "Temperature Converter" } $ ( Semigroupoid.do
          filledTextField @"Celsius" {} # projected celsiusText # updated (informed fromCelsius)
          filledTextField @"Fahrenheit" {} # projected fahrenheitText # updated (informed fromFahrenheit)
      ) # mvu roomTemperature
