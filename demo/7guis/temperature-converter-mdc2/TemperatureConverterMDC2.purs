module TemperatureConverterMDC2 (temperatureConverterMDC2) where

import Prelude ((#), ($), Unit, show)

import Effect (Effect)
import PUI (projected, informed, mvu, updated)
import PUI.Web.HTML (body)
import PUI.Web.MDC2 (card, elevation20, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterLogic (celsiusText, fahrenheitText, fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterMDC2 :: Effect Unit
temperatureConverterMDC2 =
  body $
    elevation20 $
      card { caption: "Temperature Converter" } $ ( Semigroupoid.do
          filledTextField @"Degrees Celsius" {} # projected celsiusText # updated (informed fromCelsius)
          filledTextField @"Degrees Fahrenheit" {} # projected fahrenheitText # updated (informed fromFahrenheit)
      ) # mvu roomTemperature
