module TemperatureConverterBootstrap (temperatureConverterBootstrap) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (projected, informed, mvu, updated)
import PUI.Web.Bootstrap (card, textField)
import PUI.Web.HTML (body)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterLogic (celsiusText, fahrenheitText, fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterBootstrap :: Effect Unit
temperatureConverterBootstrap =
  body $
    card { caption: "Temperature Converter" } $ ( Semigroupoid.do
        textField @"°C" {} # projected celsiusText # updated (informed fromCelsius)
        textField @"°F" {} # projected fahrenheitText # updated (informed fromFahrenheit)
    ) # mvu roomTemperature
