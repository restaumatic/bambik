module TemperatureConverterFluent (temperatureConverterFluent) where

import Prelude ((#), ($), Unit, show)

import Effect (Effect)
import PUI (projected, informed, mvu, updated)
import PUI.Web.Fluent (card, textField)
import PUI.Web.HTML (body)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterLogic (celsiusText, fahrenheitText, fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterFluent :: Effect Unit
temperatureConverterFluent =
  body $
    card { caption: "Temperature Converter" } $ ( Semigroupoid.do
        textField @"celsiusText" { label: "Celsius" } # projected celsiusText # updated (informed fromCelsius)
        textField @"fahrenheitText" { label: "Fahrenheit" } # projected fahrenheitText # updated (informed fromFahrenheit)
    ) # mvu roomTemperature
