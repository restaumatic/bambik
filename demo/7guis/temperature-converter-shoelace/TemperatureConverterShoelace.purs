module TemperatureConverterShoelace (temperatureConverterShoelace) where

import Prelude ((#), ($), Unit, show)

import Effect (Effect)
import PUI (projected, informed, mvu, updated)
import PUI.Web.HTML (body)
import PUI.Web.Shoelace (card, textField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterLogic (celsiusText, fahrenheitText, fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterShoelace :: Effect Unit
temperatureConverterShoelace =
  body $
    card { caption: "Temperature Converter" } $ ( Semigroupoid.do
        textField @"°C" {} # projected celsiusText # updated (informed fromCelsius)
        textField @"°F" {} # projected fahrenheitText # updated (informed fromFahrenheit)
    ) # mvu roomTemperature
