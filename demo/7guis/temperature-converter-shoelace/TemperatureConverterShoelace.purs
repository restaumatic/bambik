module TemperatureConverterShoelace (temperatureConverterShoelace) where

import Prelude ((#), ($), Unit, show)

import Effect (Effect)
import PUI (forField, informed, mvu, updated)
import PUI.Web.HTML (body)
import PUI.Web.Shoelace (card, textField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterShoelace :: Effect Unit
temperatureConverterShoelace =
  body $
    card { caption: "Temperature Converter" } $ ( Semigroupoid.do
        textField @"value" { label: "Celsius" } # forField @"celsius" show # updated (informed fromCelsius)
        textField @"value" { label: "Fahrenheit" } # forField @"fahrenheit" show # updated (informed fromFahrenheit)
    ) # mvu roomTemperature
