module TemperatureConverterBootstrap (temperatureConverterBootstrap) where

import Prelude ((#), ($), Unit, show)

import Effect (Effect)
import PUI (forField, informed, mvu, updated)
import PUI.Web.Bootstrap (card, textField)
import PUI.Web.HTML (body)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterBootstrap :: Effect Unit
temperatureConverterBootstrap =
  body $
    card { caption: "Temperature Converter" } $ ( Semigroupoid.do
        textField @"value" { label: "Celsius" } # forField @"celsius" show # updated (informed fromCelsius)
        textField @"value" { label: "Fahrenheit" } # forField @"fahrenheit" show # updated (informed fromFahrenheit)
    ) # mvu roomTemperature
