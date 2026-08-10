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
        textField { label: "Celsius" } # forField @"value" @"celsius" show # updated (informed fromCelsius)
        textField { label: "Fahrenheit" } # forField @"value" @"fahrenheit" show # updated (informed fromFahrenheit)
    ) # mvu roomTemperature
