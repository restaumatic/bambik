module TemperatureConverterMDC3 (temperatureConverterMDC3) where

import Prelude ((#), ($), Unit, show)

import Effect (Effect)
import PUI (forField, informed, mvu, updated)
import PUI.Web.HTML (body)
import PUI.Web.MDC3 (card, elevation5, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterMDC3 :: Effect Unit
temperatureConverterMDC3 =
  body $
    elevation5 $
      card { caption: "Temperature Converter" } $ ( Semigroupoid.do
          filledTextField @"value" { floatingLabel: "Celsius" } # forField @"celsius" show # updated (informed fromCelsius)
          filledTextField @"value" { floatingLabel: "Fahrenheit" } # forField @"fahrenheit" show # updated (informed fromFahrenheit)
      ) # mvu roomTemperature
