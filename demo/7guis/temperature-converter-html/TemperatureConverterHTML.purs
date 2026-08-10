module TemperatureConverterHTML (temperatureConverterHTML) where

import Prelude ((#), ($), Unit, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (field, forField, informed, mvu, updated)
import PUI.Web.HTML (body, div, input, label, p, staticText)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterHTML :: Effect Unit
temperatureConverterHTML =
  body $ div $ ( Semigroupoid.do
      p ( label $ RecordToRecord.do
          staticText "Celsius "
          input "text" # field @"value" ) # forField @"value" @"celsius" show # updated (informed fromCelsius)
      p ( label $ RecordToRecord.do
          staticText "Fahrenheit "
          input "text" # field @"value" ) # forField @"value" @"fahrenheit" show # updated (informed fromFahrenheit)
  ) # mvu roomTemperature
