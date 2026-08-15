module TemperatureConverterHTML (temperatureConverterHTML) where

import Prelude ((#), ($), Unit, show)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (field, projected, informed, mvu, updated)
import PUI.Web.HTML (body, div, input, label, p, staticText)
import QualifiedDo.Semigroupoid as Semigroupoid
import TemperatureConverterLogic (celsiusText, fahrenheitText, fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterHTML :: Effect Unit
temperatureConverterHTML =
  body $ div $ ( Semigroupoid.do
      p ( label $ RecordToRecord.do
          staticText "Celsius "
          input "text" # field @"°C" ) # projected celsiusText # updated (informed fromCelsius)
      p ( label $ RecordToRecord.do
          staticText "Fahrenheit "
          input "text" # field @"°F" ) # projected fahrenheitText # updated (informed fromFahrenheit)
  ) # mvu roomTemperature
