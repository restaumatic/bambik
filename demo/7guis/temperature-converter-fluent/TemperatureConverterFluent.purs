module TemperatureConverterFluent (temperatureConverterFluent) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.Fluent (card, textField)
import PUI.Web.HTML (body)
import QualifiedDo.Semigroupoid as Pipeline
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterFluent :: Effect Unit
temperatureConverterFluent =
  body $
    card $ ( Pipeline.do
        textField @"°C" {} # settled fromCelsius
        textField @"°F" {} # settled fromFahrenheit
    ) # mvu roomTemperature
