module TemperatureConverterShoelace (temperatureConverterShoelace) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.HTML (body)
import PUI.Web.Shoelace (card, textField)
import QualifiedDo.Semigroupoid as Pipeline
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterShoelace :: Effect Unit
temperatureConverterShoelace =
  body $
    card $ ( Pipeline.do
        textField @"°C" {} # settled fromCelsius
        textField @"°F" {} # settled fromFahrenheit
    ) # mvu roomTemperature
