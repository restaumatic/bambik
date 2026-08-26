module TemperatureConverterShoelace (temperatureConverterShoelace) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.HTML (body)
import PUI.Web.Shoelace (card, textField)
import QualifiedDo.Category as Category
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterShoelace :: Effect Unit
temperatureConverterShoelace =
  body $
    card $ ( Category.do
        textField @"°C" {} # settled fromCelsius
        textField @"°F" {} # settled fromFahrenheit
    ) # mvu roomTemperature
