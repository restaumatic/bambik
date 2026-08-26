module TemperatureConverterBootstrap (temperatureConverterBootstrap) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (mvu, settled)
import PUI.Web.Bootstrap (card, textField)
import PUI.Web.HTML (body)
import QualifiedDo.Category as Category
import TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterBootstrap :: Effect Unit
temperatureConverterBootstrap =
  body $
    card $ ( Category.do
        textField @"°C" {} # settled fromCelsius
        textField @"°F" {} # settled fromFahrenheit
    ) # mvu roomTemperature
