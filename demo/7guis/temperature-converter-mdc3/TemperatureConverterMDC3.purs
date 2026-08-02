module TemperatureConverterMDC3 (temperatureConverterMDC3) where

import Prelude ((#), ($), (*), (+), (-), (/), Unit, show)

import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Effect (Effect)
import PUI (forField, informed, mvu, projected, updated)
import PUI.HTML (body)
import PUI.MDC3 (card, elevation5, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid

temperatureConverterMDC3 :: Effect Unit
temperatureConverterMDC3 =
  body $
    elevation5 $
      card { caption: "Temperature Converter" } $ ( Semigroupoid.do
          filledTextField { floatingLabel: "Celsius" } # forField @"celsius" show # updated (informed fromCelsius)
          filledTextField { floatingLabel: "Fahrenheit" } # forField @"fahrenheit" show # updated (informed fromFahrenheit)
      ) # mvu roomTemperature

fromCelsius :: { value :: String, celsius :: Number, fahrenheit :: Number } -> { celsius :: Number, fahrenheit :: Number }
fromCelsius { value, celsius, fahrenheit } = case fromString value of
  Just c -> { celsius: c, fahrenheit: c * 9.0 / 5.0 + 32.0 }
  Nothing -> { celsius, fahrenheit }

fromFahrenheit :: { value :: String, celsius :: Number, fahrenheit :: Number } -> { celsius :: Number, fahrenheit :: Number }
fromFahrenheit { value, celsius, fahrenheit } = case fromString value of
  Just f -> { fahrenheit: f, celsius: (f - 32.0) * 5.0 / 9.0 }
  Nothing -> { celsius, fahrenheit }

roomTemperature :: { celsius :: Number, fahrenheit :: Number }
roomTemperature = { celsius: 20.0, fahrenheit: 68.0 }
