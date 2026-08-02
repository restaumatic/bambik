module TemperatureConverterMDC2 (temperatureConverterMDC2) where

import Prelude ((#), ($), (*), (+), (-), (/), Unit, show)

import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Effect (Effect)
import PUI (forField, mvu, projected, updated)
import PUI.HTML (body)
import PUI.MDC2 (card, elevation20, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid

temperatureConverterMDC2 :: Effect Unit
temperatureConverterMDC2 =
  body $
    elevation20 $
      card { caption: "Temperature Converter" } $ ( Semigroupoid.do
          filledTextField { floatingLabel: "Celsius" } # forField @"celsius" show # updated fromCelsius
          filledTextField { floatingLabel: "Fahrenheit" } # forField @"fahrenheit" show # updated fromFahrenheit
      ) # mvu roomTemperature

fromCelsius :: { value :: String } -> { celsius :: Number, fahrenheit :: Number } -> { celsius :: Number, fahrenheit :: Number }
fromCelsius { value } m = case fromString value of
  Just c -> m { celsius = c, fahrenheit = c * 9.0 / 5.0 + 32.0 }
  Nothing -> m

fromFahrenheit :: { value :: String } -> { celsius :: Number, fahrenheit :: Number } -> { celsius :: Number, fahrenheit :: Number }
fromFahrenheit { value } m = case fromString value of
  Just f -> m { fahrenheit = f, celsius = (f - 32.0) * 5.0 / 9.0 }
  Nothing -> m

roomTemperature :: { celsius :: Number, fahrenheit :: Number }
roomTemperature = { celsius: 20.0, fahrenheit: 68.0 }
