module TemperatureConverterLogic (fromCelsius, fromFahrenheit, roomTemperature) where

import Prelude ((*), (+), (-), (/))

import Data.Maybe (Maybe(..))
import Data.Number (fromString)

roomTemperature :: { celsius :: Number, fahrenheit :: Number }
roomTemperature = { celsius: 20.0, fahrenheit: 68.0 }

fromCelsius :: { value :: String, celsius :: Number, fahrenheit :: Number } -> { celsius :: Number, fahrenheit :: Number }
fromCelsius { value, celsius, fahrenheit } = case fromString value of
  Just c -> { celsius: c, fahrenheit: c * 9.0 / 5.0 + 32.0 }
  Nothing -> { celsius, fahrenheit }

fromFahrenheit :: { value :: String, celsius :: Number, fahrenheit :: Number } -> { celsius :: Number, fahrenheit :: Number }
fromFahrenheit { value, celsius, fahrenheit } = case fromString value of
  Just f -> { fahrenheit: f, celsius: (f - 32.0) * 5.0 / 9.0 }
  Nothing -> { celsius, fahrenheit }
