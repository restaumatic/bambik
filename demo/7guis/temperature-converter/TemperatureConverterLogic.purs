module TemperatureConverterLogic (celsiusText, fahrenheitText, fromCelsius, fromFahrenheit, roomTemperature) where

import Prelude (show, (*), (+), (-), (/))

import Data.Maybe (Maybe(..))
import Data.Number (fromString)

roomTemperature :: { celsius :: Number, fahrenheit :: Number }
roomTemperature = { celsius: 20.0, fahrenheit: 68.0 }

celsiusText :: { celsius :: Number } -> String
celsiusText { celsius } = show celsius

fahrenheitText :: { fahrenheit :: Number } -> String
fahrenheitText { fahrenheit } = show fahrenheit

fromCelsius :: { celsiusText :: String, celsius :: Number, fahrenheit :: Number } -> { celsius :: Number, fahrenheit :: Number }
fromCelsius { celsiusText: typed, celsius, fahrenheit } = case fromString typed of
  Just c -> { celsius: c, fahrenheit: c * 9.0 / 5.0 + 32.0 }
  Nothing -> { celsius, fahrenheit }

fromFahrenheit :: { fahrenheitText :: String, celsius :: Number, fahrenheit :: Number } -> { celsius :: Number, fahrenheit :: Number }
fromFahrenheit { fahrenheitText: typed, celsius, fahrenheit } = case fromString typed of
  Just f -> { fahrenheit: f, celsius: (f - 32.0) * 5.0 / 9.0 }
  Nothing -> { celsius, fahrenheit }
