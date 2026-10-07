module TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature) where

import Prelude (show, (*), (+), (-), (/))

import Data.Maybe (Maybe(..))
import Data.Number (fromString)

roomTemperature :: { celsius :: String, fahrenheit :: String }
roomTemperature = { celsius: "20.0", fahrenheit: "68.0" }

fromCelsius :: { celsius :: String, fahrenheit :: String } -> { celsius :: String, fahrenheit :: String }
fromCelsius r = case fromString r.celsius of
  Just c -> r { fahrenheit = show (c * 9.0 / 5.0 + 32.0) }
  Nothing -> r

fromFahrenheit :: { celsius :: String, fahrenheit :: String } -> { celsius :: String, fahrenheit :: String }
fromFahrenheit r = case fromString r.fahrenheit of
  Just f -> r { celsius = show ((f - 32.0) * 5.0 / 9.0) }
  Nothing -> r
