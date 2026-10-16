module TemperatureConverter (Temperatures, fromCelsius, fromFahrenheit, roomTemperature) where

import Prelude (show, (*), (+), (-), (/))

import Data.Maybe (Maybe(..))
import Data.Number (fromString)

type Temperatures = { celsius :: String, fahrenheit :: String }

roomTemperature :: Temperatures
roomTemperature = { celsius: "20.0", fahrenheit: "68.0" }

fromCelsius :: Temperatures -> Temperatures
fromCelsius r = case fromString r.celsius of
  Just c -> r { fahrenheit = show (c * 9.0 / 5.0 + 32.0) }
  Nothing -> r

fromFahrenheit :: Temperatures -> Temperatures
fromFahrenheit r = case fromString r.fahrenheit of
  Just f -> r { celsius = show ((f - 32.0) * 5.0 / 9.0) }
  Nothing -> r
