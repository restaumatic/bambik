module TemperatureConverter (Temperatures, fromCelsius, fromFahrenheit, roomTemperature) where

import Prelude (show, (*), (+), (-), (/))

import Data.Maybe (Maybe(..))
import Data.Number (fromString)

type Temperatures = { "Celsius" :: String, "Fahrenheit" :: String }

roomTemperature :: Temperatures
roomTemperature = { "Celsius": "20.0", "Fahrenheit": "68.0" }

fromCelsius :: Temperatures -> Temperatures
fromCelsius r = case fromString r."Celsius" of
  Just c -> r { "Fahrenheit" = show (c * 9.0 / 5.0 + 32.0) }
  Nothing -> r

fromFahrenheit :: Temperatures -> Temperatures
fromFahrenheit r = case fromString r."Fahrenheit" of
  Just f -> r { "Celsius" = show ((f - 32.0) * 5.0 / 9.0) }
  Nothing -> r
