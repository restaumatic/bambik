module TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature) where

import Prelude (show, (*), (+), (-), (/))

import Data.Maybe (Maybe(..))
import Data.Number (fromString)

roomTemperature :: { "°C" :: String, "°F" :: String }
roomTemperature = { "°C": "20.0", "°F": "68.0" }

fromCelsius :: forall r1. { "°C" :: String, "°F" :: String | r1 } -> { "°C" :: String, "°F" :: String | r1 }
fromCelsius r = case fromString r."°C" of
  Just c -> r { "°F" = show (c * 9.0 / 5.0 + 32.0) }
  Nothing -> r

fromFahrenheit :: forall r1. { "°C" :: String, "°F" :: String | r1 } -> { "°C" :: String, "°F" :: String | r1 }
fromFahrenheit r = case fromString r."°F" of
  Just f -> r { "°C" = show ((f - 32.0) * 5.0 / 9.0) }
  Nothing -> r
