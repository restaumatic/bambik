module TemperatureConverterLogic (celsiusText, fahrenheitText, fromCelsius, fromFahrenheit, roomTemperature) where

import Prelude (show, (*), (+), (-), (/))

import Data.Maybe (Maybe(..))
import Data.Number (fromString)

roomTemperature :: { celsiusReading :: Number, fahrenheitReading :: Number }
roomTemperature = { celsiusReading: 20.0, fahrenheitReading: 68.0 }

celsiusText :: { celsiusReading :: Number } -> String
celsiusText { celsiusReading } = show celsiusReading

fahrenheitText :: { fahrenheitReading :: Number } -> String
fahrenheitText { fahrenheitReading } = show fahrenheitReading

fromCelsius :: { "°C" :: String, celsiusReading :: Number, fahrenheitReading :: Number } -> { celsiusReading :: Number, fahrenheitReading :: Number }
fromCelsius { "°C": typed, celsiusReading, fahrenheitReading } = case fromString typed of
  Just c -> { celsiusReading: c, fahrenheitReading: c * 9.0 / 5.0 + 32.0 }
  Nothing -> { celsiusReading, fahrenheitReading }

fromFahrenheit :: { "°F" :: String, celsiusReading :: Number, fahrenheitReading :: Number } -> { celsiusReading :: Number, fahrenheitReading :: Number }
fromFahrenheit { "°F": typed, celsiusReading, fahrenheitReading } = case fromString typed of
  Just f -> { fahrenheitReading: f, celsiusReading: (f - 32.0) * 5.0 / 9.0 }
  Nothing -> { celsiusReading, fahrenheitReading }
