module TemperatureConverter (Temperatures, blankTemperatures, fromCelsius, fromFahrenheit) where

import Prelude (not, show, (*), (+), (-), (/), (<))

import Data.Maybe (Maybe(..))
import Data.Number (abs, fromString)

type Temperatures = { "Celsius" :: String, "Fahrenheit" :: String }

blankTemperatures :: Temperatures
blankTemperatures = { "Celsius": "", "Fahrenheit": "" }

fromCelsius :: Temperatures -> Temperatures
fromCelsius r = case fromString r."Celsius" of
  Just c | not (agrees (fahrenheitOf c) r."Fahrenheit") -> r { "Fahrenheit" = show (fahrenheitOf c) }
  _ -> r

fromFahrenheit :: Temperatures -> Temperatures
fromFahrenheit r = case fromString r."Fahrenheit" of
  Just f | not (agrees (celsiusOf f) r."Celsius") -> r { "Celsius" = show (celsiusOf f) }
  _ -> r

fahrenheitOf :: Number -> Number
fahrenheitOf c = c * 9.0 / 5.0 + 32.0

celsiusOf :: Number -> Number
celsiusOf f = (f - 32.0) * 5.0 / 9.0

agrees :: Number -> String -> Boolean
agrees expected shown = case fromString shown of
  Just n -> abs (n - expected) < 1.0e-9
  Nothing -> false
