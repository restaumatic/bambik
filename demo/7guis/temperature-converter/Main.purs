module Main (main) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (fromString) as Number
import Data.Number.Format (fixed, toStringWith)
import Data.Profunctor (dimap)
import Data.String (Pattern(..), stripSuffix)
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import UI (debounced, looped)
import Web (bodyWith)

main :: Effect Unit
main = bodyWith { celsius: "20" } $ MDC.elevation20 $ MDC.card { caption: Just "Temperature Converter" } $ looped Semigroupoid.do
  MDC.filledTextField @"celsius" { floatingLabel: "Celsius" }
  debounced $ dimap celsiusToFahrenheit fahrenheitToCelsius $
    MDC.filledTextField @"fahrenheit" { floatingLabel: "Fahrenheit" }

celsiusToFahrenheit :: { celsius :: String } -> { fahrenheit :: String }
celsiusToFahrenheit r = { fahrenheit: maybe "" (\c -> format (c * 9.0 / 5.0 + 32.0)) (Number.fromString r.celsius) }

fahrenheitToCelsius :: { fahrenheit :: String } -> { celsius :: String }
fahrenheitToCelsius r = { celsius: maybe "" (\f -> format ((f - 32.0) * 5.0 / 9.0)) (Number.fromString r.fahrenheit) }

-- one decimal place, so the loop's canonicalization is stable
format :: Number -> String
format n = let s = toStringWith (fixed 1) n in fromMaybe s (stripSuffix (Pattern ".0") s)
