module Main (main) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (fromString) as Number
import Data.Number.Format (fixed, toStringWith)
import Data.Profunctor (dimap)
import Data.String (Pattern(..), stripSuffix)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import PUI (looped, with)
import PUI.HTML (body) as HTML
import PUI.MDC (card, debouncedTextField, elevation20) as MDC
import QualifiedDo.Semigroupoid as Semigroupoid

main :: Effect Unit
main =
  HTML.body $ MDC.elevation20 $ MDC.card { caption: Just "Temperature Converter" } $ ( Semigroupoid.do
      MDC.debouncedTextField @"celsius" { floatingLabel: "Celsius", millis: Milliseconds 300.0 }
      MDC.debouncedTextField @"fahrenheit" { floatingLabel: "Fahrenheit", millis: Milliseconds 300.0 }
        # dimap celsiusToFahrenheit fahrenheitToCelsius
  ) # with { celsius: "20" } # looped

celsiusToFahrenheit :: { celsius :: String } -> { fahrenheit :: String }
celsiusToFahrenheit r = { fahrenheit: maybe "" (\c -> format (c * 9.0 / 5.0 + 32.0)) (Number.fromString r.celsius) }

fahrenheitToCelsius :: { fahrenheit :: String } -> { celsius :: String }
fahrenheitToCelsius r = { celsius: maybe "" (\f -> format ((f - 32.0) * 5.0 / 9.0)) (Number.fromString r.fahrenheit) }

format :: Number -> String
format n = let s = toStringWith (fixed 1) n in fromMaybe s (stripSuffix (Pattern ".0") s)
