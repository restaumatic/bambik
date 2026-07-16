-- | 7GUIs task 2: **Temperature Converter** — bidirectional C ↔ F fields.
-- |
-- | The model holds one temperature (`{ celsius :: String }`); the
-- | Fahrenheit field is the same model seen through a `dimap` conversion
-- | bracket. `looped` ties the two stages into a self-trace, so an edit in
-- | either field flows around the loop and re-renders the other. The
-- | Fahrenheit stage is `debounced`, so mid-typing bursts settle before
-- | the loop canonicalizes the fields.
-- |
-- | Per the task, a non-numeric entry leaves the other field alone —
-- | here the conversion maps unparseable input to the empty string.
module Main (main) where

import Prelude

import Data.Int (round, toNumber) as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Number (fromString) as Number
import Data.Profunctor (dimap, lcmap)
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import UI (debounced, looped, silence)
import Web (body)

main :: Effect Unit
main = body @Unit $ MDC.elevation20 $ MDC.card { caption: Just "Temperature Converter" } Semigroupoid.do
  lcmap (const { celsius: "20" }) $ looped Semigroupoid.do
    MDC.filledTextField @"celsius" { floatingLabel: "Celsius" }
    debounced $ dimap celsiusToFahrenheit fahrenheitToCelsius $
      MDC.filledTextField @"fahrenheit" { floatingLabel: "Fahrenheit" }
  silence

celsiusToFahrenheit :: { celsius :: String } -> { fahrenheit :: String }
celsiusToFahrenheit r = { fahrenheit: maybe "" (\c -> format (c * 9.0 / 5.0 + 32.0)) (Number.fromString r.celsius) }

fahrenheitToCelsius :: { fahrenheit :: String } -> { celsius :: String }
fahrenheitToCelsius r = { celsius: maybe "" (\f -> format ((f - 32.0) * 5.0 / 9.0)) (Number.fromString r.fahrenheit) }

-- one decimal place, so the loop's canonicalization is stable
format :: Number -> String
format n =
  let scaled = Int.round (n * 10.0)
  in if scaled `mod` 10 == 0
     then show (scaled / 10)
     else show (Int.toNumber scaled / 10.0)
