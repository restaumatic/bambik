module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (fromString) as Number
import Data.Profunctor (rmap)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import PUI (asField, completed, mvu)
import PUI.HTML (body) as HTML
import PUI.MDC (card, debouncedTextField, elevation20) as MDC
import QualifiedDo.Semigroupoid as Semigroupoid

type Model = { celsius :: String, fahrenheit :: String }

main :: Effect Unit
main =
  HTML.body $ MDC.elevation20 $ MDC.card { caption: Just "Temperature Converter" } $ ( Semigroupoid.do
      MDC.debouncedTextField { floatingLabel: "Celsius", millis: Milliseconds 300.0 } # asField @"celsius"
        # completed # rmap fromCelsius
      MDC.debouncedTextField { floatingLabel: "Fahrenheit", millis: Milliseconds 300.0 } # asField @"fahrenheit"
        # completed # rmap fromFahrenheit
  ) # mvu { celsius: "20", fahrenheit: "68" }

-- 7GUIs: a non-numeric entry leaves the other field untouched
fromCelsius :: Model -> Model
fromCelsius m = case Number.fromString m.celsius of
  Just c -> m { fahrenheit = show (c * 9.0 / 5.0 + 32.0) }
  Nothing -> m

fromFahrenheit :: Model -> Model
fromFahrenheit m = case Number.fromString m.fahrenheit of
  Just f -> m { celsius = show ((f - 32.0) * 5.0 / 9.0) }
  Nothing -> m
