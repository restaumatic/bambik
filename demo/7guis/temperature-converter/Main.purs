module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (fromString) as Number
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import PUI (forField, mvu, projection, updates, widenRecordInput)
import PUI.HTML (body) as HTML
import PUI.MDC (card, debouncedTextField, elevation20) as MDC
import QualifiedDo.Semigroupoid as Semigroupoid

type Model = { celsius :: Number, fahrenheit :: Number }

main :: Effect Unit
main =
  HTML.body $ MDC.elevation20 $ MDC.card { caption: Just "Temperature Converter" } $ ( Semigroupoid.do
      MDC.debouncedTextField { floatingLabel: "Celsius", millis: Milliseconds 300.0 }
        # projection show # forField @"celsius" # widenRecordInput # updates fromCelsius
      MDC.debouncedTextField { floatingLabel: "Fahrenheit", millis: Milliseconds 300.0 }
        # projection show # forField @"fahrenheit" # widenRecordInput # updates fromFahrenheit
  ) # mvu { celsius: 20.0, fahrenheit: 68.0 }

fromCelsius :: { value :: String } -> Model -> Model
fromCelsius { value } m = case Number.fromString value of
  Just c -> m { celsius = c, fahrenheit = c * 9.0 / 5.0 + 32.0 }
  Nothing -> m

fromFahrenheit :: { value :: String } -> Model -> Model
fromFahrenheit { value } m = case Number.fromString value of
  Just f -> m { fahrenheit = f, celsius = (f - 32.0) * 5.0 / 9.0 }
  Nothing -> m
