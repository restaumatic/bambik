module Main (main) where

import Prelude ((#), ($), (*), (+), (-), (/), Unit, show)

import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import PUI (forField, mvu, projection, updates, widenRecordInput)
import PUI.HTML (body)
import PUI.MDC (card, debouncedTextField, elevation20)
import QualifiedDo.Semigroupoid as Semigroupoid

type Model = { celsius :: Number, fahrenheit :: Number }

main :: Effect Unit
main =
  body $ elevation20 $ card { caption: Just "Temperature Converter" } $ ( Semigroupoid.do
      debouncedTextField { floatingLabel: "Celsius", millis: Milliseconds 300.0 }
        # projection show # forField @"celsius" # widenRecordInput # updates fromCelsius
      debouncedTextField { floatingLabel: "Fahrenheit", millis: Milliseconds 300.0 }
        # projection show # forField @"fahrenheit" # widenRecordInput # updates fromFahrenheit
  ) # mvu { celsius: 20.0, fahrenheit: 68.0 }

fromCelsius :: { value :: String } -> Model -> Model
fromCelsius { value } m = case fromString value of
  Just c -> m { celsius = c, fahrenheit = c * 9.0 / 5.0 + 32.0 }
  Nothing -> m

fromFahrenheit :: { value :: String } -> Model -> Model
fromFahrenheit { value } m = case fromString value of
  Just f -> m { fahrenheit = f, celsius = (f - 32.0) * 5.0 / 9.0 }
  Nothing -> m
