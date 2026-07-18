module TemperatureConverter (temperatureConverter) where

import Prelude ((#), ($), (*), (+), (-), (/), Unit, show)

import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Effect (Effect)
import PUI (forField, mvu, projection, updates, widenRecordInput)
import PUI.HTML (body)
import PUI.MDC (card, elevation20, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid

type Model = { celsius :: Number, fahrenheit :: Number }

temperatureConverter :: Effect Unit
temperatureConverter =
  body $
    elevation20 $
      card { caption: "Temperature Converter" } $ ( Semigroupoid.do
          filledTextField { floatingLabel: "Celsius" }
            # projection show # forField @"celsius" # widenRecordInput # updates fromCelsius
          filledTextField { floatingLabel: "Fahrenheit" }
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
