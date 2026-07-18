module TemperatureConverter (temperatureConverter) where

import Prelude ((#), ($), (*), (+), (-), (/), Unit, show)

import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Effect (Effect)
import PUI (forField, mvu, projection, updates, widenRecordInput)
import PUI.HTML (body)
import PUI.MDC (card, elevation20, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid

type Temperature = { celsius :: Number, fahrenheit :: Number }

temperatureConverter :: Effect Unit
temperatureConverter =
  body $
    elevation20 $
      card { caption: "Temperature Converter" } $ ( Semigroupoid.do
          filledTextField { floatingLabel: "Celsius" }
            # projection show # forField @"celsius" # widenRecordInput # updates fromCelsius
          filledTextField { floatingLabel: "Fahrenheit" }
            # projection show # forField @"fahrenheit" # widenRecordInput # updates fromFahrenheit
      ) # mvu roomTemperature

fromCelsius :: { value :: String } -> Temperature -> Temperature
fromCelsius { value } m = case fromString value of
  Just c -> m { celsius = c, fahrenheit = c * 9.0 / 5.0 + 32.0 }
  Nothing -> m

fromFahrenheit :: { value :: String } -> Temperature -> Temperature
fromFahrenheit { value } m = case fromString value of
  Just f -> m { fahrenheit = f, celsius = (f - 32.0) * 5.0 / 9.0 }
  Nothing -> m

roomTemperature :: Temperature
roomTemperature = { celsius: 20.0, fahrenheit: 68.0 }
