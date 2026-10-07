module TemperatureConverterViewModelTest (temperatureConverterClaims) where

import Prelude ((==))

import TemperatureConverterViewModel (fromCelsius, fromFahrenheit, roomTemperature)

temperatureConverterClaims :: Array { claim :: String, holds :: Boolean }
temperatureConverterClaims =
  [ { claim: "boiling water in Celsius is 212 in Fahrenheit", holds: (fromCelsius { celsius: "100", fahrenheit: "" }).fahrenheit == "212.0" }
  , { claim: "freezing water in Fahrenheit is 0 in Celsius", holds: (fromFahrenheit { celsius: "", fahrenheit: "32" }).celsius == "0.0" }
  , { claim: "a non-number leaves the other scale untouched", holds: fromCelsius { celsius: "warm", fahrenheit: "68.0" } == { celsius: "warm", fahrenheit: "68.0" } }
  , { claim: "room temperature agrees with itself", holds: fromCelsius roomTemperature == roomTemperature }
  ]
