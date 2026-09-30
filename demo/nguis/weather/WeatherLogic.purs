module WeatherLogic (aboutLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, rememberReport, servedLine, temperatureLine, warsawBulletin) where

import Prelude (discard, mod, pure, show, (*), (+), (-), (<#>), (<>), (==))

import Data.Array (filter, index)
import Data.Int (round, toNumber)
import Data.Maybe (fromMaybe)
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)

warsawBulletin :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int }
warsawBulletin = { report: conditionsFor "Warsaw" 0, servedReports: 1 }

temperatureLine :: forall r1. { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } | r1 } -> String
temperatureLine { report } = show report.temperature <> " °C"

conditionLine :: forall r1. { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } | r1 } -> String
conditionLine { report } = report.condition <> " in " <> report.city

humidityWindLine :: forall r1. { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } | r1 } -> String
humidityWindLine { report } = "Humidity " <> show report.humidity <> "% · Wind " <> show report.wind <> " km/h"

servedLine :: forall r1. { servedReports :: Int | r1 } -> String
servedLine { servedReports } = "Simulated service · " <> if servedReports == 1 then "1 report served" else show servedReports <> " reports served"

aboutLine :: forall r1. { servedReports :: Int | r1 } -> String
aboutLine { servedReports } = "A simulated weather service: canned per-city climate with slight variation per reading, served with a " <> show (round serviceDelay.ms) <> " ms delay. Reports served so far: " <> show servedReports <> "."

serviceDelay :: { ms :: Number }
serviceDelay = { ms: 800.0 }

climateTable :: Array { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }
climateTable =
  [ { city: "Warsaw", temperature: 21.0, condition: "Partly cloudy", humidity: 55, wind: 12.0 }
  , { city: "Lisbon", temperature: 27.0, condition: "Sunny", humidity: 48, wind: 18.0 }
  , { city: "Reykjavik", temperature: 11.0, condition: "Drizzle", humidity: 82, wind: 26.0 }
  , { city: "Cairo", temperature: 36.0, condition: "Clear sky", humidity: 22, wind: 9.0 }
  , { city: "Singapore", temperature: 31.0, condition: "Thunderstorm", humidity: 88, wind: 7.0 }
  , { city: "Sydney", temperature: 17.0, condition: "Showers", humidity: 64, wind: 21.0 }
  ]

conditionsFor :: String -> Int -> { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }
conditionsFor city sample =
  let base = firstWithCity city
  in base
    { temperature = base.temperature + toNumber (sample * 3 `mod` 5) - 2.0
    , humidity = base.humidity + (sample * 7 `mod` 9) - 4
    , wind = base.wind + toNumber (sample * 5 `mod` 7) - 3.0
    }

firstWithCity :: String -> { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }
firstWithCity city = fromMaybe unknownTerritory (index (filter (\r -> r.city == city) climateTable) 0)

unknownTerritory :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }
unknownTerritory = { city: "Unknown", temperature: 0.0, condition: "No data", humidity: 0, wind: 0.0 }

fetchReport :: forall r1. { city :: String, sample :: Int | r1 } -> Aff [ reportServed :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } } ]
fetchReport { city, sample } = do
  delay (Milliseconds serviceDelay.ms)
  pure (.reportServed { report: conditionsFor city sample })

rememberReport :: forall r1 r2. { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } | r1 } -> { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int | r2 } -> { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int | r2 }
rememberReport { report } forecast = forecast { report = report, servedReports = forecast.servedReports + 1 }

forecastRequests :: forall r1. { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int | r1 } -> Array { request :: { city :: String, sample :: Int }, focus :: [ current :: {}, other :: {} ] }
forecastRequests { servedReports, report } = climateTable <#> \r ->
  { request: { city: r.city, sample: servedReports }, focus: if r.city == report.city then .current {} else .other {} }

isCurrent :: forall r1. { request :: { city :: String, sample :: Int }, focus :: [ current :: {}, other :: {} ] | r1 } -> Boolean
isCurrent { focus } = match { current: \_ -> true, other: \_ -> false } focus
