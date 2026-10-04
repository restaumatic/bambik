module WeatherViewModel (aboutLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, servedLine, temperatureLine, warsawBulletin) where

import Prelude (discard, mod, pure, show, (*), (+), (-), (<#>), (<>), (==))

import Data.Array (filter, index)
import Data.Int (round, toNumber)
import Data.Maybe (fromMaybe)
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)

warsawBulletin :: { report :: { city :: String, condition :: String, humidity :: Int, temperature :: Number, wind :: Number }, servedReports :: Int }
warsawBulletin = { report: conditionsFor "Warsaw" 0, servedReports: 1 }

temperatureLine :: { report :: { city :: String, condition :: String, humidity :: Int, temperature :: Number, wind :: Number }, servedReports :: Int } -> String
temperatureLine { report } = show report.temperature <> " °C"

conditionLine :: { report :: { city :: String, condition :: String, humidity :: Int, temperature :: Number, wind :: Number }, servedReports :: Int } -> String
conditionLine { report } = report.condition <> " in " <> report.city

humidityWindLine :: { report :: { city :: String, condition :: String, humidity :: Int, temperature :: Number, wind :: Number }, servedReports :: Int } -> String
humidityWindLine { report } = "Humidity " <> show report.humidity <> "% · Wind " <> show report.wind <> " km/h"

servedLine :: { report :: { city :: String, condition :: String, humidity :: Int, temperature :: Number, wind :: Number }, servedReports :: Int } -> String
servedLine { servedReports } = "Simulated service · " <> if servedReports == 1 then "1 report served" else show servedReports <> " reports served"

aboutLine :: { report :: { city :: String, condition :: String, humidity :: Int, temperature :: Number, wind :: Number }, servedReports :: Int } -> String
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

fetchReport :: { event :: { city :: String, sample :: Int }, model :: { report :: { city :: String, condition :: String, humidity :: Int, temperature :: Number, wind :: Number }, servedReports :: Int } } -> Aff { report :: { city :: String, condition :: String, humidity :: Int, temperature :: Number, wind :: Number }, servedReports :: Int }
fetchReport { event: { city, sample }, model: forecast } = do
  delay (Milliseconds serviceDelay.ms)
  pure (rememberReport { report: conditionsFor city sample } forecast)

rememberReport :: { report :: { city :: String, condition :: String, humidity :: Int, temperature :: Number, wind :: Number } } -> { report :: { city :: String, condition :: String, humidity :: Int, temperature :: Number, wind :: Number }, servedReports :: Int } -> { report :: { city :: String, condition :: String, humidity :: Int, temperature :: Number, wind :: Number }, servedReports :: Int }
rememberReport { report } forecast = forecast { report = report, servedReports = forecast.servedReports + 1 }

forecastRequests :: { report :: { city :: String, condition :: String, humidity :: Int, temperature :: Number, wind :: Number }, servedReports :: Int } -> Array { focus :: [ current :: {}, other :: {} ], request :: { city :: String, sample :: Int } }
forecastRequests { servedReports, report } = climateTable <#> \r ->
  { request: { city: r.city, sample: servedReports }, focus: if r.city == report.city then .current {} else .other {} }

isCurrent :: { focus :: [ current :: {}, other :: {} ], request :: { city :: String, sample :: Int } } -> Boolean
isCurrent { focus } = match { current: \_ -> true, other: \_ -> false } focus
