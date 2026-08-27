module WeatherLogic (cityText, conditionText, fetchReport, forecastRequests, humidityText, isCurrent, rememberReport, temperatureText, warsawBulletin, windText) where

import Prelude (discard, mod, pure, show, (*), (+), (-), (<#>), (==))

import Data.Array (filter, index)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Data.Variant (match)

warsawBulletin :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int }
warsawBulletin = { report: conditionsFor "Warsaw" 0, servedReports: 1 }

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

fetchReport :: { city :: String, sample :: Int, focus :: [ current :: {}, other :: {} ] } -> Aff [ reportServed :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } } ]
fetchReport { city, sample } = do
  delay (Milliseconds 800.0)
  pure (.reportServed { report: conditionsFor city sample })

rememberReport :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } } -> { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int } -> { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int }
rememberReport { report } { servedReports } = { report, servedReports: servedReports + 1 }

forecastRequests :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int } -> Array { city :: String, sample :: Int, focus :: [ current :: {}, other :: {} ] }
forecastRequests { servedReports, report } = climateTable <#> \r ->
  { city: r.city, sample: servedReports, focus: if r.city == report.city then .current {} else .other {} }

isCurrent :: { city :: String, sample :: Int, focus :: [ current :: {}, other :: {} ] } -> Boolean
isCurrent { focus } = match { current: \_ -> true, other: \_ -> false } focus

temperatureText :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } -> String
temperatureText report = show report.temperature

conditionText :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } -> String
conditionText report = report.condition

cityText :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } -> String
cityText report = report.city

humidityText :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } -> String
humidityText report = show report.humidity

windText :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } -> String
windText report = show report.wind
