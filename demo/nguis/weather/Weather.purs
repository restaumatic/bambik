module Weather (weather) where

import Prelude ((#), ($), (*), (+), (-), (<#>), (<>), (==), Unit, const, discard, identity, mod, pure, show)

import Data.Array (filter, index)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Profunctor (lcmap)
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import PUI (action, forValue, mvu, onCase, projection, tapped, toCase, updates)
import PUI.HTML (body, text)
import PUI.MDC (body1, caption, card, elevation20, headline1, headline5, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid

type WeatherBoard =
  { report :: Report
  , servedReports :: Int
  }

weather :: Effect Unit
weather =
  body $
    elevation20 $
      card { caption: "Weather Dashboard" } $ ( Semigroupoid.do
          ( Semigroupoid.do
              ( listOf { selected: _.shown } (text # projection _.city # forValue)
              ) # toCase @"cityPicked" # lcmap forecastRequests
              indeterminateCircularProgress # action fetchReport # onCase @"cityPicked"
          ) # updates (match { reportServed: rememberReport })
          headline1 (text # projection temperatureLine # forValue) # tapped
          headline5 (text # projection conditionLine # forValue) # tapped
          body1 (text # projection detailsLine # forValue) # tapped
          caption (text # projection serviceLine # forValue) # tapped
          ( Semigroupoid.do
              iconButton { icon: "info", label: "About this dashboard" }
              simpleDialog { title: "About this dashboard", confirm: "Got it" }
                ( body1 (text # projection serviceStory # forValue) # tapped
                ) # onCase @"clicked" # toCase @"dashboardResumed"
          ) # updates (match { dashboardResumed: const identity })
      ) # mvu warsawBulletin

type Report =
  { city :: String
  , temperature :: Number
  , condition :: String
  , humidity :: Int
  , wind :: Number
  }

type ForecastRequest =
  { city :: String
  , sample :: Int
  , shown :: Boolean
  }

climateTable :: Array Report
climateTable =
  [ { city: "Warsaw", temperature: 21.0, condition: "Partly cloudy", humidity: 55, wind: 12.0 }
  , { city: "Lisbon", temperature: 27.0, condition: "Sunny", humidity: 48, wind: 18.0 }
  , { city: "Reykjavik", temperature: 11.0, condition: "Drizzle", humidity: 82, wind: 26.0 }
  , { city: "Cairo", temperature: 36.0, condition: "Clear sky", humidity: 22, wind: 9.0 }
  , { city: "Singapore", temperature: 31.0, condition: "Thunderstorm", humidity: 88, wind: 7.0 }
  , { city: "Sydney", temperature: 17.0, condition: "Showers", humidity: 64, wind: 21.0 }
  ]

conditionsFor :: String -> Int -> Report
conditionsFor city sample =
  let base = firstWithCity city
  in base
    { temperature = base.temperature + toNumber (sample * 3 `mod` 5) - 2.0
    , humidity = base.humidity + (sample * 7 `mod` 9) - 4
    , wind = base.wind + toNumber (sample * 5 `mod` 7) - 3.0
    }

firstWithCity :: String -> Report
firstWithCity city = fromMaybe unknownTerritory (index (filter (\r -> r.city == city) climateTable) 0)

unknownTerritory :: Report
unknownTerritory = { city: "Unknown", temperature: 0.0, condition: "No data", humidity: 0, wind: 0.0 }

fetchReport :: ForecastRequest -> Aff [ reportServed :: Report ]
fetchReport request = do
  delay (Milliseconds 800.0)
  pure (.reportServed (conditionsFor request.city request.sample))

rememberReport :: Report -> WeatherBoard -> WeatherBoard
rememberReport report board = { report, servedReports: board.servedReports + 1 }

forecastRequests :: WeatherBoard -> Array ForecastRequest
forecastRequests board = climateTable <#> \r ->
  { city: r.city, sample: board.servedReports, shown: r.city == board.report.city }

temperatureLine :: WeatherBoard -> String
temperatureLine board = show board.report.temperature <> " °C"

conditionLine :: WeatherBoard -> String
conditionLine board = board.report.condition <> " in " <> board.report.city

detailsLine :: WeatherBoard -> String
detailsLine board = "Humidity " <> show board.report.humidity <> "% · Wind " <> show board.report.wind <> " km/h"

serviceLine :: WeatherBoard -> String
serviceLine board = "Simulated service · " <> show board.servedReports <> " reports served"

serviceStory :: WeatherBoard -> String
serviceStory board = "A simulated weather service: canned per-city climate with slight variation per reading, served with a 800 ms delay. Reports served so far: " <> show board.servedReports <> "."

warsawBulletin :: WeatherBoard
warsawBulletin = { report: conditionsFor "Warsaw" 0, servedReports: 1 }
