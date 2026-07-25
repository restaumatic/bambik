module Weather (weather) where

import Prelude ((#), ($), (*), (+), (-), (<#>), (==), Unit, discard, mod, pure, show)

import Data.Array (filter, index)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import PUI (action, mvu, onCase, projection, tapped, toCase, updates)
import PUI.HTML (body, staticText, text)
import PUI.MDC (body1, caption, card, elevation20, headline1, headline5, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid

weather :: Effect Unit
weather =
  body $
    elevation20 $
      card { caption: "Weather Dashboard" } $ ( Semigroupoid.do
          ( Semigroupoid.do
              listOf { selected: _.shown } (text # projection _.city) # toCase @"cityPicked" # lcmap forecastRequests
              indeterminateCircularProgress # action fetchReport # onCase @"cityPicked") # updates (match { reportServed: rememberReport })
          headline1 ( RecordToRecord.do
              text # projection temperatureText
              staticText " °C" ) # tapped
          headline5 ( RecordToRecord.do
              text # projection conditionText
              staticText " in "
              text # projection cityText ) # tapped
          body1 ( RecordToRecord.do
              staticText "Humidity "
              text # projection humidityText
              staticText "% · Wind "
              text # projection windText
              staticText " km/h" ) # tapped
          caption ( RecordToRecord.do
              staticText "Simulated service · "
              text # projection servedReportsText
              staticText " reports served" ) # tapped
          ( Semigroupoid.do
              iconButton { icon: "info", label: "About this dashboard" }
              simpleDialog { title: "About this dashboard", confirm: "Got it" }
                ( body1 ( RecordToRecord.do
                    staticText "A simulated weather service: canned per-city climate with slight variation per reading, served with a 800 ms delay. Reports served so far: "
                    text # projection servedReportsText
                    staticText "." ) # tapped) # onCase @"clicked" # toCase @"dashboardResumed") # updates (match { dashboardResumed: resumeDashboard })
      ) # mvu warsawBulletin

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

fetchReport :: { city :: String, sample :: Int, shown :: Boolean } -> Aff [ reportServed :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } ]
fetchReport request = do
  delay (Milliseconds 800.0)
  pure (.reportServed (conditionsFor request.city request.sample))

rememberReport :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } -> { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int } -> { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int }
rememberReport report board = { report, servedReports: board.servedReports + 1 }

resumeDashboard :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int } -> { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int } -> { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int }
resumeDashboard _ board = board

forecastRequests :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int } -> Array { city :: String, sample :: Int, shown :: Boolean }
forecastRequests board = climateTable <#> \r ->
  { city: r.city, sample: board.servedReports, shown: r.city == board.report.city }

temperatureText :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } } -> String
temperatureText board = show board.report.temperature

conditionText :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } } -> String
conditionText board = board.report.condition

cityText :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } } -> String
cityText board = board.report.city

humidityText :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } } -> String
humidityText board = show board.report.humidity

windText :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } } -> String
windText board = show board.report.wind

servedReportsText :: { servedReports :: Int } -> String
servedReportsText board = show board.servedReports

warsawBulletin :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int }
warsawBulletin = { report: conditionsFor "Warsaw" 0, servedReports: 1 }
