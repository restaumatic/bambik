module WeatherMDC2 (weatherMDC2) where

import Prelude (identity, (#), ($), (*), (+), (-), (<#>), (<<<), (==), Unit, const, discard, mod, pure, show)

import Data.Array (filter, index)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import PUI (action, mvu, forProperty, onCase, projected, tapped, toCase, updated)
import PUI.HTML (body, staticText, text)
import PUI.MDC2 (body1, caption, card, elevation20, headline1, headline5, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid

weatherMDC2 :: Effect Unit
weatherMDC2 =
  body $
    elevation20 $
      card { caption: "Weather Dashboard" } $ ( Semigroupoid.do
          ( Semigroupoid.do
              listOf { selected: _.shown } forecastRequests (text # forProperty @"city") # toCase @"cityPicked" identity
              indeterminateCircularProgress # action fetchReport # onCase @"cityPicked") # updated (match { reportServed: rememberReport })
          headline1 ( RecordToRecord.do
              text # projected temperatureText
              staticText " °C" ) # tapped
          headline5 ( RecordToRecord.do
              text # projected conditionText
              staticText " in "
              text # projected cityText ) # tapped
          body1 ( RecordToRecord.do
              staticText "Humidity "
              text # projected humidityText
              staticText "% · Wind "
              text # projected windText
              staticText " km/h" ) # tapped
          caption ( RecordToRecord.do
              staticText "Simulated service · "
              text # projected servedReportsText
              staticText " reports served" ) # tapped
          ( Semigroupoid.do
              iconButton { icon: "info", label: "About this dashboard" }
              simpleDialog { title: "About this dashboard", confirm: "Got it" }
                ( body1 ( RecordToRecord.do
                    staticText "A simulated weather service: canned per-city climate with slight variation per reading, served with a 800 ms delay. Reports served so far: "
                    text # projected servedReportsText
                    staticText "." ) # tapped) # onCase @"clicked" # toCase @"dashboardResumed" identity) # updated (match { dashboardResumed: const <<< resumeDashboard })
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
fetchReport { city, sample } = do
  delay (Milliseconds 800.0)
  pure (.reportServed (conditionsFor city sample))

rememberReport :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } -> { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int } -> { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int }
rememberReport report { servedReports } = { report, servedReports: servedReports + 1 }

resumeDashboard :: { servedReports :: Int } -> { servedReports :: Int }
resumeDashboard woken = woken

forecastRequests :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int } -> Array { city :: String, sample :: Int, shown :: Boolean }
forecastRequests { servedReports, report } = climateTable <#> \r ->
  { city: r.city, sample: servedReports, shown: r.city == report.city }

temperatureText :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } } -> String
temperatureText { report } = show report.temperature

conditionText :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } } -> String
conditionText { report } = report.condition

cityText :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } } -> String
cityText { report } = report.city

humidityText :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } } -> String
humidityText { report } = show report.humidity

windText :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } } -> String
windText { report } = show report.wind

servedReportsText :: { servedReports :: Int } -> String
servedReportsText { servedReports } = show servedReports

warsawBulletin :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }, servedReports :: Int }
warsawBulletin = { report: conditionsFor "Warsaw" 0, servedReports: 1 }
