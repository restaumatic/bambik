module WeatherMDC2 (weatherMDC2) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (action, atCase, captured, joined, looped, prefixed, unshown, with)
import PUI.Web (text)
import PUI.Web.MDC2 (body, body1, caption, headline1, headline5, iconButton, indeterminateCircularProgress, listOf', simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid
import WeatherViewModel (aboutLine, cityLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, servedLine, temperatureLine, warsawBulletin)

weatherMDC2 :: Effect Unit
weatherMDC2 =
  body $ Semigroupoid.do
    prefixed ( RecordToRecord.do
      headline1 (text temperatureLine)
      headline5 (text conditionLine)
      body1 (text humidityWindLine)
      caption (text servedLine)
      ( Semigroupoid.do
        iconButton @"About this dashboard" "info"
        simpleDialog "Got it" "About this dashboard"
          ( body1 (text aboutLine) ) # atCase @"About this dashboard" ) ) $ listOf' @"Forecast requested" @"request"
      @( request :: { city :: String, sample :: Int }
       , focus :: [ current :: {}, other :: {} ]
       ) { selected: isCurrent } forecastRequests (text cityLine) # joined @"Forecast requested"
    indeterminateCircularProgress # action fetchReport # atCase @"Forecast requested"
    unshown @"Forecast fetched" # captured
  # looped
    @( report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }
     , servedReports :: Int
     ) # with warsawBulletin
