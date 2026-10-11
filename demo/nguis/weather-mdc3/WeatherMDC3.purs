module WeatherMDC3 (weatherMDC3) where

import Prelude (identity, Unit, (#), ($))

import Effect (Effect)
import PUI (action, atCase, blankStatus, fold, joined, looped, with)
import PUI.Pipeline as Pipeline
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyLarge, bodySmall, displayLarge, headlineMedium, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import WeatherViewModel (aboutLine, cityLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, servedLine, temperatureLine, warsawBulletin)

weatherMDC3 :: Effect Unit
weatherMDC3 =
  body $ Pipeline.do
    displayLarge (text temperatureLine)
    headlineMedium (text conditionLine)
    bodyLarge (text humidityWindLine)
    bodySmall (text servedLine)
    ( Pipeline.do
      iconButton @"About this dashboard" {} "info"
      simpleDialog "Got it" "About this dashboard"
        ( bodyLarge (text aboutLine) ) # atCase @"About this dashboard" )
    listOf @"Forecast requested" @"request"
      @( request :: { city :: String, sample :: Int }
       , focus :: [ current :: {}, other :: {} ]
       ) { selected: isCurrent } forecastRequests (text cityLine) # joined @"Forecast requested"
    indeterminateCircularProgress # action fetchReport # atCase @"Forecast requested"
    blankStatus @"Forecast fetched" # fold identity
  # looped
    @( report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }
     , servedReports :: Int
     ) # with warsawBulletin
