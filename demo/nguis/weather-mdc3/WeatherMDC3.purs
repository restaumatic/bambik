module WeatherMDC3 (weatherMDC3) where

import Prelude (identity, Unit, (#), ($))

import Effect (Effect)
import PUI (action, atCase, blankStatus, fold, joined, looped, with)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyLarge, bodySmall, displayLarge, headlineMedium, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid
import WeatherViewModel (aboutLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, servedLine, temperatureLine, warsawBulletin)

weatherMDC3 :: Effect Unit
weatherMDC3 =
  body $ Semigroupoid.do
    displayLarge (text temperatureLine) # shown
    headlineMedium (text conditionLine) # shown
    bodyLarge (text humidityWindLine) # shown
    bodySmall (text servedLine) # shown
    ( Semigroupoid.do
      iconButton @"About this dashboard" {} "info"
      simpleDialog "Got it" "About this dashboard"
        ( bodyLarge (text aboutLine) ) # atCase @"About this dashboard" ) # shown
    listOf @"Forecast requested" @"request"
      @( request :: { city :: String, sample :: Int }
       , focus :: [ current :: {}, other :: {} ]
       ) { selected: isCurrent } forecastRequests (text _.request.city) # joined @"Forecast requested"
    indeterminateCircularProgress # action fetchReport # atCase @"Forecast requested"
    blankStatus @"Forecast fetched" # fold identity
  # looped
    @( report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }
     , servedReports :: Int
     ) # with warsawBulletin
