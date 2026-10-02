module WeatherMDC3 (weatherMDC3) where

import Prelude (Unit, (#), ($))

import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, mvu, updated)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyLarge, bodySmall, displayLarge, headlineMedium, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid
import WeatherViewModel (aboutLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, rememberReport, servedLine, temperatureLine, warsawBulletin)

weatherMDC3 :: Effect Unit
weatherMDC3 =
  body $
    ( Semigroupoid.do
      ( Semigroupoid.do
        listOf @"requested" @"request" @( request :: { city :: String, sample :: Int }, focus :: [ current :: {}, other :: {} ] ) { selected: isCurrent } forecastRequests (text _.request.city)
        indeterminateCircularProgress @"Fetching forecast" # action @[ reportServed :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } } ] fetchReport # atCase @"requested" ) # updated (match { reportServed: rememberReport })
      displayLarge (text temperatureLine) # shown
      headlineMedium (text conditionLine) # shown
      bodyLarge (text humidityWindLine) # shown
      bodySmall (text servedLine) # shown
      ( Semigroupoid.do
        iconButton @"About this dashboard" {} "info"
        simpleDialog @"Got it" @"About this dashboard"
          ( bodyLarge (text aboutLine) ) # atCase @"About this dashboard" ) # shown
    ) # mvu
      @( report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }
       , servedReports :: Int
       )
      warsawBulletin
