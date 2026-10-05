module WeatherMDC2 (weatherMDC2) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (action, atCase, joined, looped, with)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body1, caption, headline1, headline5, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid
import WeatherViewModel (aboutLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, servedLine, temperatureLine, warsawBulletin)

weatherMDC2 :: Effect Unit
weatherMDC2 =
  body $
    ( Semigroupoid.do
      headline1 (text temperatureLine) # shown
      headline5 (text conditionLine) # shown
      body1 (text humidityWindLine) # shown
      caption (text servedLine) # shown
      ( Semigroupoid.do
        iconButton @"About this dashboard" {} "info"
        simpleDialog @"Got it" @"About this dashboard"
          ( body1 (text aboutLine) ) # atCase @"About this dashboard" ) # shown
      listOf @"requested" @"request" @( request :: { city :: String, sample :: Int }, focus :: [ current :: {}, other :: {} ] ) { selected: isCurrent } forecastRequests (text _.request.city) # joined @"requested"
      indeterminateCircularProgress @"Fetching forecast" # action @{ report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } , servedReports :: Int } fetchReport # atCase @"requested"
    ) # looped
      @( report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }
       , servedReports :: Int
       ) # with warsawBulletin
