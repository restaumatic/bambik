module WeatherMDC2 (weatherMDC2) where

import Prelude (Unit, (#), ($))

import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, mvu, state, updated)
import PUI.Web (shown, text)
import PUI.Web.MDC2 (body, body1, caption, headline1, headline5, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid
import WeatherViewModel (aboutLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, rememberReport, servedLine, temperatureLine, warsawBulletin)

weatherMDC2 :: Effect Unit
weatherMDC2 =
  body $
    ( Semigroupoid.do
      state @"report" @{ city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }
      state @"servedReports" @Int
      ( Semigroupoid.do
        listOf @"requested" @"request" @{ city :: String, sample :: Int } { selected: isCurrent } forecastRequests ( Semigroupoid.do
          state @"focus" @[ current :: {}, other :: {} ]
          text _.request.city )
        indeterminateCircularProgress @"Fetching forecast" # action @[ reportServed :: { report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number } } ] fetchReport # atCase @"requested" ) # updated (match { reportServed: rememberReport })
      headline1 (text temperatureLine) # shown
      headline5 (text conditionLine) # shown
      body1 (text humidityWindLine) # shown
      caption (text servedLine) # shown
      ( Semigroupoid.do
        iconButton @"About this dashboard" {} "info"
        simpleDialog @"Got it" @"About this dashboard"
          ( body1 (text aboutLine) ) # atCase @"About this dashboard" ) # shown
    ) # mvu warsawBulletin
