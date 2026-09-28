module WeatherMDC3 (weatherMDC3) where

import Prelude (Unit, (#), ($))

import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, mvu, updated)
import PUI.Web (shown, text)
import PUI.Web.MDC3 (body, bodyLarge, bodySmall, displayLarge, headlineMedium, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid
import WeatherLogic (aboutLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, rememberReport, servedLine, temperatureLine, warsawBulletin)

weatherMDC3 :: Effect Unit
weatherMDC3 =
  body $
    ( Semigroupoid.do
      ( Semigroupoid.do
        listOf @"cityPicked" @"request" { selected: isCurrent } forecastRequests (text _.request.city)
        indeterminateCircularProgress @"busy" # action fetchReport # atCase @"cityPicked" ) # updated (match { reportServed: rememberReport })
      displayLarge (text temperatureLine) # shown
      headlineMedium (text conditionLine) # shown
      bodyLarge (text humidityWindLine) # shown
      bodySmall (text servedLine) # shown
      ( Semigroupoid.do
        iconButton @"About this dashboard" {} "info"
        simpleDialog @"Got it" "About this dashboard"
          ( bodyLarge (text aboutLine) ) # atCase @"About this dashboard" ) # shown
    ) # mvu warsawBulletin
