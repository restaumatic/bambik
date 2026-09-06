module WeatherMDC3 (weatherMDC3) where

import Prelude (Unit, (#), ($))

import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, mvu, toCase, updated)
import PUI.Web.HTML (shown, text)
import PUI.Web.MDC3 (body, bodyLarge, bodySmall, card, displayLarge, headlineMedium, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Category as Category
import WeatherLogic (aboutLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, rememberReport, reportRequest, servedLine, temperatureLine, warsawBulletin)

weatherMDC3 :: Effect Unit
weatherMDC3 =
  body $
    card $ ( Category.do
      ( Category.do
        listOf { selected: isCurrent } forecastRequests (text _.city) # toCase @"cityPicked" reportRequest
        indeterminateCircularProgress @"busy" # action fetchReport # atCase @"cityPicked" ) # updated (match { reportServed: rememberReport })
      displayLarge (text temperatureLine) # shown
      headlineMedium (text conditionLine) # shown
      bodyLarge (text humidityWindLine) # shown
      bodySmall (text servedLine) # shown
      ( Category.do
        iconButton @"About this dashboard" { icon: "info" }
        simpleDialog { title: "About this dashboard", confirm: "Got it" }
          ( bodyLarge (text aboutLine) ) # atCase @"About this dashboard" ) # shown
    ) # mvu warsawBulletin
