module WeatherMDC3 (weatherMDC3) where

import Prelude (Unit, identity, (#), ($))

import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, mvu, toCase, updated)
import PUI.Web.HTML (shown, body, text)
import PUI.Web.MDC3 (bodyLarge, bodySmall, card, elevation5, displayLarge, headlineMedium, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Category as Category
import WeatherLogic (aboutLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, rememberReport, servedLine, temperatureLine, warsawBulletin)

weatherMDC3 :: Effect Unit
weatherMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( Category.do
              listOf { selected: isCurrent } forecastRequests (text _.city) # toCase @"cityPicked" identity
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
