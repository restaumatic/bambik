module WeatherMDC2 (weatherMDC2) where

import Prelude (Unit, (#), ($))

import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, mvu, updated)
import PUI.Web.HTML (shown, text)
import PUI.Web.MDC2 (body, body1, caption, card, headline1, headline5, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Category as Category
import WeatherLogic (aboutLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, rememberReport, reportRequest, servedLine, temperatureLine, warsawBulletin)

weatherMDC2 :: Effect Unit
weatherMDC2 =
  body $
    card $ ( Category.do
      ( Category.do
        listOf @"cityPicked" reportRequest { selected: isCurrent } forecastRequests (text _.city)
        indeterminateCircularProgress @"busy" # action fetchReport # atCase @"cityPicked" ) # updated (match { reportServed: rememberReport })
      headline1 (text temperatureLine) # shown
      headline5 (text conditionLine) # shown
      body1 (text humidityWindLine) # shown
      caption (text servedLine) # shown
      ( Category.do
        iconButton @"About this dashboard" { icon: "info" }
        simpleDialog { title: "About this dashboard", confirm: "Got it" }
          ( body1 (text aboutLine) ) # atCase @"About this dashboard" ) # shown
    ) # mvu warsawBulletin
