module WeatherMDC3 (weatherMDC3) where

import Prelude (Unit, identity, show, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, forProperty, mvu, projection, toCase, updated)
import PUI.Web.HTML (shown, body, staticText, text)
import PUI.Web.MDC3 (bodyLarge, bodySmall, card, elevation5, displayLarge, headlineMedium, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Category as Category
import WeatherLogic (cityText, conditionText, fetchReport, forecastRequests, humidityText, isCurrent, rememberReport, temperatureText, warsawBulletin, windText)

weatherMDC3 :: Effect Unit
weatherMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( Category.do
              listOf { selected: isCurrent } forecastRequests (text @"city" # forProperty) # toCase @"cityPicked" identity
              indeterminateCircularProgress @"busy" # action fetchReport # atCase @"cityPicked" ) # updated (match { reportServed: rememberReport })
          ( displayLarge $ RecordToRecord.do
              text @"report" # projection temperatureText
              staticText " °C" ) # shown
          ( headlineMedium $ RecordToRecord.do
              text @"report" # projection conditionText
              staticText " in "
              text @"report" # projection cityText ) # shown
          ( bodyLarge $ RecordToRecord.do
              staticText "Humidity "
              text @"report" # projection humidityText
              staticText "% · Wind "
              text @"report" # projection windText
              staticText " km/h" ) # shown
          ( bodySmall $ RecordToRecord.do
              staticText "Simulated service · "
              text @"servedReports" # projection show
              staticText " reports served" ) # shown
          ( Category.do
              iconButton @"About this dashboard" { icon: "info" }
              simpleDialog { title: "About this dashboard", confirm: "Got it" }
                ( bodyLarge ( RecordToRecord.do
                    staticText "A simulated weather service: canned per-city climate with slight variation per reading, served with a 800 ms delay. Reports served so far: "
                    text @"servedReports" # projection show
                    staticText "." )) # atCase @"About this dashboard" ) # shown
      ) # mvu warsawBulletin
