module WeatherMDC2 (weatherMDC2) where

import Prelude (Unit, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, forProperty, mvu, settled, toCase, updated)
import PUI.Web.HTML (shown, body, staticText, text)
import PUI.Web.MDC2 (body1, caption, card, elevation20, headline1, headline5, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Category as Category
import WeatherLogic (fetchReport, forecastRequests, isCurrent, presentWeather, rememberReport, warsawBulletin)

weatherMDC2 :: Effect Unit
weatherMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          ( Category.do
              listOf { selected: isCurrent } forecastRequests (text @"city" # forProperty) # toCase @"cityPicked" identity
              indeterminateCircularProgress @"busy" # action fetchReport # atCase @"cityPicked" ) # updated (match { reportServed: rememberReport })
          ( headline1 $ RecordToRecord.do
              text @"temperatureText"
              staticText " °C" ) # shown
          ( headline5 $ RecordToRecord.do
              text @"conditionText"
              staticText " in "
              text @"cityText" ) # shown
          ( body1 $ RecordToRecord.do
              staticText "Humidity "
              text @"humidityText"
              staticText "% · Wind "
              text @"windText"
              staticText " km/h" ) # shown
          ( caption $ RecordToRecord.do
              staticText "Simulated service · "
              text @"servedReportsText"
              staticText " reports served" ) # shown
          ( Category.do
              iconButton @"About this dashboard" { icon: "info" }
              simpleDialog { title: "About this dashboard", confirm: "Got it" }
                ( body1 ( RecordToRecord.do
                    staticText "A simulated weather service: canned per-city climate with slight variation per reading, served with a 800 ms delay. Reports served so far: "
                    text @"servedReportsText"
                    staticText "." )) # atCase @"About this dashboard" ) # shown
      ) # settled presentWeather # mvu warsawBulletin
