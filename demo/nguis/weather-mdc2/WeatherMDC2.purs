module WeatherMDC2 (weatherMDC2) where

import Prelude (Unit, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (action, displayed, informed, mvu, forProperty, atCase, projected, tapped, toCase, updated)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC2 (body1, caption, card, elevation20, headline1, headline5, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid
import WeatherLogic (cityText, conditionText, fetchReport, forecastRequests, humidityText, rememberReport, servedReportsText, temperatureText, warsawBulletin, windText)

weatherMDC2 :: Effect Unit
weatherMDC2 =
  body $
    elevation20 $
      card { caption: "Weather Dashboard" } $ ( Semigroupoid.do
          ( Semigroupoid.do
              listOf { selected: _.shown } forecastRequests (text @"value" # forProperty @"value" @"city" identity) # toCase @"cityPicked" identity
              indeterminateCircularProgress @"busy" # action fetchReport # atCase @"cityPicked") # updated (match { reportServed: informed rememberReport })
          headline1 ( RecordToRecord.do
              text @"value" # projected @"value" temperatureText
              staticText " °C" ) # tapped
          headline5 ( RecordToRecord.do
              text @"value" # projected @"value" conditionText
              staticText " in "
              text @"value" # projected @"value" cityText ) # tapped
          body1 ( RecordToRecord.do
              staticText "Humidity "
              text @"value" # projected @"value" humidityText
              staticText "% · Wind "
              text @"value" # projected @"value" windText
              staticText " km/h" ) # tapped
          caption ( RecordToRecord.do
              staticText "Simulated service · "
              text @"value" # projected @"value" servedReportsText
              staticText " reports served" ) # tapped
          ( Semigroupoid.do
              iconButton { icon: "info", label: "About this dashboard" }
              simpleDialog { title: "About this dashboard", confirm: "Got it" }
                ( body1 ( RecordToRecord.do
                    staticText "A simulated weather service: canned per-city climate with slight variation per reading, served with a 800 ms delay. Reports served so far: "
                    text @"value" # projected @"value" servedReportsText
                    staticText "." )) # atCase @"clicked") # displayed
      ) # mvu warsawBulletin
