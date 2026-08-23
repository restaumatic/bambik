module WeatherMDC3 (weatherMDC3) where

import Prelude (Unit, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (action, informed, mvu, forProperty, atCase, projected, toCase, updated)
import PUI.Web.HTML (shownAs, body, staticText, text)
import PUI.Web.MDC3 (bodyLarge, bodySmall, card, elevation5, displayLarge, headlineMedium, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid
import WeatherLogic (cityText, conditionText, fetchReport, forecastRequests, humidityText, rememberReport, servedReportsText, temperatureText, warsawBulletin, windText)

weatherMDC3 :: Effect Unit
weatherMDC3 =
  body $
    elevation5 $
      card $ ( Semigroupoid.do
          ( Semigroupoid.do
              listOf { selected: _.shown } forecastRequests (text @"city" # forProperty identity) # toCase @"cityPicked" identity
              indeterminateCircularProgress @"busy" # action fetchReport # atCase @"cityPicked") # updated (match { reportServed: informed rememberReport })
          shownAs identity ( displayLarge $ RecordToRecord.do
              text @"temperature" # projected temperatureText
              staticText " °C" )
          shownAs identity ( headlineMedium $ RecordToRecord.do
              text @"condition" # projected conditionText
              staticText " in "
              text @"city" # projected cityText )
          shownAs identity ( bodyLarge $ RecordToRecord.do
              staticText "Humidity "
              text @"humidity" # projected humidityText
              staticText "% · Wind "
              text @"wind" # projected windText
              staticText " km/h" )
          shownAs identity ( bodySmall $ RecordToRecord.do
              staticText "Simulated service · "
              text @"servedReports" # projected servedReportsText
              staticText " reports served" )
          shownAs identity ( Semigroupoid.do
              iconButton @"About this dashboard" { icon: "info" }
              simpleDialog { title: "About this dashboard", confirm: "Got it" }
                ( bodyLarge ( RecordToRecord.do
                    staticText "A simulated weather service: canned per-city climate with slight variation per reading, served with a 800 ms delay. Reports served so far: "
                    text @"servedReports" # projected servedReportsText
                    staticText "." )) # atCase @"About this dashboard")
      ) # mvu warsawBulletin
