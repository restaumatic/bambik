module WeatherMDC2 (weatherMDC2) where

import Prelude (Unit, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (action, informed, mvu, forProperty, atCase, projected, toCase, updated)
import PUI.Web.HTML (shownAs, body, staticText, text)
import PUI.Web.MDC2 (body1, caption, card, elevation20, headline1, headline5, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid
import WeatherLogic (cityText, conditionText, fetchReport, forecastRequests, humidityText, rememberReport, servedReportsText, temperatureText, warsawBulletin, windText)

weatherMDC2 :: Effect Unit
weatherMDC2 =
  body $
    elevation20 $
      card $ ( Semigroupoid.do
          ( Semigroupoid.do
              listOf { selected: _.shown } forecastRequests (text @"city" # forProperty identity) # toCase @"cityPicked" identity
              indeterminateCircularProgress @"busy" # action fetchReport # atCase @"cityPicked") # updated (match { reportServed: informed rememberReport })
          shownAs identity ( headline1 $ RecordToRecord.do
              text @"temperature" # projected temperatureText
              staticText " °C" )
          shownAs identity ( headline5 $ RecordToRecord.do
              text @"condition" # projected conditionText
              staticText " in "
              text @"city" # projected cityText )
          shownAs identity ( body1 $ RecordToRecord.do
              staticText "Humidity "
              text @"humidity" # projected humidityText
              staticText "% · Wind "
              text @"wind" # projected windText
              staticText " km/h" )
          shownAs identity ( caption $ RecordToRecord.do
              staticText "Simulated service · "
              text @"servedReports" # projected servedReportsText
              staticText " reports served" )
          shownAs identity ( Semigroupoid.do
              iconButton @"About this dashboard" { icon: "info" }
              simpleDialog { title: "About this dashboard", confirm: "Got it" }
                ( body1 ( RecordToRecord.do
                    staticText "A simulated weather service: canned per-city climate with slight variation per reading, served with a 800 ms delay. Reports served so far: "
                    text @"servedReports" # projected servedReportsText
                    staticText "." )) # atCase @"About this dashboard")
      ) # mvu warsawBulletin
