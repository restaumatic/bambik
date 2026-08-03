module WeatherMDC3 (weatherMDC3) where

import Prelude (Unit, identity, (#), ($))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (action, displayed, informed, mvu, forProperty, onCase, projected, tapped, toCase, updated)
import PUI.Web.HTML (body, staticText, text)
import PUI.Web.MDC3 (bodyLarge, bodySmall, card, elevation5, displayLarge, headlineMedium, iconButton, indeterminateCircularProgress, listOf, simpleDialog)
import QualifiedDo.Semigroupoid as Semigroupoid
import WeatherLogic (cityText, conditionText, fetchReport, forecastRequests, humidityText, rememberReport, servedReportsText, temperatureText, warsawBulletin, windText)

weatherMDC3 :: Effect Unit
weatherMDC3 =
  body $
    elevation5 $
      card { caption: "Weather Dashboard" } $ ( Semigroupoid.do
          ( Semigroupoid.do
              listOf { selected: _.shown } forecastRequests (text # forProperty @"city" identity) # toCase @"cityPicked" identity
              indeterminateCircularProgress # action fetchReport # onCase @"cityPicked") # updated (match { reportServed: informed rememberReport })
          displayLarge ( RecordToRecord.do
              text # projected temperatureText
              staticText " °C" ) # tapped
          headlineMedium ( RecordToRecord.do
              text # projected conditionText
              staticText " in "
              text # projected cityText ) # tapped
          bodyLarge ( RecordToRecord.do
              staticText "Humidity "
              text # projected humidityText
              staticText "% · Wind "
              text # projected windText
              staticText " km/h" ) # tapped
          bodySmall ( RecordToRecord.do
              staticText "Simulated service · "
              text # projected servedReportsText
              staticText " reports served" ) # tapped
          ( Semigroupoid.do
              iconButton { icon: "info", label: "About this dashboard" }
              simpleDialog { title: "About this dashboard", confirm: "Got it" }
                ( bodyLarge ( RecordToRecord.do
                    staticText "A simulated weather service: canned per-city climate with slight variation per reading, served with a 800 ms delay. Reports served so far: "
                    text # projected servedReportsText
                    staticText "." )) # onCase @"clicked") # displayed
      ) # mvu warsawBulletin
