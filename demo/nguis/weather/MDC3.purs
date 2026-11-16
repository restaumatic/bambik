module Weather.MDC3 (weather) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Effect (Effect)
import PUI (acknowledged, action, continued, foldInto, looped, stepped, with)
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyLarge, bodySmall, displayLarge, headlineMedium, iconButton, indeterminateCircularProgress, listOf', modal)
import QualifiedDo.Semigroupoid as Semigroupoid
import Weather (aboutLine, asForecast, cityLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, rememberReport, servedLine, temperatureLine, warningLine, warsawBulletin)

weather :: Effect Unit
weather =
  body $ Semigroupoid.do
    displayLarge $ text temperatureLine
    headlineMedium $ text conditionLine
    bodyLarge $ text humidityWindLine
    bodySmall $ text servedLine
    ( Semigroupoid.do
      RecordToVariant.do
        iconButton @"About this dashboard" "info"
        listOf' @"Forecast requested" @"request" { selected: isCurrent } forecastRequests $ text cityLine
      ( modal @"About this dashboard" $ bodyLarge $ text aboutLine ) # acknowledged
      indeterminateCircularProgress # action @"Forecast requested" fetchReport
      ( modal @"Weather warning" $ bodyLarge $ text warningLine ) # continued asForecast
      foldInto @"Forecast fetched" rememberReport ) # stepped
  # looped # with warsawBulletin
