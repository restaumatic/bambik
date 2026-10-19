module Weather.MDC3 (weather) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Effect (Effect)
import PUI (action, capture, joined, looped, with)
import PUI.Web (text)
import PUI.Web.MDC3 (acknowledged, body, bodyLarge, bodySmall, displayLarge, headlineMedium, iconButton, indeterminateCircularProgress, listOf')
import QualifiedDo.Semigroupoid as Semigroupoid
import Weather (aboutLine, cityLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, servedLine, temperatureLine, warsawBulletin)

weather :: Effect Unit
weather =
  body $ Semigroupoid.do
    displayLarge $ text temperatureLine
    headlineMedium $ text conditionLine
    bodyLarge $ text humidityWindLine
    bodySmall $ text servedLine
    RecordToVariant.do
      iconButton @"About this dashboard" "info"
      ( listOf' @"Forecast requested" @"request" { selected: isCurrent } forecastRequests $ text cityLine ) # joined @"Forecast requested"
    acknowledged @"About this dashboard" $ bodyLarge $ text aboutLine
    indeterminateCircularProgress # action @"Forecast requested" fetchReport
    capture @"Forecast fetched"
  # looped # with warsawBulletin
