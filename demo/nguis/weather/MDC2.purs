module Weather.MDC2 (weather) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Effect (Effect)
import PUI (acknowledged, action, continued, foldInto, looped, stepped, with)
import PUI.Web (text)
import PUI.Web.MDC2 (body, body1, caption, headline1, headline5, iconButton, indeterminateCircularProgress, listOf', modal)
import QualifiedDo.Semigroupoid as Semigroupoid
import Weather (aboutLine, asForecast, cityLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, rememberReport, servedLine, temperatureLine, warningLine, warsawBulletin)

weather :: Effect Unit
weather =
  body $ Semigroupoid.do
    headline1 $ text temperatureLine
    headline5 $ text conditionLine
    body1 $ text humidityWindLine
    caption $ text servedLine
    ( Semigroupoid.do
      RecordToVariant.do
        iconButton @"About this dashboard" "info"
        listOf' @"Forecast requested" @"request" { selected: isCurrent } forecastRequests $ text cityLine
      ( modal @"About this dashboard" $ body1 $ text aboutLine ) # acknowledged
      indeterminateCircularProgress # action @"Forecast requested" fetchReport
      ( modal @"Weather warning" $ body1 $ text warningLine ) # continued asForecast
      foldInto @"Forecast fetched" rememberReport ) # stepped
  # looped # with warsawBulletin
