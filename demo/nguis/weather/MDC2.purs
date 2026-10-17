module Weather.MDC2 (weather) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Effect (Effect)
import PUI (action, capture, joined, looped, with)
import PUI.Web (text)
import PUI.Web.MDC2 (body, body1, caption, headline1, headline5, iconButton, indeterminateCircularProgress, listOf', acknowledged)
import QualifiedDo.Semigroupoid as Semigroupoid
import Weather (aboutLine, cityLine, conditionLine, fetchReport, forecastRequests, humidityWindLine, isCurrent, servedLine, temperatureLine, warsawBulletin)

weather :: Effect Unit
weather =
  body $ Semigroupoid.do
    headline1 $ text temperatureLine
    headline5 $ text conditionLine
    body1 $ text humidityWindLine
    caption $ text servedLine
    RecordToVariant.do
      iconButton @"About this dashboard" "info"
      ( listOf' @"Forecast requested" @"request" { selected: isCurrent } forecastRequests $ text cityLine ) # joined @"Forecast requested"
    acknowledged @"About this dashboard" $ body1 $ text aboutLine
    indeterminateCircularProgress # action @"Forecast requested" fetchReport
    capture @"Forecast fetched"
  # looped # with warsawBulletin
