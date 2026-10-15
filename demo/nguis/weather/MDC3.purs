module Weather.MDC3 (weather) where

import Prelude (Unit, (#), ($))

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (action, atCase, capture, joined, looped, with)
import PUI.Web (text)
import PUI.Web.MDC3 (body, bodyLarge, bodySmall, displayLarge, headlineMedium, iconButton, indeterminateCircularProgress, listOf', simpleDialog)
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
      ( listOf' @"Forecast requested" @"request"
        @( request :: { city :: String, sample :: Int }
         , focus :: [ current :: {}, other :: {} ]
         ) { selected: isCurrent } forecastRequests $ text cityLine ) # joined @"Forecast requested"
    VariantToRecord.do
      ( simpleDialog "Got it" "About this dashboard" $ bodyLarge $ text aboutLine ) # atCase @"About this dashboard"
      ( Semigroupoid.do
        indeterminateCircularProgress # action fetchReport
        capture @"Forecast fetched" ) # atCase @"Forecast requested"
  # looped
    @( report :: { city :: String, temperature :: Number, condition :: String, humidity :: Int, wind :: Number }
     , servedReports :: Int
     ) # with warsawBulletin
