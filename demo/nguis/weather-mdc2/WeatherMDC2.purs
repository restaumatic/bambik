module WeatherMDC2 (weatherMDC2) where

import Prelude (Unit, identity, (#), ($))

import Data.Variant (match)
import Effect (Effect)
import PUI (action, atCase, forProperty, mvu, settled, toCase, updated)
import PUI.Web.HTML (shown, body, text)
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
          headline1 (text @"temperatureLine") # shown
          headline5 (text @"conditionLine") # shown
          body1 (text @"humidityWindLine") # shown
          caption (text @"servedLine") # shown
          ( Category.do
              iconButton @"About this dashboard" { icon: "info" }
              simpleDialog { title: "About this dashboard", confirm: "Got it" }
                ( body1 (text @"aboutLine") ) # atCase @"About this dashboard" ) # shown
      ) # settled presentWeather # mvu warsawBulletin
