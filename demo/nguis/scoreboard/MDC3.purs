module Scoreboard.MDC3 (scoreboard) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (accumulated, foreach)
import PUI.Web (clock, text)
import PUI.Web.MDC3 (body, bodyMedium, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import Scoreboard (boardSummary, clockPeriod, goal, scoreLine, summaryLine)

scoreboard :: Effect Unit
scoreboard =
  body $ Semigroupoid.do
    clock @"time" clockPeriod
    list $ ( listItem $ text scoreLine ) # accumulated goal
    ( bodyMedium $ text summaryLine ) # foreach @"key" boardSummary
