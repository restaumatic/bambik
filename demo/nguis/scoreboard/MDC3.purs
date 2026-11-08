module Scoreboard.MDC3 (scoreboard) where

import Prelude (Unit, (#), ($))

import Effect (Effect)
import PUI (accumulatedOf, actedOf)
import PUI.Web (clock, text)
import PUI.Web.MDC3 (body, bodyMedium, list, listItem)
import QualifiedDo.Semigroupoid as Semigroupoid
import Scoreboard (boardSummary, clockPeriod, goal, scoreLine, summaryLine)

scoreboard :: Effect Unit
scoreboard =
  body $ Semigroupoid.do
    clock @"time" clockPeriod
    list $ ( listItem $ text scoreLine ) # accumulatedOf @"team" goal
    ( bodyMedium $ text summaryLine ) # actedOf @"key" boardSummary
